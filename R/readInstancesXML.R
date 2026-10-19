#' Extract data from ODK Collect instance XMLs (without ODK Briefcase)
#'
#' Reads all submission XMLs stored under an ODK Collect `instances/`
#' directory and returns them as a named list of data frames (one per
#' repeat level). It does NOT use ODK Briefcase.
#'
#' The repeat structure is taken from the **XForm** (the form XML), not
#' from the submission XMLs. This is essential because ODK Collect strips
#' the `jr:template` markers (and the page-body `<repeat>` elements) from
#' the submitted instances, so the repeat hierarchy cannot be recovered
#' from the data alone. The form is read from `formPath`. When `formPath`
#' is not given, the function tries to locate it automatically: it
#' reads the form id from the submission root (`<data id="...">`) and
#' searches for `<formId>.xml` in the usual locations.
#'
#' Each row carries a `KEY` (unique id of the observation) and, for
#' nested repeats, a `PARENT_KEY` (the `KEY` of the observation one level
#' up), so the hierarchy can be reconstructed by joining the files. The
#' `instanceID` column is present on every row and allows grouping by
#' submission.
#'
#' The first table (stored internally under the sentinel name `".root"`,
#' written as `<formBase>.csv`) holds the root record: every leaf field
#' of the `<data>` element that is not inside a repeat (i.e. everything
#' before `<subquad>` plus any other non-repeated siblings and
#' `<meta>/instanceID`).
#'
#' @param instancesDir Path to the ODK Collect `instances` directory.
#' @param expDir Optional. If provided, each data frame is also written
#'   as a CSV in this directory. Naming follows ODK Briefcase:
#'   `<formBase>.csv` for the main form and `<formBase>-<repeat>.csv`
#'   for each repeat (nested repeats use `-` as separator, e.g.
#'   `<formBase>-subquad-tree-sec_stem.csv`).
#' @param formBase Base name for the output CSVs. If NULL, it is derived
#'   from the first instance XML found (stripping any
#'   `_YYYY-MM-DD_HH-MM-SS` submission suffix). Only affects output file
#'   names, not form discovery (that uses the form id).
#' @param formPath Optional. Path to the XForm XML of the form (e.g.
#'   `inst/odk/treeCensusForm.xml`). When NULL, the function tries to
#'   locate it automatically.
#' @param overwrite Logical. If FALSE (default), existing CSVs are not
#'   overwritten.
#' @param mediaOutDir Optional. If provided, all non-XML files found in
#'   the instance folders (photos, etc.) are copied here, preserving
#'   the subfolder structure.
#'
#' @return Invisibly, a named list of data frames. Names are `".root"`
#'   (main form) plus each repeat path (e.g. `"subquad"`,
#'   `"subquad/tree"`, ...). Columns use dot-separated paths for nested
#'   groups, as in Briefcase CSV exports.
#' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
#' @seealso
#' \url{http://labtrop.ib.usp.br}
#' @references \url{https://opendatakit.org/}
#' @examples
#' \dontrun{
#' dfs <- readInstancesXML("inst/testDataODK/instances",
#'                         expDir = "inst/exportDataODK/testDataODK",
#'                         formBase = "treeCensusForm",
#'                         formPath = "inst/odk/treeCensusForm.xml")
#' }
#' @export
readInstancesXML <- function(instancesDir,
                             expDir = NULL,
                             formBase = NULL,
                             formPath = NULL,
                             overwrite = FALSE,
                             mediaOutDir = NULL) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' is required.")
  }
  if (!dir.exists(instancesDir)) {
    stop("Directory not found: ", instancesDir)
  }

  ## Internal sentinel key for the root record. Kept non-empty on
  ## purpose: indexing a named list by the empty string (dfs[[""]])
  ## returns NULL in R, and paste0() then silently drops the value,
  ## e.g. message("Wrote ", nrow(NULL), " row(s)") -> "Wrote  row(s)".
  ## We use ".root" throughout and translate it to "" only when
  ## composing the output file name.
  ROOT_KEY <- ".root"

  ## 1. Find instance XMLs
  xmls <- list.files(instancesDir, pattern = "\\.xml$",
                     recursive = TRUE, full.names = TRUE)
  if (length(xmls) == 0) {
    stop("No instance XML found in: ", instancesDir)
  }

  if (is.null(formBase)) {
    formBase <- .deriveFormBase(basename(xmls[1]))
  }

  ## 2. Read the first submission and get the form id from its root
  doc0 <- xml2::read_xml(xmls[1])
  dataRoot <- xml2::xml_find_first(doc0, "/*[local-name()='data']")
  formId <- if (!is.na(dataRoot)) xml2::xml_attr(dataRoot, "id") else NA_character_
  formId <- trimws(formId)
  if (is.na(formId) || !nzchar(formId)) formId <- NULL

  ## 3. Locate the XForm (source of the repeat structure)
  if (is.null(formPath) || !nzchar(formPath) || !file.exists(formPath)) {
    formPath <- .findFormFile(instancesDir, formBase, formId)
  }

  if (!is.null(formPath) && nzchar(formPath) && file.exists(formPath)) {
    message("Reading repeat structure from form: ", formPath)
    formDoc <- xml2::read_xml(formPath)
    repeatPaths <- .detectRepeatPaths(formDoc)
    if (length(repeatPaths) == 0) {
      warning("No repeated groups found in the form; ",
              "the export will contain only the main (root) record.")
    }
  } else {
    warning("Could not locate the form XML (looked for '",
            if (!is.null(formId)) formId else formBase,
            ".xml'). Falling back to detecting repeats directly in the ",
            "submission XML, which fails on modern forms whose instances ",
            "do not carry the 'jr:template' marker. Pass 'formPath =' to ",
            "point to the XForm file.")
    repeatPaths <- .detectRepeatPaths(doc0)
  }

  message("Repeat paths detected: ",
          paste(c("<root>", repeatPaths), collapse = ", "))
  message("Processing ", length(xmls), " instance file(s)...")

  ## 4. Accumulator: a plain named list, passed down and returned by
  ##    the recursive walk (functional style). This avoids the pitfalls
  ##    of mutating a nested list inside an environment, which was
  ##    leaving the root record empty.
  records <- list()
  records[[ROOT_KEY]] <- list()
  for (p in repeatPaths) records[[p]] <- list()

  ## 5. Walk each instance
  nOk <- 0L
  nFail <- 0L
  for (xp in xmls) {
    ok <- tryCatch({
      doc <- xml2::read_xml(xp)
      root <- xml2::xml_find_first(doc, "/*[local-name()='data']")
      if (is.na(root)) stop("No <data> root element.")

      instID <- xml2::xml_text(xml2::xml_find_first(root, "./meta/instanceID"))
      instID <- trimws(instID)
      if (!nzchar(instID)) {
        instID <- paste0("uuid:", sub("\\.xml$", "", basename(xp)))
      }

      records <- .walkRecord(root,
                             thisKey      = instID,
                             parentKey    = "",
                             depthPath    = "",
                             records      = records,
                             repeatPaths  = repeatPaths,
                             instanceID   = instID)
      TRUE
    }, error = function(e) {
      warning("Skipping ", xp, ": ", conditionMessage(e))
      FALSE
    })
    if (isTRUE(ok)) nOk <- nOk + 1L else nFail <- nFail + 1L
  }
  message("Instances parsed OK: ", nOk, "; failed: ", nFail)

  ## Diagnostic: how many records each table received.
  message("Records per table: ",
          paste0(names(records), "=",
                 vapply(records, length, integer(1)),
                 collapse = ", "))

  ## Reorder so the root table comes first. We keep the ".root"
  ## sentinel as a name so that dfs[["..."]] never has to be looked up
  ## by the empty string.
  nms <- names(records)
  rootPos <- match(ROOT_KEY, nms)
  if (!is.na(rootPos) && rootPos != 1L) {
    ord <- c(rootPos, setdiff(seq_along(nms), rootPos))
    nms <- nms[ord]
    records <- records[ord]
  }

  ## 6. Convert to data frames, one row per record. We build each
  ##    column explicitly (a plain character vector per column) and
  ##    assemble the data frame from a named list of equal-length
  ##    vectors. This is robust for every table, including the root,
  ##    and it does not depend on the corner-case behavior of
  ##    matrix("", nrow = n, ncol = 0) + as.data.frame(), which was
  ##    silently producing a 0 x 0 data frame for the root table.
  ##    ".root" remains the internal name of the root table; it is
  ##    translated to "" only when composing the output file name.
  ##
  ##    NOTE: lst[[i]] is a named character vector, and `[[` on a
  ##    named *atomic vector* raises "subscript out of bounds" when
  ##    the name is absent (unlike on a list, where it returns NULL).
  ##    Repeats contain heterogeneous records, so a given column may
  ##    exist in some rows and not in others. We therefore check
  ##    `nmj %in% names(row)` before indexing.
  dfs <- vector("list", length(nms))
  names(dfs) <- nms

  for (k in seq_along(nms)) {
    lst <- records[[k]]
    n <- length(lst)
    if (n == 0) {
      dfs[[k]] <- data.frame()
      next
    }

    ## Union of column names, in first-appearance order.
    colNames <- character(0)
    for (i in seq_len(n)) {
      rn <- names(lst[[i]])
      if (length(rn) > 0) {
        newOnes <- setdiff(rn, colNames)
        if (length(newOnes) > 0) colNames <- c(colNames, newOnes)
      }
    }
    ## Bookkeeping columns first, then the form fields.
    metaCols <- intersect(c("KEY", "PARENT_KEY", "instanceID"), colNames)
    dataCols <- setdiff(colNames, metaCols)
    colNames <- c(metaCols, dataCols)

    if (length(colNames) == 0) {
      ## No leaf values at all: keep the correct number of rows so the
      ## record count is visible in the CSV.
      m <- matrix(NA_character_, nrow = n, ncol = 0)
      dfs[[k]] <- as.data.frame(m, stringsAsFactors = FALSE)
      next
    }

    cols <- vector("list", length(colNames))
    names(cols) <- colNames
    for (j in seq_along(colNames)) {
      nmj <- colNames[j]
      col <- character(n)
      for (i in seq_len(n)) {
        row <- lst[[i]]
        if (nmj %in% names(row)) {
          v <- row[[nmj]]
          if (length(v) == 0) {
            col[i] <- ""
          } else {
            vc <- as.character(v)
            col[i] <- if (length(vc) == 0 || is.na(vc[1L])) "" else vc[1L]
          }
        } else {
          col[i] <- ""
        }
      }
      cols[[j]] <- col
    }
    dfs[[k]] <- as.data.frame(cols, stringsAsFactors = FALSE)
  }

  ## Sanity: every table must be a data.frame so that write.csv() and
  ## nrow() always behave.
  for (k in seq_along(dfs)) {
    if (!is.data.frame(dfs[[k]])) {
      dfs[[k]] <- tryCatch(as.data.frame(dfs[[k]], stringsAsFactors = FALSE),
                           error = function(e) data.frame())
    }
  }

  ## Diagnostic: shape of each output table (helps spot empty ones).
  message("Table dims (output): ",
          paste0(names(dfs), "=",
                 vapply(dfs, function(d) {
                   if (is.data.frame(d)) paste0(NROW(d), "x", NCOL(d))
                   else paste0("?", class(d)[1L])
                 }, character(1)),
                 collapse = ", "))

  ## 7. Optionally save CSVs. The output file name is derived from the
  ##    internal name, translating the root sentinel to "" here (and
  ##    only here). This is what makes "<formBase>.csv" come out correct.
  if (!is.null(expDir)) {
    if (!dir.exists(expDir)) {
      dir.create(expDir, recursive = TRUE)
    }
    for (k in seq_along(dfs)) {
      nm <- names(dfs)[k]
      suffix <- if (identical(nm, ROOT_KEY)) "" else paste0("-", gsub("/", "-", nm))
      fname  <- file.path(expDir, paste0(formBase, suffix, ".csv"))
      if (file.exists(fname) && !overwrite) {
        warning("File exists, skipping: ", fname)
        next
      }
      df <- dfs[[k]]
      utils::write.csv(df, fname, row.names = FALSE, na = "")
      message("Wrote ", nrow(df), " row(s) to ", fname)
    }
  }

  ## 8. Optionally copy media files
  if (!is.null(mediaOutDir)) {
    if (!dir.exists(mediaOutDir)) {
      dir.create(mediaOutDir, recursive = TRUE)
    }
    allFiles  <- list.files(instancesDir, recursive = TRUE, full.names = TRUE)
    isFile    <- !dir.exists(allFiles)
    notXml    <- !grepl("\\.xml$", allFiles, ignore.case = TRUE)
    mediaFiles <- allFiles[isFile & notXml]
    if (length(mediaFiles) > 0) {
      rootNorm <- normalizePath(instancesDir, winslash = "/", mustWork = FALSE)
      for (f in mediaFiles) {
        fNorm <- normalizePath(f, winslash = "/", mustWork = FALSE)
        rel   <- sub(paste0("^", rootNorm, "/?"), "", fNorm)
        dest  <- file.path(mediaOutDir, rel)
        dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
        file.copy(f, dest, overwrite = FALSE)
      }
      message("Copied ", length(mediaFiles), " media file(s) to ", mediaOutDir)
    } else {
      message("No media files found in ", instancesDir)
    }
  }

  invisible(dfs)
}
