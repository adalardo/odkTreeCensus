# ====================================================================
# Internal helper functions of the odkTreeCensus package
# ====================================================================
#
# This file gathers the helper functions used internally by the public
# functions of the package. All of them are for internal use and, by
# convention, use a "." (dot) prefix in their names, signalling that
# they are not part of the package's public API.
#
# Organisation:
#   1. XML instance reading helpers (used by readInstancesXML)
#   2. XLSForm -> XML conversion helper (used by xmlForm)
#   3. CSV merging helpers (used by mergeCSV)
#   4. Data merging helpers (used by joinData)
#   5. Picture link helpers (used by joinData)


# --------------------------------------------------------------------
# 1. XML instance reading helpers (readInstancesXML)
# --------------------------------------------------------------------

#' Derive the form base name from a submission file name
#'
#' ODK Collect names submission files after the form plus a
#' `_YYYY-MM-DD_HH-MM-SS` timestamp, e.g.
#' `treeCensusForm_2026-10-06_16-38-57.xml`. This helper strips the
#' extension and the timestamp, returning `"treeCensusForm"`.
#'
#' @param fname A file name (basename) of a submission XML.
#' @return A character scalar with the derived base name.
#' @keywords internal
.deriveFormBase <- function(fname) {
  fname <- sub("\\.xml$", "", fname, ignore.case = TRUE)
  sub("_[0-9]{4}-[0-9]{2}-[0-9]{2}[_-][0-9]{2}-[0-9]{2}-[0-9]{2}.*$", "", fname)
}


#' Try to locate the XForm XML for a submission
#'
#' The most reliable key is the form id carried by the submission root
#' (`<data id="...">`), which is independent of the user-supplied
#' `formBase`. This function searches for `<formId>.xml` (and, as a
#' fallback, `<formBase>.xml`) in a few conventional locations: the
#' `instances` directory itself, a sibling `forms/` directory, the parent
#' directory of `instances`, the package `odk` directory and a local
#' `inst/odk` directory. As a last resort it searches recursively under
#' the parent directory of `instances`.
#'
#' @param instancesDir Path to the ODK Collect `instances` directory.
#' @param formBase Base name for the output CSVs (used as a fallback
#'   search key only).
#' @param formId Optional form id read from the submission root; when
#'   provided it takes precedence over `formBase` as the search key.
#' @param hint Optional explicit path to try first.
#' @return A path to the found XML, or NULL if none is found.
#' @keywords internal
.findFormFile <- function(instancesDir, formBase, formId = NULL, hint = NULL) {
  cand <- character(0)
  if (!is.null(hint) && nzchar(hint)) cand <- c(cand, hint)

  keys <- unique(c(formId, formBase))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (length(keys) == 0) return(NULL)

  parent <- dirname(instancesDir)

  pkgDirs <- character(0)
  pkgDir <- system.file("odk", package = "odkTreeCensus")
  if (nzchar(pkgDir) && dir.exists(pkgDir)) pkgDirs <- c(pkgDirs, pkgDir)
  localOdk <- file.path(getwd(), "inst", "odk")
  if (dir.exists(localOdk)) pkgDirs <- c(pkgDirs, localOdk)

  for (nm in keys) {
    fname <- paste0(nm, ".xml")
    cand <- c(cand,
              file.path(instancesDir, fname),
              file.path(parent, "forms", fname),
              file.path(parent, fname),
              file.path(pkgDirs, fname))
  }

  cand <- cand[file.exists(cand)]
  if (length(cand) > 0) return(cand[1])

  ## Last resort: recursive search under the parent of `instances`
  allxml <- list.files(parent, pattern = "\\.xml$",
                       recursive = TRUE, full.names = TRUE)
  if (length(allxml) > 0) {
    instancesNorm <- normalizePath(instancesDir, winslash = "/", mustWork = FALSE)
    allNorm <- normalizePath(allxml, winslash = "/", mustWork = FALSE)
    allxml <- allxml[!startsWith(allNorm, instancesNorm)]
    for (nm in keys) {
      hit <- allxml[basename(allxml) == paste0(nm, ".xml")]
      if (length(hit) > 0) return(hit[1])
    }
  }

  NULL
}


#' Detect repeat paths in a XForm (or, as a fallback, in an instance)
#'
#' A form's repeat structure is authoritative and must be read from the
#' **XForm**. This function first reads the `nodeset` of every
#' `<repeat>` element in the form body (preferred). If the form does
#' not contain such elements, it falls back to finding `jr:template`
#' markers inside the form's primary instance. As a last resort (when
#' passed a submission document instead of a form), it looks for
#' `jr:template` directly, which only works with old-style instances.
#'
#' @param formDoc An `xml2` document: the XForm (preferred) or, as a
#'   fallback, a submission instance.
#' @return A character vector of repeat paths relative to the `/data`
#'   root, ordered from the shallowest to the deepest.
#' @keywords internal
.detectRepeatPaths <- function(formDoc) {
  ## Strategy 1: <repeat nodeset="..."> elements in the form body
  reps <- xml2::xml_find_all(formDoc, "//*[local-name()='repeat']")
  nodesets <- xml2::xml_attr(reps, "nodeset")
  nodesets <- nodesets[!is.na(nodesets) & nzchar(nodesets)]
  if (length(nodesets) > 0) {
    paths <- sub("^/data/?", "", nodesets)
    paths <- unique(paths[nzchar(paths)])
    return(.sortByDepth(paths))
  }

  ## Strategy 2: jr:template in the primary instance of the form
  insts <- xml2::xml_find_all(formDoc, "//*[local-name()='instance']")
  prim  <- NULL
  for (inst in insts) {
    a <- xml2::xml_attrs(inst)
    if (!any(names(a) %in% c("id", "src"))) {
      prim <- inst
      break
    }
  }
  if (is.null(prim)) {
    ## Strategy 3 (fallback): jr:template anywhere in the passed document
    tpls <- xml2::xml_find_all(formDoc, "//*[@*[local-name()='template']]")
  } else {
    tpls <- xml2::xml_find_all(prim, ".//*[@*[local-name()='template']]")
  }
  if (length(tpls) == 0) return(character(0))

  paths <- vapply(tpls, function(n) {
    p <- xml2::xml_path(n)
    p <- sub("^.*?/data(?:/|$)", "", p)
    p
  }, character(1))
  paths <- unique(paths[nzchar(paths)])
  .sortByDepth(paths)
}


#' Sort repeat paths from shallowest to deepest
#'
#' @param paths Character vector of slash-separated paths.
#' @return The same paths reordered by depth (and then alphabetically),
#'   so that parents always come before their children.
#' @keywords internal
.sortByDepth <- function(paths) {
  if (length(paths) == 0) return(paths)
  d <- vapply(strsplit(paths, "/", fixed = TRUE), length, integer(1))
  paths[order(d, paths)]
}


#' Identify the "direct" child repeats of a given repeat level
#'
#' A repeat `rp2` is a direct child of `rp1` when `rp1` is a proper
#' prefix of `rp2` and there is no other repeat strictly between them.
#' Non-repeat groups (e.g. `cobertura`) are transparently skipped, so
#' `subquad/cobertura/rep_cover` is considered a direct child of
#' `subquad`.
#'
#' @param depthPath The repeat path of the current level (`""` for the
#'   root record).
#' @param repeatPaths All repeat paths in the form.
#' @return A character vector of direct child repeat paths.
#' @keywords internal
.childRepeats <- function(depthPath, repeatPaths) {
  desc <- if (!nzchar(depthPath)) {
    repeatPaths
  } else {
    repeatPaths[startsWith(repeatPaths, paste0(depthPath, "/"))]
  }
  if (length(desc) == 0) return(character(0))

  ## Candidate "intermediate" repeats MUST be strict descendants of
  ## depthPath. Ancestors of depthPath MUST be excluded: otherwise,
  ## for a deep rp such as "subquad/tree/sec_stem", the ancestor
  ## "subquad" would still be in the candidate pool, and
  ## startsWith(rp, "subquad/") would be TRUE, wrongly classifying
  ## that rp as a non-direct child of "subquad". That bug made
  ## the walk skip deeper repeats entirely (e.g. sec_stem and
  ## sec_stem_miss), producing empty CSVs for them.
  candPool <- desc

  keep <- vapply(desc, function(rp) {
    cand <- setdiff(candPool, rp)
    if (length(cand) == 0) return(TRUE)
    !any(startsWith(rp, paste0(cand, "/")))
  }, logical(1))

  desc[keep]
}


#' Compute a repeat path relative to the current record level
#'
#' @param depthPath The repeat path of the current level (`""` for the
#'   root record).
#' @param rp An absolute (relative to `/data`) repeat path.
#' @return The portion of `rp` below `depthPath`.
#' @keywords internal
.relativePath <- function(depthPath, rp) {
  if (!nzchar(depthPath)) return(rp)
  pref <- paste0(depthPath, "/")
  if (startsWith(rp, pref)) substring(rp, nchar(pref) + 1L) else rp
}


#' Return the last segment of a slash-separated path
#'
#' @param path A slash-separated path.
#' @return The last element name.
#' @keywords internal
.lastSegment <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts[length(parts)]
}


#' Find all XML elements matching a slash-separated relative path
#'
#' Navigates from `node` along `relPath` (e.g. `"tree"`,
#' `"cobertura/rep_cover"`) and returns every element reached. When a
#' level matches multiple elements (e.g. several `cobertura` groups),
#' all combinations below are returned.
#'
#' @param node The starting XML element.
#' @param relPath A slash-separated path relative to `node`.
#' @return A list of matching XML elements (possibly empty).
#' @keywords internal
.findElementsAtPath <- function(node, relPath) {
  parts <- strsplit(relPath, "/", fixed = TRUE)[[1]]
  current <- list(node)
  for (p in parts) {
    nxt <- list()
    for (nd in current) {
      kids <- xml2::xml_children(nd)
      if (length(kids) == 0) next
      knames <- xml2::xml_name(kids)
      hits <- kids[knames == p]
      if (length(hits) > 0) {
        nxt <- c(nxt, as.list(hits))
      }
    }
    current <- nxt
    if (length(current) == 0) return(list())
  }
  current
}


#' Walk one record of an ODK instance and register it (plus any repeats)
#'
#' The accumulator `records` is a plain named list that is passed down
#' and returned back up by the recursion. Using a functional style avoids
#' the pitfalls of mutating a nested list through an environment, which
#' previously left the root table empty. The root record is stored under
#' the internal sentinel key `".root"` (see `ROOT_KEY` in
#' `readInstancesXML`).
#'
#' @param node The XML element representing the record.
#' @param thisKey The KEY assigned to this record.
#' @param parentKey The KEY of the parent record ("" for the root).
#' @param depthPath The repeat path of this record ("" for the root).
#' @param records Named list of lists of named character vectors
#'   (one entry per observation). Returned, updated.
#' @param repeatPaths Character vector of all repeat paths in the form.
#' @param instanceID Submission id, added to every row as a column.
#' @return The updated `records` list.
#' @keywords internal
.walkRecord <- function(node, thisKey, parentKey, depthPath,
                        records, repeatPaths, instanceID = "") {
  ## 1. Direct repeat children for this record (repeats may be nested
  ##    under non-repeat groups, e.g. cobertura/rep_cover).
  directRepeats <- .childRepeats(depthPath, repeatPaths)

  ## 2. Collect leaf values (stopping at ANY repeat boundary)
  rec <- .collectLeafValues(node, depthPath = depthPath, repeatPaths = repeatPaths)
  rec["KEY"] <- thisKey
  if (nzchar(parentKey)) rec["PARENT_KEY"] <- parentKey
  if (nzchar(instanceID)) rec["instanceID"] <- instanceID

  ## 3. Register this record. The root uses the internal sentinel
  ##    ".root" instead of "" to avoid ambiguous empty-string indexing.
  storagePath <- if (nzchar(depthPath)) depthPath else ".root"
  records[[storagePath]] <- c(records[[storagePath]], list(rec))

  ## 4. Recurse into direct repeat children
  if (length(directRepeats) > 0) {
    for (rp in directRepeats) {
      relPath <- .relativePath(depthPath, rp)
      matches <- .findElementsAtPath(node, relPath)
      rnName  <- .lastSegment(rp)
      for (i in seq_along(matches)) {
        childKey <- paste0(thisKey, "/", rnName, "[", i, "]")
        records <- .walkRecord(matches[[i]],
                               thisKey      = childKey,
                               parentKey    = thisKey,
                               depthPath    = rp,
                               records      = records,
                               repeatPaths  = repeatPaths,
                               instanceID   = instanceID)
      }
    }
  }

  records
}


#' Collect leaf values (dot-separated paths) under an XML element
#'
#' Recurses through the tree, producing a named character vector where
#' names are dot-separated paths (e.g. `"parcela.quadrat"`). Any
#' descendant whose absolute path is a repeat boundary (see
#' `repeatPaths`) is not entered: those elements belong to their own
#' records and are handled by `.walkRecord`.
#'
#' @param node The XML element (a record).
#' @param depthPath The repeat path of the current record (`""` for the
#'   root).
#' @param repeatPaths All repeat paths in the form.
#' @param namePrefix Dot-separated prefix for column names (internal).
#' @param pathPrefix Slash-separated prefix for path comparison
#'   (internal).
#' @return A named character vector.
#' @keywords internal
.collectLeafValues <- function(node, depthPath, repeatPaths,
                               namePrefix = "", pathPrefix = "") {
  out <- character(0)
  kids <- xml2::xml_children(node)
  for (k in kids) {
    nm <- xml2::xml_name(k)

    nameFull  <- if (nzchar(namePrefix)) paste0(namePrefix, ".", nm) else nm
    pathFull  <- if (nzchar(pathPrefix)) paste0(pathPrefix, "/", nm) else nm
    fullRepeat <- if (nzchar(depthPath)) paste0(depthPath, "/", pathFull) else pathFull

    if (fullRepeat %in% repeatPaths) next

    grand <- xml2::xml_children(k)
    if (length(grand) == 0) {
      val <- xml2::xml_text(k)
      out[nameFull] <- if (is.na(val)) "" else val
    } else {
      sub <- .collectLeafValues(k,
                                depthPath   = depthPath,
                                repeatPaths = repeatPaths,
                                namePrefix  = nameFull,
                                pathPrefix  = pathFull)
      out <- c(out, sub)
    }
  }
  out
}


# --------------------------------------------------------------------
# 2. XLSForm -> XML conversion helper (xmlForm)
# --------------------------------------------------------------------

#' @noRd
setupPyEnv <- function() {
  pkg_name <- "odkTreeCensus"
  # 1. Try the development path (local project folder)
  dev_path <- file.path("inst", "python")

  # 2. If the local 'inst/python' folder exists (dev mode), use it.
  # Otherwise, use the default path of the installed package.
  if (dir.exists(dev_path)) {
    wheels_dir <- dev_path
  } else {
    wheels_dir <- system.file("python", package = pkg_name)
  }

  # Simple validation
  if (wheels_dir == "" || !dir.exists(wheels_dir)) {
    stop("Error: the 'inst/python' (dev) or 'python' (installed) folder was not found.")
  }

  # Path to save the virtualenv in the user cache
  venv_path <- file.path(tools::R_user_dir(pkg_name, "cache"), "python_env")

  # Offline installation when the Python environment does not exist in cache
  if (!reticulate::virtualenv_exists(envname = venv_path)) {
    message("Creating offline Python environment...")
    reticulate::virtualenv_create(envname = venv_path)

    wheels <- list.files(wheels_dir, pattern = "\\.whl$", full.names = TRUE)

    if (length(wheels) == 0) {
      stop(paste("No .whl file found in:", wheels_dir))
    }

    # Install the wheels without using the internet
    reticulate::py_install(
      packages = wheels,
      envname = venv_path,
      pip_options = c("--no-index", paste0("--find-links=", wheels_dir))
    )
  }

  # Activate the virtualenv in the session
  reticulate::use_virtualenv(virtualenv = venv_path, required = TRUE)
}


# --------------------------------------------------------------------
# 3. CSV merging helpers (mergeCSV)
# --------------------------------------------------------------------
#
# These functions used to be defined as local closures inside mergeCSV.
# Once promoted to package level, they receive as arguments everything
# that was previously captured by the mergeCSV environment:
#   - csvDir, csvFiles, dataList, keyEnv, etc.

#' @noRd
.tidyName <- function(x) sub("^.*\\.", "", x)


#' @noRd
.round1 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), NA_real_, round(x, 1))
}


#' @noRd
.asMediaPath <- function(x) {
  x <- as.character(x)
  ok <- !is.na(x) & nzchar(x)
  x[ok] <- file.path("media", x[ok])
  x
}


#' @noRd
.isEmptyCsvFile <- function(path) {
  sz <- suppressWarnings(file.size(path))
  if (is.na(sz) || sz <= 0L) return(TRUE)

  bytes <- tryCatch(readBin(path, what = "raw", n = sz),
                    error = function(e) raw(0))
  if (length(bytes) == 0L) return(TRUE)

  ## Strip a leading UTF-8 BOM (EF BB BF)
  if (length(bytes) >= 3L &&
      bytes[1L] == as.raw(0xEF) &&
      bytes[2L] == as.raw(0xBB) &&
      bytes[3L] == as.raw(0xBF)) {
    bytes <- bytes[-(1L:3L)]
  }
  if (length(bytes) == 0L) return(TRUE)

  ## Only whitespace bytes left -> treat as empty
  all(as.integer(bytes) %in% c(0x09L, 0x0AL, 0x0DL, 0x20L))
}


#' @noRd
.readOne <- function(f, csvDir) {
  path <- file.path(csvDir, f)

  if (.isEmptyCsvFile(path)) {
    return(data.frame())
  }

  lines <- tryCatch(readLines(path, warn = FALSE),
                    error = function(e) character(0))
  lines <- lines[!is.na(lines)]
  if (length(lines) > 0L) {
    lines[1L] <- sub("^\xef\xbb\xbf", "", lines[1L], useBytes = TRUE)
  }
  lines <- lines[vapply(lines, function(x) nzchar(trimws(x)), logical(1))]

  if (length(lines) == 0L) {
    return(data.frame())
  }

  d <- tryCatch(
    utils::read.csv(text = paste(lines, collapse = "\n"),
                    stringsAsFactors = FALSE,
                    check.names      = FALSE),
    error = function(e) data.frame()
  )

  if (ncol(d) > 0L) names(d) <- .tidyName(names(d))
  d
}


#' @noRd
.getCol <- function(d, nm, default = NA) {
  if (nm %in% names(d)) return(d[[nm]])
  rep(default, nrow(d))
}


#' @noRd
.bindRows <- function(lst) {
  lst <- lst[vapply(lst, function(x) !is.null(x) && nrow(x) > 0L, logical(1))]
  if (length(lst) == 0L) return(data.frame())
  allCols <- unique(unlist(lapply(lst, names)))
  lst <- lapply(lst, function(d) {
    miss <- setdiff(allCols, names(d))
    for (m in miss) d[[m]] <- NA
    d[, allCols, drop = FALSE]
  })
  do.call(rbind, lst)
}


#' @noRd
.readCsvSafe <- function(path) {
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )
}


#' @noRd
.baseName <- function(f) gsub("-.*|\\.csv", "", f)


#' @noRd
.readMergedBaseNames <- function(path) {
  if (!file.exists(path)) return(character(0))
  oldFiles <- .readCsvSafe(path)
  if (nrow(oldFiles) == 0L) return(character(0))
  colOld <- if ("fileNames" %in% names(oldFiles)) {
    as.character(oldFiles$fileNames)
  } else {
    as.character(oldFiles[[1L]])
  }
  colOld[!is.na(colOld) & nzchar(colOld)]
}


#' @noRd
.ancestorValue <- function(parentKey, field, keyEnv) {
  cur <- as.character(parentKey)
  hops <- 0L
  while (!is.na(cur) && nzchar(cur) && hops < 100L) {
    entry <- keyEnv[[cur]]
    if (is.null(entry)) break
    row <- entry$row
    if (field %in% names(row)) {
      v <- row[[field]][1]
      if (!is.na(v) && nzchar(as.character(v))) return(v)
    }
    if ("PARENT_KEY" %in% names(row)) {
      cur <- as.character(row$PARENT_KEY[1])
    } else {
      break
    }
    hops <- hops + 1L
  }
  NA_character_
}


#' @noRd
.fieldSelfOrAncestor <- function(d, field, keyEnv) {
  self <- if (field %in% names(d)) as.character(d[[field]]) else
    rep(NA_character_, nrow(d))
  pk <- if ("PARENT_KEY" %in% names(d)) as.character(d$PARENT_KEY) else
    rep(NA_character_, nrow(d))
  for (i in seq_along(self)) {
    if (is.na(self[i]) || !nzchar(self[i])) {
      self[i] <- as.character(.ancestorValue(pk[i], field, keyEnv))
    }
  }
  self
}


#' Normalise a date value to "YYYY-MM-DD"
#'
#' The date stored in the `date` column of every output file must be the
#' **collection** date, i.e. the value of the form field `today`, and
#' never the date on which \code{mergeCSV} happened to be run. ODK
#' stores such values in different shapes depending on the form:
#' \itemize{
#'   \item a plain date, e.g. `"2024-05-10"`;
#'   \item an ISO datetime, e.g. `"2024-05-10T00:00:00.000-03:00"`;
#'   \item a date in some other human format accepted by
#'     \code{as.Date} (e.g. `"10/05/2024"`).
#' }
#' Only the calendar date part is kept. Missing/empty/unparseable
#' values fall back to the current date so that legacy data (collected
#' before `today` existed in the form) do not end up with `NA`.
#'
#' @param x A character vector of date values.
#' @return A character vector of `"YYYY-MM-DD"` values, same length as
#'   `x`.
#' @keywords internal
.normalizeDate <- function(x) {
  n <- length(x)
  if (n == 0L) return(character(0))

  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- trimws(x)

  out <- rep(NA_character_, n)

  ## 1. ISO date or ISO datetime: keep only the YYYY-MM-DD part
  iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  out[iso] <- substr(x[iso], 1L, 10L)

  ## 2. Anything else non-empty: let as.Date try to parse it
  rest <- !iso & nzchar(x)
  if (any(rest)) {
    alt <- suppressWarnings(as.Date(x[rest]))
    out[rest] <- ifelse(is.na(alt), NA_character_, format(alt))
  }

  ## 3. Last resort: current date (legacy data without `today`)
  miss <- is.na(out) | !nzchar(out)
  out[miss] <- format(Sys.Date())

  out
}


#' @noRd
.contextDf <- function(d, keyEnv) {
  n <- nrow(d)

  ## 'date' must be the collection date (form field 'today'), never the
  ## processing date. As with plot/quad/subquad/team, 'today' may live
  ## only on the parent record (root), so it is resolved through the
  ## PARENT_KEY chain.
  today <- .fieldSelfOrAncestor(d, "today", keyEnv)

  data.frame(
    date    = .normalizeDate(today),
    plot    = .fieldSelfOrAncestor(d, "plot_name",   keyEnv),
    quad    = .fieldSelfOrAncestor(d, "quadrat",     keyEnv),
    subquad = .fieldSelfOrAncestor(d, "sel_subquad", keyEnv),
    team    = .fieldSelfOrAncestor(d, "equipe_nomes", keyEnv),
    stringsAsFactors = FALSE
  )
}


#' @noRd
.pick <- function(pattern, csvFiles) {
  csvFiles[grepl(pattern, csvFiles, ignore.case = TRUE)]
}


#' @noRd
.getOne <- function(f, dataList) dataList[[f]]


#' @noRd
.writeOneWithMerge <- function(df, dirOut, name) {
  path <- file.path(dirOut, name)

  oldExists <- file.exists(path)
  old <- if (oldExists) .readCsvSafe(path) else data.frame()
  nOld <- nrow(old)

  ## Drop duplicated rows from the incoming data itself
  if (nrow(df) > 0L) {
    df <- df[!duplicated(df), , drop = FALSE]
  }

  if (oldExists) {
    ## Keep everything that already exists and add only the new rows
    merged <- .bindRows(list(old, df))
    if (ncol(merged) == 0L) merged <- df
    if (nrow(merged) > 0L) {
      merged <- merged[!duplicated(merged), , drop = FALSE]
    }
    utils::write.csv(merged, path, row.names = FALSE, na = "")
    nTotal <- nrow(merged)
    nAdded <- max(0L, nTotal - nOld)
    message(sprintf("  - %s: merged (%d + %d = %d record(s)).",
                    name, nOld, nAdded, nTotal))
    return(list(file = name, old = nOld, new = nAdded, total = nTotal,
                action = "merge"))
  }

  utils::write.csv(df, path, row.names = FALSE, na = "")
  nNew <- nrow(df)
  message(sprintf("  - %s: created (%d record(s)).", name, nNew))
  list(file = name, old = 0L, new = nNew, total = nNew, action = "create")
}


# --------------------------------------------------------------------
# 4. Data merging helpers (joinData)
# --------------------------------------------------------------------
#
# Specific to joinData. The ".jd" prefix keeps them clearly distinct
# from the general-purpose CSV helpers in section 3 (e.g. .jdMergeAll
# versus .bindRows), so that a future reader does not mistake one for
# the other.

#' Output files and their corresponding source files
#' @noRd
.jdTargets <- function() {
  list(
    "treesAll.csv"        = c("trees.csv", "treesFound.csv"),
    "multStemAll.csv"     = c("multStem.csv"),
    "missTreesAll.csv"    = c("missTrees.csv"),
    "idCheckAll.csv"      = c("idCheck.csv"),
    "fieldSessionAll.csv" = c("fieldSession.csv")
  )
}

#' Read a CSV tolerating missing, empty, or invalid files
#' @noRd
.jdReadCsv <- function(path) {
  if (!file.exists(path)) return(data.frame())

  sz <- suppressWarnings(file.size(path))
  if (is.na(sz) || sz <= 0L) return(data.frame())

  d <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )

  if (is.null(d) || ncol(d) == 0L) return(data.frame())
  d
}

#' Combine two data.frames with `merge(..., all = TRUE)`
#'
#' Uses exactly the approach that was validated locally:
#'
#' \preformatted{
#'   trees <- data.frame()
#'   for (i in files) {
#'       tt    <- read.table(i, header = TRUE, sep = ",")
#'       trees <- merge(tt, trees, all = TRUE)
#'   }
#' }
#'
#' When `by` is not supplied, `merge()` uses every same-named column as
#' a natural key. For files that share the same schema (all `trees.csv`
#' of different quadrats, for example), this stacks the rows without
#' creating `date.x`/`date.y` columns.
#' @noRd
.jdMergeAll <- function(x, y) {

  xOk <- !is.null(x) && ncol(x) > 0L && nrow(x) > 0L
  yOk <- !is.null(y) && ncol(y) > 0L && nrow(y) > 0L

  if (!xOk && !yOk) return(data.frame())
  if (!xOk) return(y)
  if (!yOk) return(x)

  ## Same call the user tested locally: merge(x, y, all = TRUE),
  ## with no explicit `by`.
  out <- merge(x, y, all = TRUE)
  rownames(out) <- NULL
  out
}

#' Most recent \code{date} value found in a quad's \code{trees.csv}
#'
#' Scans every \code{trees.csv} under `quadDir` (recursively) and
#' returns the most recent value of its \code{date} column as a
#' \code{"YYYY-MM-DD"} string. Returns \code{NA_character_} when no
#' \code{trees.csv} (or no \code{date} column / no dates) is found.
#'
#' ISO-shaped dates (the format produced by \code{.normalizeDate}) are
#' parsed through the \code{YYYY-MM-DD} prefix. If none parse, the
#' lexicographic maximum is returned as a last resort.
#'
#' @param quadDir Path to a data directory (e.g. \code{dataA00}).
#' @return A character scalar with the most recent date, or
#'   \code{NA_character_}.
#' @keywords internal
.jdStopDate <- function(quadDir) {
  files <- list.files(quadDir, pattern = "^trees\\.csv$",
                      recursive = TRUE, full.names = TRUE)
  if (length(files) == 0L) return(NA_character_)

  dates <- character(0)
  for (f in files) {
    d <- .jdReadCsv(f)
    if (nrow(d) == 0L || !"date" %in% names(d)) next
    v <- as.character(d$date)
    v <- v[!is.na(v) & nzchar(v)]
    dates <- c(dates, v)
  }
  if (length(dates) == 0L) return(NA_character_)

  parsed <- suppressWarnings(as.Date(substr(dates, 1L, 10L)))
  parsed <- parsed[!is.na(parsed)]
  if (length(parsed) == 0L) return(max(dates))
  format(max(parsed))
}

#' List the files of interest in a data directory
#'
#' Preserves the order in which names appear in `fileNames`
#' (e.g. `trees.csv` before `treesFound.csv`).
#' @noRd
.jdFilesInDir <- function(quadDir, fileNames) {
  allFiles <- list.files(quadDir, recursive = TRUE, full.names = TRUE)
  if (length(allFiles) == 0L) return(character(0))

  allFiles <- allFiles[!dir.exists(allFiles)]
  allFiles <- allFiles[basename(allFiles) %in% fileNames]
  if (length(allFiles) == 0L) return(character(0))

  ord <- match(basename(allFiles), fileNames)
  allFiles[order(ord, allFiles)]
}

#' Collect and stack records from several data directories
#' @noRd
.jdCollect <- function(quadDirs, fileNames) {
  out <- data.frame()

  for (qd in quadDirs) {
    files <- .jdFilesInDir(qd, fileNames)
    for (f in files) {
      d <- .jdReadCsv(f)
      if (nrow(d) == 0L) next
      out <- .jdMergeAll(out, d)
    }
  }

  out
}


# --------------------------------------------------------------------
# 5. Picture link helpers (joinData)
# --------------------------------------------------------------------
#
# A CSV file stores only text. Building a "=HYPERLINK(...)" formula in
# a CSV cell only works when the reader (Excel/LibreOffice) evaluates
# formulas on import. LibreOffice, by default, does NOT: with
# "Evaluate formulas" disabled (the CSV-import default) the raw text
# of the formula is shown and the cell is not clickable. That is why
# the previous approach failed there.
#
# The portable solution - the one that matches a link created through
# the LibreOffice "Hyperlink" dialog and then exported to CSV - is to
# store the bare "file:///absolute/path" URL as the cell content.
# LibreOffice and Excel both recognise such URLs in a CSV as
# clickable hyperlinks on open, regardless of the formula setting.
#
# A 'pictures' cell may contain several media paths separated by ';'.
# Each path becomes its own file:// URL, and they are joined back with
# '; '. Neither LibreOffice nor Excel supports several independent
# hyperlinks inside a single cell, so in multi-path cells only the
# first URL is generally clickable - a limitation of the CSV format,
# not of the code.

#' Columns to be linked (matches "pictures" and "pictureNewTag")
#'
#' Returns the names of the source columns whose values are media
#' paths, i.e. any column whose name contains "picture" (case
#' insensitive) and is not itself a generated '<col>Link' column.
#'
#' @param d A data.frame.
#' @return A character vector of column names (possibly empty).
#' @keywords internal
.pictureColumns <- function(d) {
  nm <- names(d)
  if (length(nm) == 0L) return(character(0))
  isPic  <- grepl("picture", nm, ignore.case = TRUE)
  isLink <- grepl("Link$", nm, ignore.case = FALSE)
  nm[isPic & !isLink]
}

#' Name of the '<col>Link' companion column
#'
#' @param col Source column name, e.g. \code{"pictures"}.
#' @return The target column name, e.g. \code{"picturesLink"}.
#' @keywords internal
.pictureLinkName <- function(col) paste0(col, "Link")

#' Convert an absolute path to a file:// URL
#'
#' On Unix, "/home/user/x.jpg" becomes "file:///home/user/x.jpg".
#' On Windows, "C:/Users/x.jpg" becomes "file:///C:/Users/x.jpg".
#' Forward slashes are used in both cases.
#' @noRd
.fileUrl <- function(path) {
  p <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = FALSE),
    error = function(e) as.character(path)
  )
  if (.Platform$OS.type == "windows" && !startsWith(p, "/")) {
    p <- paste0("/", p)
  }
  paste0("file://", p)
}

#' Build file:// URLs from one or several media paths
#'
#' Each cell may hold one path ("media/x.jpg") or several separated
#' by ";". Each path is resolved to its absolute file:// URL; when
#' there is more than one, the URLs are rejoined with "; " so the
#' cell keeps the same shape as the source 'pictures' column. See
#' the section header above for the (unavoidable) limitation
#' regarding multi-link clickability.
#'
#' Cells already starting with "file://" are skipped (idempotent).
#' A legacy "=HYPERLINK(...)" cell is also skipped here - it will be
#' cleaned up by .pictureUnlinkCell() before this is called (see
#' joinData). Empty/NA cells are left untouched.
#'
#' @param x Character vector of media paths.
#' @param baseDir Directory the paths are relative to (usually the
#'   \code{dataDir} where the \code{media/} folder lives).
#' @return A character vector with file:// URLs.
#' @keywords internal
.pictureLinkCell <- function(x, baseDir) {
  x <- as.character(x)
  for (i in seq_along(x)) {
    if (is.na(x[i]) || !nzchar(x[i])) next
    if (startsWith(x[i], "file://")) next
    if (startsWith(x[i], "=HYPERLINK(")) next

    parts <- trimws(strsplit(x[i], ";", fixed = TRUE)[[1]])
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0L) next

    urls <- vapply(parts, function(p) {
      .fileUrl(file.path(baseDir, p))
    }, character(1))

    x[i] <- paste(urls, collapse = "; ")
  }
  x
}

#' Reverse of .pictureLinkCell: extract the label(s) from HYPERLINK(s)
#'
#' Handles the legacy single-link form
#' \code{'=HYPERLINK("file:///...","media/x.jpg")'} -> \code{"media/x.jpg"}
#' and the legacy concatenated multi-link form
#' \code{'=HYPERLINK(...,"a") & "; " & HYPERLINK(...,"b")'} -> \code{"a; b"}.
#' It also flattens "file://.../media/x.jpg" URLs back to "media/x.jpg"
#' (label relative to the 'media/' folder). Non-formula/non-URL cells
#' are left unchanged, so deduplication keeps working even when a
#' previous output has already stored formulas or URLs in the source
#' column.
#'
#' @param x Character vector.
#' @return Character vector with the paths re-extracted.
#' @keywords internal
.pictureUnlinkCell <- function(x) {
  x <- as.character(x)

  ## Legacy: '=HYPERLINK("url","label")' concatenated with ' & '.
  isFormula <- !is.na(x) & startsWith(x, "=HYPERLINK(")
  for (i in which(isFormula)) {
    m <- gregexpr('HYPERLINK\\("[^"]*","((?:[^"]|"")*)"\\)', x[i],
                  perl = TRUE)
    hits <- regmatches(x[i], m)[[1]]
    if (length(hits) == 0L) next
    labels <- sub('^HYPERLINK\\("[^"]*","((?:[^"]|"")*)"\\)$',
                  "\\1", hits, perl = TRUE)
    labels <- gsub('""', '"', labels)
    x[i] <- paste(labels, collapse = "; ")
  }

  ## URLs written in the source column (should not normally happen):
  ## flatten "file://.../media/x.jpg" back to "media/x.jpg" so that
  ## deduplication against the incoming data keeps working.
  isUrl <- !is.na(x) & startsWith(x, "file://")
  for (i in which(isUrl)) {
    parts <- trimws(strsplit(x[i], ";", fixed = TRUE)[[1]])
    parts <- parts[nzchar(parts)]
    parts <- sub("^file://.*?/(media/[^/]+)$", "\\1", parts)
    x[i] <- paste(parts, collapse = "; ")
  }

  x
}
