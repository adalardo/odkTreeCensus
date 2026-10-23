#' Merge data files from all data subdirectories
#'
#' Combines, into single files at the root of \code{dataDir}, the files
#' \code{trees.csv}/\code{treesFound.csv}, \code{multStem.csv},
#' \code{missTrees.csv}, \code{idCheck.csv} and \code{fieldSession.csv}
#' found in the data subdirectories (pattern \code{data<quadrat>},
#' e.g. \code{dataA00}, \code{dataA01}).
#'
#' All data directories are scanned on every call; the decision unit is
#' the **record**, not the directory. When \code{overwrite = FALSE},
#' records whose fields are all identical to a row already present in
#' the output files are discarded, and every other record is added
#' (regardless of its date). When \code{overwrite = TRUE}, the output
#' files are rebuilt from scratch from the current data directories.
#' The return value of \code{joinData} is a registry
#' (\code{dataQuadMerged.csv}) with columns:
#' \itemize{
#'   \item \code{mergedDir}: name of a data subdirectory that was
#'     scanned (e.g. \code{dataA00});
#'   \item \code{runDate}: timestamp of the last \code{joinData} run
#'     that scanned that directory;
#'   \item \code{stopDate}: most recent date found in the \code{date}
#'     column of that directory's \code{trees.csv} (or \code{NA} when
#'     the column/file is absent).
#' }
#' Progress messages report how many records were actually added per
#' output file and how many duplicates were skipped, plus a grand total
#' of new records at the end.
#'
#' @param dataDir Root directory containing the data subdirectories.
#'   Default: \code{getwd()}.
#' @param overwrite Logical. If \code{TRUE}, removes
#'   \code{dataQuadMerged.csv} and the \code{*All.csv} files before
#'   processing, rebuilding every output file from scratch.
#'   Default: \code{FALSE}.
#' @param quiet Logical. If \code{TRUE}, suppresses progress messages.
#'
#' @return Invisibly, a \code{data.frame} with the registry of scanned
#'   directories (\code{mergedDir}, \code{runDate} and \code{stopDate}).
#'
#' @export
joinData <- function(dataDir = getwd(), overwrite = FALSE, quiet = FALSE) {

  msg <- function(...) if (!isTRUE(quiet)) message(...)

  if (!dir.exists(dataDir)) {
    stop("The directory 'dataDir' does not exist: ", dataDir)
  }
  dataDir <- normalizePath(dataDir, winslash = "/", mustWork = TRUE)

  targets      <- .jdTargets()
  registryPath <- file.path(dataDir, "dataQuadMerged.csv")
  outFiles     <- file.path(dataDir, names(targets))

  ## ---------------------------------------------------------------------
  ## 1. Registry of scanned directories
  ## ---------------------------------------------------------------------
  emptyRegistry <- function() {
    data.frame(mergedDir = character(0),
               runDate   = character(0),
               stopDate  = character(0),
               stringsAsFactors = FALSE)
  }

  if (isTRUE(overwrite)) {
    unlink(c(registryPath, outFiles))
    registry <- emptyRegistry()
    msg("'overwrite = TRUE': previous files removed; all output files ",
        "will be rebuilt from the current data directories.")
  } else {
    registry <- .jdReadCsv(registryPath)
    if (ncol(registry) == 0L) {
      registry <- emptyRegistry()
    } else {
      ## Backward compatibility: older registries used 'date' instead
      ## of 'runDate' and had no 'stopDate'.
      if ("date" %in% names(registry) && !"runDate" %in% names(registry)) {
        names(registry)[names(registry) == "date"] <- "runDate"
      }
      if (!"mergedDir" %in% names(registry)) {
        registry <- emptyRegistry()
      } else {
        if (!"runDate"  %in% names(registry)) registry$runDate  <- NA_character_
        if (!"stopDate" %in% names(registry)) registry$stopDate <- NA_character_
        registry <- registry[, c("mergedDir", "runDate", "stopDate"),
                             drop = FALSE]
      }
    }
  }
  registry$mergedDir <- as.character(registry$mergedDir)
  registry$runDate   <- as.character(registry$runDate)
  registry$stopDate  <- as.character(registry$stopDate)

  ## ---------------------------------------------------------------------
  ## 2. Every data directory (pattern 'data<quadrat>') is scanned;
  ##    the unit of decision is the record, not the directory.
  ## ---------------------------------------------------------------------
  quadDirs  <- list.dirs(dataDir, recursive = FALSE, full.names = TRUE)
  quadNames <- basename(quadDirs)
  keep      <- grepl("^data", quadNames) & dir.exists(quadDirs)
  quadDirs  <- quadDirs[keep]
  quadNames <- quadNames[keep]

  ord       <- order(quadNames)
  quadDirs  <- quadDirs[ord]
  quadNames <- quadNames[ord]

  if (length(quadDirs) == 0L) {
    msg("No data directories (pattern 'data<quadrat>') found in ", dataDir, ".")
    return(invisible(registry))
  }

  msg("Scanning ", length(quadDirs), " data directorie(s): ",
      paste(quadNames, collapse = ", "))

  ## ---------------------------------------------------------------------
  ## 3. Merge the data files
  ## ---------------------------------------------------------------------
  totalNew <- 0L
  for (nm in names(targets)) {

    outPath  <- file.path(dataDir, nm)
    existing <- if (isTRUE(overwrite)) data.frame() else .jdReadCsv(outPath)
    nOld     <- nrow(existing)

    incoming <- .jdCollect(quadDirs, targets[[nm]])

    if (nrow(incoming) == 0L) {
      msg(sprintf("  %s: no records found in data directories; ",
                  nm),
          sprintf("file kept with %d record(s).", nOld))
      next
    }

    ## 'overwrite = TRUE' rebuilds the file from the incoming data only;
    ## 'overwrite = FALSE' appends every incoming record (regardless of
    ## date) and discards rows whose fields are all identical to a row
    ## already present in the output file.
    combined <- if (isTRUE(overwrite)) {
      incoming
    } else {
      .bindRows(list(existing, incoming))
    }
    if (nrow(combined) > 0L) {
      combined <- combined[!duplicated(combined), , drop = FALSE]
    }

    nNew <- nrow(combined) - nOld
    nDup <- (nOld + nrow(incoming)) - nrow(combined)
    totalNew <- totalNew + max(0L, nNew)

    ## 'na = ""' keeps NA cells empty in the CSV, consistent with
    ## .writeOneWithMerge() and the rest of the package outputs.
    utils::write.csv(combined, outPath, row.names = FALSE, na = "")

    msg(sprintf(
      "  %s: %d new record(s) added; %d duplicate(s) skipped ",
      nm, nNew, nDup),
      sprintf("(file now has %d record(s)).", nrow(combined)))
  }

  ## ---------------------------------------------------------------------
  ## 4. Refresh the registry (one row per scanned directory)
  ## ---------------------------------------------------------------------
  procDate <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  regNew   <- data.frame(
    mergedDir = quadNames,
    runDate   = procDate,
    stopDate  = vapply(quadDirs, .jdStopDate, character(1)),
    stringsAsFactors = FALSE
  )

  regOut <- if (nrow(registry) == 0L) {
    regNew
  } else {
    ## Replace entries for the directories just scanned; keep the rest.
    keepRows <- registry[!(registry$mergedDir %in% quadNames), , drop = FALSE]
    rbind(keepRows, regNew)
  }

  utils::write.csv(regOut, registryPath, row.names = FALSE, na = "")

  msg(sprintf("%d new record(s) added across %d output file(s). ",
              totalNew, length(targets)),
      sprintf("Registry saved to: %s", registryPath))

  invisible(regOut)
}
