#' Merge data files from all data subdirectories
#'
#' Combines, into single files at the root of \code{dataDir}, the files
#' \code{trees.csv}/\code{treesFound.csv}, \code{multStem.csv},
#' \code{missTrees.csv}, \code{idCheck.csv} and \code{fieldSession.csv}
#' found in the data subdirectories (pattern \code{data<quadrat>},
#' e.g. \code{dataA00}, \code{dataA01}).
#'
#' New records (from data directories not yet processed) are added,
#' **regardless of their date**, to the existing combined files. When
#' \code{overwrite = FALSE}, records that are already present in the
#' output files (all fields equal) are discarded; when
#' \code{overwrite = TRUE}, the output files are rebuilt from scratch
#' from the current data directories.
#'
#' Which data directories have already been processed is tracked in the
#' \code{dataQuadMerged.csv} file (columns \code{mergedDir},
#' \code{runDate} and \code{stopDate}):
#' \itemize{
#'   \item \code{runDate}: timestamp of when the directory was merged;
#'   \item \code{stopDate}: most recent date found in the \code{date}
#'     column of that directory's \code{trees.csv} (or \code{NA} when
#'     the column/file is absent).
#' }
#' Only directories not yet registered are processed.
#'
#' @param dataDir Root directory containing the data subdirectories.
#'   Default: \code{getwd()}.
#' @param overwrite Logical. If \code{TRUE}, removes
#'   \code{dataQuadMerged.csv} and the \code{*All.csv} files before
#'   processing, rebuilding every output file from scratch.
#'   Default: \code{FALSE}.
#' @param quiet Logical. If \code{TRUE}, suppresses progress messages.
#'
#' @return Invisibly, a \code{data.frame} with the registry of processed
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
  ## 1. Registry of already processed directories
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
    msg("'overwrite = TRUE': previous files removed and all ",
        "directories will be processed again.")
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
  processed <- unique(registry$mergedDir[!is.na(registry$mergedDir) &
                                           nzchar(registry$mergedDir)])

  ## ---------------------------------------------------------------------
  ## 2. Data directories (pattern 'data<quadrat>') not yet processed
  ## ---------------------------------------------------------------------
  quadDirs  <- list.dirs(dataDir, recursive = FALSE, full.names = TRUE)
  quadNames <- basename(quadDirs)
  keep      <- grepl("^data", quadNames) & dir.exists(quadDirs)
  quadDirs  <- quadDirs[keep]
  quadNames <- quadNames[keep]

  ord       <- order(quadNames)
  quadDirs  <- quadDirs[ord]
  quadNames <- quadNames[ord]

  newIdx   <- !(quadNames %in% processed)
  newDirs  <- quadDirs[newIdx]
  newNames <- quadNames[newIdx]

  if (length(newDirs) == 0L) {
    msg("No new data directories found in ", dataDir, ".")
    return(invisible(registry))
  }

  msg("New data directories: ", paste(newNames, collapse = ", "))

  ## ---------------------------------------------------------------------
  ## 3. Merge the data files
  ## ---------------------------------------------------------------------
  for (nm in names(targets)) {

    outPath  <- file.path(dataDir, nm)
    existing <- if (isTRUE(overwrite)) data.frame() else .jdReadCsv(outPath)
    nOld     <- nrow(existing)

    incoming <- .jdCollect(newDirs, targets[[nm]])

    if (nrow(incoming) == 0L) {
      msg(sprintf("  %s: no new records; file kept with %d record(s).",
                  nm, nOld))
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

    ## 'na = ""' keeps NA cells empty in the CSV, consistent with
    ## .writeOneWithMerge() and the rest of the package outputs.
    utils::write.csv(combined, outPath, row.names = FALSE, na = "")

    msg(sprintf(
      "  %s: %d new record(s) added; %d duplicate(s) skipped ",
      nm, nNew, nDup),
      sprintf("(file now has %d record(s)).", nrow(combined)))
  }

  ## ---------------------------------------------------------------------
  ## 4. Update the registry of processed directories
  ## ---------------------------------------------------------------------
  procDate <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  regNew   <- data.frame(
    mergedDir = newNames,
    runDate   = procDate,
    stopDate  = vapply(newDirs, .jdStopDate, character(1)),
    stringsAsFactors = FALSE
  )

  regOut <- if (nrow(registry) == 0L) {
    regNew
  } else {
    rbind(registry, regNew)
  }

  utils::write.csv(regOut, registryPath, row.names = FALSE, na = "")

  msg(sprintf("%d data directorie(s) processed. Registry saved to: %s",
              length(newDirs), registryPath))

  invisible(regOut)
}
