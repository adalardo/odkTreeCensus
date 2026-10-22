#' Merge data files from all data subdirectories
#'
#' Combines, into single files at the root of \code{dataDir}, the files
#' \code{trees.csv}/\code{treesFound.csv}, \code{multStem.csv},
#' \code{missTrees.csv}, \code{idCheck.csv} and \code{fieldSession.csv}
#' found in the data subdirectories (pattern \code{data<quadrat>},
#' e.g. \code{dataA00}, \code{dataA01}).
#'
#' Rows are combined with \code{merge(..., all = TRUE)} so that no variable
#' is lost: rows are stacked and columns missing on one side are filled
#' with \code{NA}.
#'
#' Which data directories have already been processed is tracked in the
#' \code{dataQuadMerged.csv} file (columns \code{mergedDir} and
#' \code{date}). Only directories not yet registered are processed.
#'
#' @param dataDir Root directory containing the data subdirectories.
#'   Default: \code{getwd()}.
#' @param overwrite Logical. If \code{TRUE}, removes
#'   \code{dataQuadMerged.csv} and the \code{*All.csv} files before
#'   processing, reprocessing every data directory from scratch.
#'   Default: \code{FALSE}.
#' @param quiet Logical. If \code{TRUE}, suppresses progress messages.
#'
#' @return Invisibly, a \code{data.frame} with the registry of processed
#'   directories (\code{mergedDir} and \code{date}).
#'
#' @export
joinData <- function(dataDir = getwd(), overwrite = FALSE, quiet = FALSE) {

  msg <- function(...) if (!isTRUE(quiet)) message(...)

  if (!dir.exists(dataDir)) {
    stop("The directory 'dataDir' does not exist: ", dataDir)
  }
  dataDir <- normalizePath(dataDir, winslash = "/", mustWork = TRUE)

  targets <- .jdTargets()
  registryPath <- file.path(dataDir, "dataQuadMerged.csv")
  outFiles <- file.path(dataDir, names(targets))

  ## ---------------------------------------------------------------------
  ## 1. Registry of already processed directories
  ## ---------------------------------------------------------------------
  if (isTRUE(overwrite)) {
    unlink(c(registryPath, outFiles))
    registry <- data.frame(mergedDir = character(0),
                           date      = character(0),
                           stringsAsFactors = FALSE)
    msg("'overwrite = TRUE': previous files removed and all ",
        "directories will be processed again.")
  } else {
    registry <- .jdReadCsv(registryPath)
    if (!all(c("mergedDir", "date") %in% names(registry))) {
      registry <- data.frame(mergedDir = character(0),
                             date      = character(0),
                             stringsAsFactors = FALSE)
    } else {
      registry <- registry[, c("mergedDir", "date"), drop = FALSE]
    }
  }
  registry$mergedDir <- as.character(registry$mergedDir)
  registry$date      <- as.character(registry$date)
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

    combined <- .jdMergeAll(existing, incoming)
    nNew     <- nrow(combined) - nOld

    utils::write.csv(combined, outPath, row.names = FALSE)

    msg(sprintf("  %s: %d new record(s) processed; %d already present in the file.",
                nm, nNew, nOld))
  }

  ## ---------------------------------------------------------------------
  ## 4. Update the registry of processed directories
  ## ---------------------------------------------------------------------
  procDate <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  regNew   <- data.frame(mergedDir = newNames,
                         date      = procDate,
                         stringsAsFactors = FALSE)

  regOut <- if (nrow(registry) == 0L) {
    regNew
  } else {
    rbind(registry, regNew)
  }

  utils::write.csv(regOut, registryPath, row.names = FALSE)

  msg(sprintf("%d data directorie(s) processed. Registry saved to: %s",
              length(newDirs), registryPath))

  invisible(regOut)
}
