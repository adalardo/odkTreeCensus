#' Merge and tidy the CSV files exported by readInstancesXML
#'
#' Reads the CSV files produced by \code{\link{readInstancesXML}}, links the
#' records through the \code{KEY} / \code{PARENT_KEY} columns, repeats the
#' parent fields at the child level and writes a set of flat CSV files ---
#' one row per lowest level record.
#'
#' Output is split by quadrat: each quadrat gets its own sub-directory inside
#' \code{expDir}, named \code{data<quadrat>} (e.g. \code{dataA00}). When the
#' destination file already exists, the new records are merged into it without
#' discarding the existing ones, and a summary of how many records were found
#' and how many were added is printed for every file.
#'
#' Two extra files are kept at the root of \code{expDir}:
#' \itemize{
#'   \item \code{tagsUsed.csv} --- created when missing, with the columns
#'     \code{quad}, \code{subquad} and \code{tag} for every record present in
#'     any \code{trees.csv} file inside the per-quadrat directories.
#'   \item \code{tagsRepeat.csv} --- written when a \code{tag} value appears in
#'     more than one record of \code{trees.csv} (even across different
#'     directories). It holds the full \code{trees.csv} fields of both the
#'     existing and the newly added records.
#' }
#'
#' \code{trees.csv} is never allowed to hold duplicated records: a new row
#' identical (in all fields) to an existing one is not included again.
#'
#' Media files that live under a \code{media} folder (possibly with
#' sub-directories) are copied, flattened, into \code{expDir/media}; file
#' names stored in the data are rewritten as \code{file.path("media", "x.jpg")}
#' so they can be opened directly from a spreadsheet.
#'
#' @param csvDir Character. Directory that contains the CSV files exported by
#'   \code{readInstancesXML}.
#' @param expDir Character. Export directory. Defaults to \code{csvDir}.
#' @param mediaDir Character. Directory that contains the \code{media} folder
#'   with sub-directories. Defaults to \code{csvDir}.
#' @param overwrite Logical. Default \code{FALSE}. When \code{FALSE} and the
#'   destination file already exists, the new records are merged into the
#'   existing ones (no data loss). When \code{TRUE} and the session is
#'   interactive, the user is asked whether to merge, overwrite or skip ---
#'   overwriting is never done silently because it discards the record count
#'   stored in the existing file.
#'
#' @return Invisibly, the path to \code{expDir}.
#' @export
mergeCSV <- function(csvDir, expDir = NULL, mediaDir = NULL, overwrite = FALSE) {

  if (is.null(expDir))   expDir   <- csvDir
  if (is.null(mediaDir)) mediaDir <- csvDir

  if (!dir.exists(csvDir)) {
    stop("The directory 'csvDir' does not exist: ", csvDir)
  }
  if (!dir.exists(expDir)) {
    dir.create(expDir, recursive = TRUE)
  }
  ## Shared media folder (all quadrats use the same one)
  mediaDst <- file.path(expDir, "media")
  if (!dir.exists(mediaDst)) {
    dir.create(mediaDst, recursive = TRUE)
  }

  ## ------------------------------------------------------------------ ##
  ## Helpers                                                            ##
  ## ------------------------------------------------------------------ ##

  ## Final variable name: keep only the part after the last "."
  tidyName <- function(x) sub("^.*\\.", "", x)

  ## Numeric conversion rounded to one decimal place (NA stays NA)
  round1 <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    ifelse(is.na(x), NA_real_, round(x, 1))
  }

  ## Turn a file name into a path inside the "media" folder, so the link
  ## opens when the CSV is read in a spreadsheet. NA / "" are kept as-is.
  asMediaPath <- function(x) {
    x <- as.character(x)
    ok <- !is.na(x) & nzchar(x)
    x[ok] <- file.path("media", x[ok])
    x
  }

  ## Byte-level check for "empty" CSV files. Immune to encoding issues
  ## (which is what makes gsub()/trimws() unreliable for the UTF-8 BOM).
  ## Returns TRUE when the file has no content other than an optional
  ## leading UTF-8 BOM and whitespace (space, tab, CR, LF).
  ##
  ## This is what catches the little 3-byte files (a lone BOM) that the
  ## ODK export produces when a repeat has no records, and which make
  ## read.table fail with "primeiras cinco linhas estão vazias".
  isEmptyCsvFile <- function(path) {
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

  readOne <- function(f) {
    path <- file.path(csvDir, f)

    ## Skip files that are empty or contain only a BOM / blank lines.
    if (isEmptyCsvFile(path)) {
      return(data.frame())
    }

    ## Read lines and drop a possible BOM at the start of the first line
    ## (byte level, to avoid encoding surprises) and blank lines.
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

    ## Feed the cleaned lines to read.csv via `text=`. The tryCatch is a
    ## final safety net so a single bad file never aborts the whole merge.
    d <- tryCatch(
      utils::read.csv(text = paste(lines, collapse = "\n"),
                      stringsAsFactors = FALSE,
                      check.names      = FALSE),
      error = function(e) data.frame()
    )

    if (ncol(d) > 0L) names(d) <- tidyName(names(d))
    d
  }

  ## Get a column by (tidied) name; NA vector when the column is absent
  getCol <- function(d, nm, default = NA) {
    if (nm %in% names(d)) return(d[[nm]])
    rep(default, nrow(d))
  }

  ## Row-bind data.frames that may have different columns
  bindRows <- function(lst) {
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

  ## Safe read of a CSV; returns an empty data.frame on any error
  readCsvSafe <- function(path) {
    tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) data.frame()
    )
  }

  ## ------------------------------------------------------------------ ##
  ## 1. Read every CSV and tidy the column names                        ##
  ## ------------------------------------------------------------------ ##
  csvFiles <- list.files(csvDir, pattern = "\\.[Cc][Ss][Vv]$", full.names = FALSE)
  if (length(csvFiles) == 0L) {
    stop("No CSV files were found in 'csvDir': ", csvDir)
  }

  ## Drop empty files (0 bytes, BOM-only or only blank lines) before reading.
  fileEmpty <- vapply(csvFiles,
                      function(f) isEmptyCsvFile(file.path(csvDir, f)),
                      logical(1))
  if (any(fileEmpty)) {
    message("Skipping ", sum(fileEmpty), " empty CSV file(s): ",
            paste(csvFiles[fileEmpty], collapse = ", "))
  }
  csvFiles <- csvFiles[!fileEmpty]

  if (length(csvFiles) == 0L) {
    return(invisible(expDir))
  }

  dataList <- lapply(csvFiles, readOne)
  names(dataList) <- csvFiles

  ## ------------------------------------------------------------------ ##
  ## 2. Global KEY -> row index (used to walk the PARENT_KEY chain)     ##
  ## ------------------------------------------------------------------ ##
  keyEnv <- new.env(parent = emptyenv())
  for (f in csvFiles) {
    d <- dataList[[f]]
    if (!"KEY" %in% names(d)) next
    keys <- as.character(d$KEY)
    for (i in seq_along(keys)) {
      k <- keys[i]
      if (is.na(k) || !nzchar(k)) next
      keyEnv[[k]] <- list(row = d[i, , drop = FALSE], file = f)
    }
  }

  ancestorValue <- function(parentKey, field) {
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

  ## Value from the row itself, or from the closest ancestor if missing
  fieldSelfOrAncestor <- function(d, field) {
    self <- if (field %in% names(d)) as.character(d[[field]]) else
      rep(NA_character_, nrow(d))
    pk <- if ("PARENT_KEY" %in% names(d)) as.character(d$PARENT_KEY) else
      rep(NA_character_, nrow(d))
    for (i in seq_along(self)) {
      if (is.na(self[i]) || !nzchar(self[i])) {
        self[i] <- as.character(ancestorValue(pk[i], field))
      }
    }
    self
  }

  ## Common context columns for every output file
  contextDf <- function(d) {
    n <- nrow(d)
    data.frame(
      date   = rep(format(Sys.Date()), n),
      plot   = fieldSelfOrAncestor(d, "plot_name"),
      quad   = fieldSelfOrAncestor(d, "quadrat"),
      subquad = fieldSelfOrAncestor(d, "sel_subquad"),
      team   = fieldSelfOrAncestor(d, "equipe_nomes"),
      stringsAsFactors = FALSE
    )
  }

  ## ------------------------------------------------------------------ ##
  ## 3. Group the CSV files                                             ##
  ## ------------------------------------------------------------------ ##
  filesNoExt <- sub("\\.[Cc][Ss][Vv]$", "", csvFiles)
  pick <- function(pattern) csvFiles[grepl(pattern, csvFiles, ignore.case = TRUE)]

  rootF   <- csvFiles[!grepl("-", filesNoExt)]             # roots (no "-")
  subqF   <- pick("-subquad\\.csv$")                       # *-subquad.csv
  coverF  <- pick("-subquad-cobertura-rep_cover\\.csv$")   # cobertura
  treeF   <- pick("-subquad-tree\\.csv$")                  # trees
  treeNFF <- pick("-subquad-rep_miss\\.csv$")              # not found
  stemF   <- pick("sec_stem\\.csv$")                       # multStem (main)
  stemMF  <- pick("sec_stem_miss\\.csv$")                  # multStem (missing)

  getOne <- function(f) dataList[[f]]

  fieldSession <- bindRows(lapply(rootF,   getOne))
  subquadDf    <- bindRows(lapply(subqF,   getOne))
  coverDf      <- bindRows(lapply(coverF,  getOne))
  treesDf      <- bindRows(lapply(treeF,   getOne))
  treeNFDf     <- bindRows(lapply(treeNFF, getOne))

  ## multStem: combine sec_stem and sec_stem_miss, stripping "_miss" from names
  stemList <- lapply(c(stemF, stemMF), function(f) {
    d <- dataList[[f]]
    names(d) <- sub("_miss$", "", names(d))
    d
  })
  multStemDf <- bindRows(stemList)

  dropKeys <- c("KEY", "PARENT_KEY")

  ## ------------------------------------------------------------------ ##
  ## 4. fieldSession.csv                                                ##
  ##    drop 'instanceID' and any '*generate_note*' column              ##
  ## ------------------------------------------------------------------ ##
  fsCols <- setdiff(names(fieldSession), dropKeys)
  fsCols <- fsCols[!grepl("instanceID",    fsCols, ignore.case = TRUE)]
  fsCols <- fsCols[!grepl("generate_note", fsCols, ignore.case = TRUE)]

  fieldSessionOut <- cbind(
    contextDf(fieldSession),
    fieldSession[, fsCols, drop = FALSE]
  )

  ## ------------------------------------------------------------------ ##
  ## 5. subquad.csv                                                     ##
  ## ------------------------------------------------------------------ ##
  subquadOut <- cbind(
    contextDf(subquadDf),
    subquadDf[, setdiff(names(subquadDf), dropKeys), drop = FALSE]
  )

  ## ------------------------------------------------------------------ ##
  ## 6. coverSubq.csv                                                   ##
  ## ------------------------------------------------------------------ ##
  coverOut <- cbind(
    contextDf(coverDf),
    data.frame(
      coverType = getCol(coverDf, "name_cover"),
      coverArea = getCol(coverDf, "area_cover"),
      stringsAsFactors = FALSE
    )
  )

  ## ------------------------------------------------------------------ ##
  ## 7. trees.csv                                                       ##
  ## ------------------------------------------------------------------ ##
  tr <- treesDf
  nTr <- nrow(tr)

  ## tag_old: "sem info" -> NA
  tagOld <- as.character(getCol(tr, "tag_old"))
  tagOld[grepl("sem info", tagOld, ignore.case = TRUE)] <- NA

  ## dx / dy: subquad base + mapxy offset, only for rows with mapxy.
  ## subquad looks like "<prefix>_<X>x<Y>"; mapxy looks like "x = X; y = Y"
  subqVal  <- as.character(getCol(tr, "subquad"))
  mapxyVal <- as.character(getCol(tr, "mapxy"))

  baseX <- suppressWarnings(as.numeric(sub("^.*_([0-9.]+)x.*$",       "\\1", subqVal)))
  baseY <- suppressWarnings(as.numeric(sub("^.*_[0-9.]+x([0-9.]+).*$", "\\1", subqVal)))
  mapX  <- suppressWarnings(as.numeric(sub("^.*x\\s*=\\s*([-0-9.]+).*$", "\\1", mapxyVal)))
  mapY  <- suppressWarnings(as.numeric(sub("^.*y\\s*=\\s*([-0-9.]+).*$", "\\1", mapxyVal)))

  hasMap <- !is.na(mapxyVal) & nzchar(mapxyVal)
  dx <- rep(NA_real_, nTr)
  dy <- rep(NA_real_, nTr)
  dx[hasMap] <- round(baseX[hasMap] + mapX[hasMap], 1)
  dy[hasMap] <- round(baseY[hasMap] + mapY[hasMap], 1)

  ## pictures: all columns with "picture" in the name, except new_tag_picture
  ## each file name becomes a path inside "media/".
  picCols <- names(tr)[grepl("picture", names(tr), ignore.case = TRUE) &
                         names(tr) != "new_tag_picture"]
  pictures <- if (length(picCols) == 0L) {
    rep(NA_character_, nTr)
  } else {
    apply(tr[, picCols, drop = FALSE], 1, function(r) {
      r <- as.character(r)
      r <- r[!is.na(r) & nzchar(r)]
      if (length(r) == 0L) NA_character_ else
        paste(asMediaPath(r), collapse = "; ")
    })
  }

  ## dbhDif with a single decimal; htDif sourced from ht_dif, also one decimal
  dbhDif <- round1(getCol(tr, "difdbh"))
  htDif  <- round1(getCol(tr, "ht_dif"))

  treesOut <- cbind(
    contextDf(tr),
    data.frame(
      obsType  = getCol(tr, "tree_type"),
      tag      = getCol(tr, "num_tag"),
      tagOK    = getCol(tr, "tag_ok"),
      tagOld   = tagOld,
      tagMap   = getCol(tr, "tree_tag_map"),
      pom      = getCol(tr, "dbh_pom"),
      dbh      = getCol(tr, "dbh_new"),
      ## dbhOld right after dbh
      dbhOld   = getCol(tr, "old_dbh"),
      ## single decimal
      dbhDif   = dbhDif,
      dbhCheck = getCol(tr, "dbh_new_check"),
      dbhObs   = getCol(tr, "dbh_cause"),
      ht       = getCol(tr, "ht_new"),
      ## htOld right after ht
      htOld    = getCol(tr, "old_ht"),
      ## htDif from ht_dif, single decimal
      htDif    = htDif,
      htCheck  = getCol(tr, "ht_new_check"),
      nstem    = getCol(tr, "new_nstem"),
      nstemOld = getCol(tr, "old_nstem"),
      nstemObs = getCol(tr, "nstem_diff"),
      ## confirmId -> idOk
      idOk     = getCol(tr, "info_id"),
      fam      = getCol(tr, "fam_final"),
      species  = getCol(tr, "sp_final"),
      mapOk    = getCol(tr, "tree_map_conf"),
      ## qxy -> dxy
      dxy      = getCol(tr, "map_final"),
      qxyOld   = getCol(tr, "old_xy"),
      dx       = dx,
      dy       = dy,
      ## media path for the new tag picture
      pictureNewTag = asMediaPath(getCol(tr, "new_tag_picture")),
      pictures = pictures,
      stringsAsFactors = FALSE
    )
  )

  ## ------------------------------------------------------------------ ##
  ## 8. idCheck.csv                                                     ##
  ## ------------------------------------------------------------------ ##
  infoId <- as.character(getCol(tr, "info_id"))
  selId  <- is.na(infoId) | infoId != "yes"

  idCheckOut <- cbind(
    contextDf(tr),
    data.frame(
      tag       = getCol(tr, "num_tag"),
      obsType   = getCol(tr, "tree_type"),
      idRecruits = getCol(tr, "recruta_id"),
      famOld    = getCol(tr, "old_fam"),
      spOld     = getCol(tr, "old_sp"),
      famOk     = getCol(tr, "fam_final"),
      spOk      = getCol(tr, "sp_final"),
      famNew    = getCol(tr, "new_fam"),
      genNew    = getCol(tr, "new_gen"),
      spNew     = getCol(tr, "new_sp"),
      detType   = getCol(tr, "det_type"),
      idType    = getCol(tr, "indet_type"),
      toCollect = getCol(tr, "fita_coleta_depois"),
      pictures  = pictures,
      stringsAsFactors = FALSE
    )
  )
  idCheckOut <- idCheckOut[selId, , drop = FALSE]

  ## ------------------------------------------------------------------ ##
  ## 9. missTrees.csv / treesFound.csv                                  ##
  ##    treesNotFound.csv is NOT created any more.                      ##
  ## ------------------------------------------------------------------ ##
  missTrue <- suppressWarnings(as.numeric(getCol(treeNFDf, "miss_true")))
  missConf <- as.character(getCol(treeNFDf, "miss_conf"))

  ## missTrees: miss_true == 1 & miss_conf == "miss"
  selMiss <- !is.na(missTrue) & missTrue == 1 &
    !is.na(missConf) & missConf == "miss"
  missTreesOut <- cbind(
    contextDf(treeNFDf),
    data.frame(
      tag         = getCol(treeNFDf, "miss_num"),
      missConfirm = rep("tree_not_found", nrow(treeNFDf)),
      stringsAsFactors = FALSE
    )
  )
  missTreesOut <- missTreesOut[selMiss, , drop = FALSE]

  ## treesFound: miss_true == 1 & miss_conf != "miss"
  selFound <- !is.na(missTrue) & missTrue == 1 &
    (is.na(missConf) | missConf != "miss")

  tnNames <- names(treeNFDf)
  tnNames <- sub("_miss$", "", tnNames)
  tnNames <- sub("miss$",  "", tnNames)
  tn <- treeNFDf
  names(tn) <- tnNames
  keep <- !grepl("note_name", names(tn))
  tn <- tn[, keep, drop = FALSE]
  tn <- tn[, setdiff(names(tn), dropKeys), drop = FALSE]

  ## media paths inside treesFound as well
  tnPicCols <- names(tn)[grepl("picture", names(tn), ignore.case = TRUE)]
  for (pc in tnPicCols) tn[[pc]] <- asMediaPath(tn[[pc]])

  treesFoundOut <- cbind(contextDf(treeNFDf), tn)
  treesFoundOut <- treesFoundOut[selFound, , drop = FALSE]

  ## ------------------------------------------------------------------ ##
  ## 10. multStem.csv                                                   ##
  ## ------------------------------------------------------------------ ##
  ms <- multStemDf
  dbhMm <- suppressWarnings(as.numeric(getCol(ms, "dbh_mm_sec")))
  dbhCm <- suppressWarnings(as.numeric(getCol(ms, "dbh_cm_sec")))
  papCm <- suppressWarnings(as.numeric(getCol(ms, "pap_cm_sec")))

  dbh <- rep(NA_real_, nrow(ms))
  dbh[!is.na(dbhMm)] <- dbhMm[!is.na(dbhMm)]
  i2 <- is.na(dbh) & !is.na(dbhCm); dbh[i2] <- dbhCm[i2] / 10
  i3 <- is.na(dbh) & !is.na(papCm); dbh[i3] <- papCm[i3] / (pi * 10)
  dbh <- round(dbh, 1)

  multStemOut <- cbind(
    contextDf(ms),
    data.frame(
      tag     = getCol(ms, "num_tag"),
      stem    = getCol(ms, "mult_stem_sec"),
      tagMult = getCol(ms, "tag_plaq_sec"),
      pom     = getCol(ms, "sec_pom"),
      dbh     = dbh,
      ht      = getCol(ms, "sec_ht_new"),
      ## media path
      picture = asMediaPath(getCol(ms, "sec_picture")),
      stringsAsFactors = FALSE
    )
  )

  ## ------------------------------------------------------------------ ##
  ## 11. Assemble the outputs                                           ##
  ##     treesFound / multStem only when they hold records              ##
  ##     treesNotFound is intentionally NOT created                     ##
  ## ------------------------------------------------------------------ ##
  outputs <- list(
    "fieldSession.csv" = fieldSessionOut,
    "subquad.csv"      = subquadOut,
    "coverSubq.csv"    = coverOut,
    "trees.csv"        = treesOut,
    "idCheck.csv"      = idCheckOut,
    "missTrees.csv"    = missTreesOut
  )
  if (nrow(treesFoundOut) > 0L) outputs[["treesFound.csv"]] <- treesFoundOut
  if (nrow(multStemOut)   > 0L) outputs[["multStem.csv"]]   <- multStemOut

  ## ------------------------------------------------------------------ ##
  ## 12. Write the merged files, split by quadrat                       ##
  ##     one directory per quadrat:  expDir/data<quad>/                 ##
  ##     overwrite defaults to FALSE                                    ##
  ##     never silently overwrite; ask before losing records            ##
  ##     merge without data loss and report record counts               ##
  ## ------------------------------------------------------------------ ##

  writeOneWithMerge <- function(df, dirOut, name, overwrite, dedup = FALSE) {
    path <- file.path(dirOut, name)

    oldExists <- file.exists(path)
    old <- if (oldExists) readCsvSafe(path) else data.frame()
    nOld <- nrow(old)

    ## (trees.csv) drop rows already present in the incoming data, so the
    ## file never ends up with duplicated records.
    if (dedup && nrow(df) > 0L) {
      df <- df[!duplicated(df), , drop = FALSE]
    }

    if (oldExists) {
      doMerge <- TRUE
      if (isTRUE(overwrite)) {
        msg <- sprintf(
          paste0("O arquivo '%s' j\u00e1 existe com %d registro(s). ",
                 "Sobrescrever descarta esses %d registro(s). ",
                 "Mesclar, sobrescrever ou pular? [m/o/s]: "),
          path, nOld, nOld)
        ans <- if (interactive()) tolower(trimws(readline(msg))) else "m"
        if (!interactive()) {
          message("  (overwrite = TRUE, sessao nao interativa: mesclando para ",
                  "nao perder os ", nOld, " registro(s) existentes.)")
        }
        if (startsWith(ans, "o")) {
          doMerge <- FALSE
        } else if (startsWith(ans, "s")) {
          message(sprintf("  - %s: mantido (%d registro(s)).", name, nOld))
          return(list(file = name, old = nOld, new = 0L, total = nOld,
                      action = "skip"))
        } else {
          doMerge <- TRUE
        }
      }

      if (!doMerge) {
        utils::write.csv(df, path, row.names = FALSE, na = "")
        message(sprintf("  - %s: sobrescrito (%d -> %d registro(s)).",
                        name, nOld, nrow(df)))
        return(list(file = name, old = nOld, new = nrow(df), total = nrow(df),
                    action = "overwrite"))
      }

      merged <- bindRows(list(old, df))
      if (ncol(merged) == 0L) merged <- df
      ## (trees.csv) remove perfect duplicates coming from old + new
      if (dedup && nrow(merged) > 0L) {
        merged <- merged[!duplicated(merged), , drop = FALSE]
      }
      utils::write.csv(merged, path, row.names = FALSE, na = "")
      nTotal <- nrow(merged)
      nAdded <- max(0L, nTotal - nOld)
      message(sprintf("  - %s: mesclado (%d + %d = %d registro(s)).",
                      name, nOld, nAdded, nTotal))
      return(list(file = name, old = nOld, new = nAdded, total = nTotal,
                  action = "merge"))
    }

    utils::write.csv(df, path, row.names = FALSE, na = "")
    nNew <- nrow(df)
    message(sprintf("  - %s: criado (%d registro(s)).", name, nNew))
    list(file = name, old = 0L, new = nNew, total = nNew, action = "create")
  }

  ## Collect every quadrat present across the outputs
  quads <- unique(unlist(lapply(outputs, function(d) {
    if ("quad" %in% names(d)) as.character(d$quad) else character(0)
  })))
  quads <- sort(unique(stats::na.omit(quads)))
  if (length(quads) == 0L) quads <- NA_character_

  message("Writing merged data:")

  allLogs <- list()

  for (q in quads) {
    ## name = "data" + quadrat (e.g. dataA00). NA/"" -> "data"
    dirName <- if (is.na(q) || !nzchar(q)) "data" else paste0("data", q)
    dirOut  <- file.path(expDir, dirName)
    if (!dir.exists(dirOut)) dir.create(dirOut, recursive = TRUE)

    message(" Directory '", dirName, "':")

    for (nm in names(outputs)) {
      d    <- outputs[[nm]]
      qcol <- if ("quad" %in% names(d)) as.character(d$quad) else
        rep(NA_character_, nrow(d))

      keep <- if (is.na(q) || !nzchar(q)) {
        is.na(qcol) | !nzchar(qcol)
      } else {
        !is.na(qcol) & qcol == q
      }
      dd <- d[keep, , drop = FALSE]

      ## treesFound / multStem only when this quadrat has records
      if (nrow(dd) == 0L && nm %in% c("treesFound.csv", "multStem.csv")) next

      lg <- writeOneWithMerge(dd, dirOut, nm, overwrite,
                              dedup = identical(nm, "trees.csv"))
      lg$quad <- q
      lg$dir  <- dirName
      allLogs[[length(allLogs) + 1L]] <- lg
    }
  }

  ## ---- Final record summary ----------------------------------------- ##
  message("\nRecord summary (how many existed / were added / total):")
  for (lg in allLogs) {
    message(sprintf("  %s/%s: %d existente(s) + %d novo(s) = %d total.",
                    lg$dir, lg$file, lg$old, lg$new, lg$total))
  }

  ## ------------------------------------------------------------------ ##
  ## 13. tagsUsed.csv / tagsRepeat.csv at the root of expDir            ##
  ##                                                                    ##
  ## tagsUsed.csv : quad, subquad and tag of every record in any        ##
  ##                trees.csv inside the data<quad> directories.        ##
  ## tagsRepeat.csv : full trees.csv rows whose 'tag' appears in more   ##
  ##                than one record (even across directories) --- i.e.  ##
  ##                the existing record and the newly added one.        ##
  ## ------------------------------------------------------------------ ##
  treesFiles <- list.files(expDir, pattern = "trees\\.csv$",
                           recursive = TRUE, full.names = TRUE)
  ## keep only those inside the per-quadrat directories (data*)
  if (length(treesFiles) > 0L) {
    dnames <- basename(dirname(treesFiles))
    treesFiles <- treesFiles[startsWith(dnames, "data")]
  }

  treesAll <- data.frame()
  if (length(treesFiles) > 0L) {
    treesList <- lapply(treesFiles, function(tf) {
      dd <- readCsvSafe(tf)
      if (nrow(dd) == 0L) return(NULL)
      if (!"quad"    %in% names(dd)) dd$quad    <- sub("^data", "", basename(dirname(tf)))
      if (!"subquad" %in% names(dd)) dd$subquad <- NA_character_
      if (!"tag"     %in% names(dd)) dd$tag     <- NA_character_
      dd
    })
    treesAll <- bindRows(treesList)
  }

  if (nrow(treesAll) > 0L) {
    ## ---- tagsUsed.csv ------------------------------------------------- ##
    tagsNew <- data.frame(
      quad    = as.character(treesAll$quad),
      subquad = as.character(treesAll$subquad),
      tag     = as.character(treesAll$tag),
      stringsAsFactors = FALSE
    )
    tagsNew <- tagsNew[!is.na(tagsNew$tag) & nzchar(tagsNew$tag), , drop = FALSE]
    tagsNew <- tagsNew[!duplicated(tagsNew), , drop = FALSE]

    tagsUsedPath <- file.path(expDir, "tagsUsed.csv")
    if (file.exists(tagsUsedPath)) {
      oldTags <- readCsvSafe(tagsUsedPath)
      if (all(c("quad", "subquad", "tag") %in% names(oldTags))) {
        oldTags <- oldTags[, c("quad", "subquad", "tag"), drop = FALSE]
        oldTags[] <- lapply(oldTags, as.character)
        tagsNew <- rbind(oldTags, tagsNew)
        tagsNew <- tagsNew[!duplicated(tagsNew), , drop = FALSE]
      }
    }
    utils::write.csv(tagsNew, tagsUsedPath, row.names = FALSE, na = "")
    message(sprintf("\ntagsUsed.csv: %d tag(s) registrada(s).", nrow(tagsNew)))

    ## ---- tagsRepeat.csv ---------------------------------------------- ##
    ## A tag is "repeated" when its value occurs in more than one row of
    ## trees.csv (regardless of directory). Both records are copied.
    tagChr <- as.character(treesAll$tag)
    repSel <- !is.na(tagChr) & nzchar(tagChr) &
      (duplicated(tagChr) | duplicated(tagChr, fromLast = TRUE))
    tagsRepeat <- treesAll[repSel, , drop = FALSE]

    if (nrow(tagsRepeat) > 0L) {
      tagsRepeatPath <- file.path(expDir, "tagsRepeat.csv")
      if (file.exists(tagsRepeatPath)) {
        oldRep <- readCsvSafe(tagsRepeatPath)
        if (nrow(oldRep) > 0L) tagsRepeat <- bindRows(list(oldRep, tagsRepeat))
      }
      tagsRepeat <- tagsRepeat[!duplicated(tagsRepeat), , drop = FALSE]
      utils::write.csv(tagsRepeat, tagsRepeatPath, row.names = FALSE, na = "")
      message(sprintf("tagsRepeat.csv: %d registro(s) com tag repetida.",
                      nrow(tagsRepeat)))
    } else {
      message("tagsRepeat.csv: nenhuma tag repetida.")
    }
  }

  ## ------------------------------------------------------------------ ##
  ## 16. Flatten every "media" folder into expDir/media                 ##
  ## ------------------------------------------------------------------ ##
  candMedia <- c(file.path(mediaDir, "media"), file.path(csvDir, "media"))
  mediaSrc <- NULL
  for (cm in candMedia) if (dir.exists(cm)) { mediaSrc <- cm; break }

  if (!is.null(mediaSrc)) {
    mfiles <- list.files(mediaSrc, recursive = TRUE, full.names = TRUE)
    mfiles <- mfiles[!dir.exists(mfiles)]
    for (mf in mfiles) {
      file.copy(mf, file.path(mediaDst, basename(mf)), overwrite = overwrite)
    }
  }

  invisible(expDir)
}
