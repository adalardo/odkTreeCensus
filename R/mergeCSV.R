#' Merge and tidy the CSV files exported by readInstancesXML
#'
#' Reads the CSV files produced by \code{\link{readInstancesXML}}, links the
#' records through the \code{KEY} / \code{PARENT_KEY} columns, repeats the
#' parent fields at the child level and writes a set of flat CSV files ---
#' one row per lowest level record --- inside \code{expDir/rawData}.
#'
#' Media files that live under a \code{media} folder (possibly with
#' sub-directories) are copied, flattened, into \code{expDir/media}.
#'
#' @param csvDir Character. Directory that contains the CSV files exported by
#'   \code{readInstancesXML}.
#' @param expDir Character. Export directory where the merged files and the
#'   flattened \code{media} folder will be written. Defaults to \code{csvDir}.
#' @param mediaDir Character. Directory that contains the \code{media} folder
#'   with sub-directories. Defaults to \code{csvDir}.
#' @param overwrite Logical. Overwrite existing output files. Default
#'   \code{TRUE}.
#'
#' @return Invisibly, the path to the \code{rawData} directory that was
#'   created.
#' @export
mergeCSV <- function(csvDir, expDir = NULL, mediaDir = NULL, overwrite = TRUE) {

  if (is.null(expDir))   expDir   <- csvDir
  if (is.null(mediaDir)) mediaDir <- csvDir

  if (!dir.exists(csvDir)) {
    stop("The directory 'csvDir' does not exist: ", csvDir)
  }
  if (!dir.exists(expDir)) {
    dir.create(expDir, recursive = TRUE)
  }
  rawDataDir <- file.path(expDir, "rawData")
  if (!dir.exists(rawDataDir)) {
    dir.create(rawDataDir, recursive = TRUE)
  }

  ## ------------------------------------------------------------------ ##
  ## Helpers                                                            ##
  ## ------------------------------------------------------------------ ##

  ## Final variable name: keep only the part after the last "."
  tidyName <- function(x) sub("^.*\\.", "", x)

  ## TRUE when the CSV file has no usable content (empty or only blank lines)
  isEmptyCsv <- function(path) {
    ## 0 bytes: clearly empty
    sz <- suppressWarnings(file.info(path)$size)
    if (!is.na(sz) && sz == 0) return(TRUE)

    ## Any non-blank line? (header alone counts as content, but a file with
    ## only blank lines would still make read.csv fail)
    lines <- suppressWarnings(readLines(path, warn = FALSE))
    if (length(lines) == 0L) return(TRUE)
    !any(nzchar(trimws(lines)))
  }

  readOne <- function(f) {
    path <- file.path(csvDir, f)

    ## Skip files without records/header to avoid
    ## "primeiras cinco linhas estão vazias: desistindo"
    if (isEmptyCsv(path)) {
      return(data.frame())
    }

    d <- utils::read.csv(path,
                         stringsAsFactors = FALSE,
                         check.names      = FALSE)
    names(d) <- tidyName(names(d))
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

  ## ------------------------------------------------------------------ ##
  ## 1. Read every CSV and tidy the column names                        ##
  ## ------------------------------------------------------------------ ##
  csvFiles <- list.files(csvDir, pattern = "\\.[Cc][Ss][Vv]$", full.names = FALSE)
  if (length(csvFiles) == 0L) {
    stop("No CSV files were found in 'csvDir': ", csvDir)
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
  ## ------------------------------------------------------------------ ##
  fieldSessionOut <- cbind(
    contextDf(fieldSession),
    fieldSession[, setdiff(names(fieldSession), dropKeys), drop = FALSE]
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

  ## dx / dy: subquad base + mapxy offset (only when mapxy is filled)
  subqVal  <- as.character(getCol(tr, "subquad"))
  mapxyVal <- as.character(getCol(tr, "mapxy"))

  baseX <- suppressWarnings(as.numeric(sub("^[^_]*_([0-9.]+)x.*$",        "\\1", subqVal)))
  baseY <- suppressWarnings(as.numeric(sub("^[^_]*_[0-9.]+x([0-9.]+)$",    "\\1", subqVal)))
  mapX  <- suppressWarnings(as.numeric(sub("^.*x\\s*=\\s*([0-9.]+).*$",    "\\1", mapxyVal)))
  mapY  <- suppressWarnings(as.numeric(sub("^.*y\\s*=\\s*([0-9.]+).*$",    "\\1", mapxyVal)))

  hasMap <- !is.na(mapxyVal) & nzchar(mapxyVal)
  dx <- rep(NA_real_, nTr)
  dy <- rep(NA_real_, nTr)
  dx[hasMap] <- round(baseX[hasMap] + mapX[hasMap], 1)
  dy[hasMap] <- round(baseY[hasMap] + mapY[hasMap], 1)

  ## pictures: all columns with "picture" in the name, except new_tag_picture
  picCols <- names(tr)[grepl("picture", names(tr), ignore.case = TRUE) &
                         names(tr) != "new_tag_picture"]
  pictures <- if (length(picCols) == 0L) {
    rep(NA_character_, nTr)
  } else {
    apply(tr[, picCols, drop = FALSE], 1, function(r) {
      r <- as.character(r)
      r <- r[!is.na(r) & nzchar(r)]
      if (length(r) == 0L) NA_character_ else paste(r, collapse = "; ")
    })
  }

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
      dbhDif   = getCol(tr, "difdbh"),
      dbhCheck = getCol(tr, "dbh_new_check"),
      dbhObs   = getCol(tr, "dbh_cause"),
      ht       = getCol(tr, "ht_new"),
      htDif    = getCol(tr, "dif_ht"),
      htCheck  = getCol(tr, "ht_new_check"),
      nstem    = getCol(tr, "new_nstem"),
      nstemOld = getCol(tr, "old_nstem"),
      nstemObs = getCol(tr, "nstem_diff"),
      confirmId = getCol(tr, "info_id"),
      fam      = getCol(tr, "fam_final"),
      species  = getCol(tr, "sp_final"),
      mapOk    = getCol(tr, "tree_map_conf"),
      qxy      = getCol(tr, "map_final"),
      qxyOld   = getCol(tr, "old_xy"),
      dx       = dx,
      dy       = dy,
      pictureNewTag = getCol(tr, "new_tag_picture"),
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
  ## 9. treesNotFound.csv / missTrees.csv / treesFound.csv              ##
  ## ------------------------------------------------------------------ ##
  missTrue <- suppressWarnings(as.numeric(getCol(treeNFDf, "miss_true")))
  missConf <- as.character(getCol(treeNFDf, "miss_conf"))

  treeNFOut <- cbind(
    contextDf(treeNFDf),
    treeNFDf[, setdiff(names(treeNFDf), dropKeys), drop = FALSE]
  )

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
      picture = getCol(ms, "sec_picture"),
      stringsAsFactors = FALSE
    )
  )

  ## ------------------------------------------------------------------ ##
  ## 11. Write the merged files                                         ##
  ## ------------------------------------------------------------------ ##
  writeOut <- function(df, name) {
    utils::write.csv(df,
                     file.path(rawDataDir, name),
                     row.names = FALSE,
                     na      = "")
  }

  writeOut(fieldSessionOut, "fieldSession.csv")
  writeOut(subquadOut,      "subquad.csv")
  writeOut(coverOut,        "coverSubq.csv")
  writeOut(treesOut,        "trees.csv")
  writeOut(treeNFOut,       "treesNotFound.csv")
  writeOut(idCheckOut,      "idCheck.csv")
  writeOut(missTreesOut,    "missTrees.csv")
  writeOut(treesFoundOut,   "treesFound.csv")
  writeOut(multStemOut,     "multStem.csv")

  ## ------------------------------------------------------------------ ##
  ## 12. Flatten the "media" folder into expDir/media                   ##
  ## ------------------------------------------------------------------ ##
  candMedia <- c(file.path(mediaDir, "media"), file.path(csvDir, "media"))
  mediaSrc <- NULL
  for (cm in candMedia) if (dir.exists(cm)) { mediaSrc <- cm; break }

  if (!is.null(mediaSrc)) {
    mediaDst <- file.path(expDir, "media")
    if (!dir.exists(mediaDst)) dir.create(mediaDst, recursive = TRUE)
    mfiles <- list.files(mediaSrc, recursive = TRUE, full.names = TRUE)
    mfiles <- mfiles[!dir.exists(mfiles)]
    for (mf in mfiles) {
      file.copy(mf, file.path(mediaDst, basename(mf)), overwrite = overwrite)
    }
  }

  invisible(rawDataDir)
}
