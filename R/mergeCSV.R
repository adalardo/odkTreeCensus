#' Merge and tidy the CSV files exported by readInstancesXML
#'
#' Reads the CSV files produced by \code{\link{readInstancesXML}}, links the
#' records through the \code{KEY} / \code{PARENT_KEY} columns, repeats the
#' parent fields at the child level and writes a set of flat CSV files ---
#' one row per lowest level record.
#'
#' Output is split by quadrat: each quadrat gets its own sub-directory inside
#' \code{expDir}, named \code{data<quadrat>} (e.g. \code{dataA00}). When the
#' destination file already exists, the existing records are kept and only the
#' new records that are not already present are added (never overwrites and
#' never duplicates). A summary of how many records were found and how many
#' were added is printed for every file.
#'
#' No file inside \code{expDir} is ever overwritten: files that already exist
#' are read, compared with the incoming data and only the rows that are not
#' already present are appended.
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
#' No output file is allowed to hold duplicated records: a new row identical
#' (in all fields) to an existing one is not included again.
#'
#' Media files that live under a \code{media} folder (possibly with
#' sub-directories) are copied, flattened, into \code{expDir/media}; file
#' names stored in the data are rewritten as \code{file.path("media", "x.jpg")}
#' so they can be opened directly from a spreadsheet. Files that already exist
#' in the destination are not overwritten.
#'
#' @param csvDir Character. Directory that contains the CSV files exported by
#'   \code{readInstancesXML}.
#' @param expDir Character. Export directory. Defaults to \code{csvDir}.
#'
#' @return Invisibly, the path to \code{expDir}.
#' @export
mergeCSV <- function(csvDir, expDir = NULL) {

  if (is.null(expDir))   expDir <- getwd()
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

  ## Helpers used below live in R/helperFunctions.R (section 3) with the
  ## "." prefix; everything they used to capture from this function's
  ## environment (csvDir, csvFiles, dataList, keyEnv) is now passed as a
  ## parameter, so other functions can reuse them.

  ## ------------------------------------------------------------------ ##
  ## 1. Read every CSV and tidy the column names                        ##
  ## ------------------------------------------------------------------ ##
  csvFiles <- list.files(csvDir, pattern = "\\.[Cc][Ss][Vv]$", full.names = FALSE)
  if (length(csvFiles) == 0L) {
    stop("No CSV files were found in 'csvDir': ", csvDir)
  }

  ## 1a. Control files written by mergeCSV itself must never be treated
  ##     as data (filesDataMerged.csv / legacy tabsDataMerged.csv).
  isControl <- grepl("^(filesDataMerged|tabsDataMerged)\\.csv$",
                     csvFiles, ignore.case = TRUE)
  if (any(isControl)) {
    message("Skipping ", sum(isControl),
            " mergeCSV control file(s): ",
            paste(csvFiles[isControl], collapse = ", "))
    csvFiles <- csvFiles[!isControl]
  }

  ## 1b. Drop files whose base name was already merged in a previous run,
  ##     so nothing is read/processed twice. The list lives at the root
  ##     of expDir (filesDataMerged.csv).
  filesMergedPath <- file.path(expDir, "filesDataMerged.csv")
  mergedBase      <- .readMergedBaseNames(filesMergedPath)

  if (length(mergedBase) > 0L && length(csvFiles) > 0L) {
    alreadyMerged <- .baseName(csvFiles) %in% mergedBase
    if (any(alreadyMerged)) {
      message("Skipping ", sum(alreadyMerged),
              " file(s) already merged in a previous run: ",
              paste(csvFiles[alreadyMerged], collapse = ", "))
      csvFiles <- csvFiles[!alreadyMerged]
    }
  }

  if (length(csvFiles) == 0L) {
    message("No new CSV file to merge.")
    return(invisible(expDir))
  }

  ## Drop empty files (0 bytes, BOM-only or only blank lines) before reading.
  fileEmpty <- vapply(csvFiles,
                      function(f) .isEmptyCsvFile(file.path(csvDir, f)),
                      logical(1))
  if (any(fileEmpty)) {
    message("Skipping ", sum(fileEmpty), " empty CSV file(s): ",
            paste(csvFiles[fileEmpty], collapse = ", "))
  }
  csvFiles <- csvFiles[!fileEmpty]

  if (length(csvFiles) == 0L) {
    return(invisible(expDir))
  }

  dataList <- lapply(csvFiles, .readOne, csvDir = csvDir)
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

  ## ------------------------------------------------------------------ ##
  ## 3. Group the CSV files                                             ##
  ## ------------------------------------------------------------------ ##
  filesNoExt <- sub("\\.[Cc][Ss][Vv]$", "", csvFiles)

  rootF   <- csvFiles[!grepl("-", filesNoExt)]             # roots (no "-")
  subqF   <- .pick("-subquad\\.csv$",                     csvFiles)  # *-subquad.csv
  coverF  <- .pick("-subquad-cobertura-rep_cover\\.csv$", csvFiles)  # cobertura
  treeF   <- .pick("-subquad-tree\\.csv$",                csvFiles)  # trees
  treeNFF <- .pick("-subquad-rep_miss\\.csv$",            csvFiles)  # not found
  stemF   <- .pick("sec_stem\\.csv$",                     csvFiles)  # multStem (main)
  stemMF  <- .pick("sec_stem_miss\\.csv$",                csvFiles)  # multStem (missing)

  fieldSession <- .bindRows(lapply(rootF,   .getOne, dataList = dataList))
  subquadDf    <- .bindRows(lapply(subqF,   .getOne, dataList = dataList))
  coverDf      <- .bindRows(lapply(coverF,  .getOne, dataList = dataList))
  treesDf      <- .bindRows(lapply(treeF,   .getOne, dataList = dataList))
  treeNFDf     <- .bindRows(lapply(treeNFF, .getOne, dataList = dataList))

  dropKeys <- c("KEY", "PARENT_KEY")

  ## ------------------------------------------------------------------ ##
  ## 3b. multStem: prepare both sources and 'tagOld'                    ##
  ##                                                                    ##
  ## multStem gathers secondary stems from two independent XForm        ##
  ## branches (both direct children of 'subquad'):                      ##
  ##   - 'subquad/tree/sec_stem'          (census trees)                 ##
  ##   - 'subquad/rep_miss/sec_stem_miss' (found trees)                  ##
  ##                                                                    ##
  ## 'tagOld' is the OLD tag of the parent tree, so it is resolved      ##
  ## from each branch's PARENT repeat BEFORE stacking:                  ##
  ##   * 'tree'     -> 'tag_old'  (empty / "sem info" -> NA)            ##
  ##   * 'rep_miss' -> 'miss_num'                                       ##
  ##                                                                    ##
  ## Resolving first avoids an auxiliary source column (impossible to   ##
  ## create for a 0 x 0 file) and prevents cross-branch contamination,  ##
  ## since the PARENT_KEY chain never crosses branches.                 ##
  ## ------------------------------------------------------------------ ##
  prepStem <- function(f, field) {
    d   <- dataList[[f]]
    out <- d
    ## 'rep_miss' suffixes columns with "_miss"; unify the names
    names(out) <- sub("_miss$", "", names(out))

    ## 'tag_old' (tree branch) or 'miss_num' (rep_miss branch) from ancestor
    raw <- as.character(.fieldSelfOrAncestor(d, field, keyEnv))

    ## empty or "sem info" -> NA
    raw <- trimws(raw)
    raw[!is.na(raw) & grepl("sem info", raw, ignore.case = TRUE)] <- NA_character_
    raw[!is.na(raw) & !nzchar(raw)] <- NA_character_

    ## numeric; non-numeric becomes NA (warned below)
    num <- suppressWarnings(as.numeric(raw))

    out$tagOld <- num
    list(df = out, raw = raw, num = num)
  }

  stemTreeL <- lapply(stemF,  prepStem, field = "tag_old")   # repeat 'tree'
  stemMissL <- lapply(stemMF, prepStem, field = "miss_num")  # repeat 'rep_miss'

  ## ---- warnings ----------------------------------------------------- ##
  rawAll <- c(unlist(lapply(stemTreeL, `[[`, "raw"), use.names = FALSE),
              unlist(lapply(stemMissL, `[[`, "raw"), use.names = FALSE))
  numAll <- c(unlist(lapply(stemTreeL, `[[`, "num"), use.names = FALSE),
              unlist(lapply(stemMissL, `[[`, "num"), use.names = FALSE))

  badVals <- unique(rawAll[!is.na(rawAll) & is.na(numAll)])
  if (length(badVals) > 0L) {
    warning(sprintf(
      paste0("multStem: valor(es) de 'tagOld' nao numerico(s), ",
             "convertido(s) para NA: %s"),
      paste(badVals, collapse = ", ")),
      call. = FALSE)
  }

  ## the same tree number may appear in both sources; both records are
  ## kept and a warning asks for review
  tagOldTreeVec <- unlist(lapply(stemTreeL, `[[`, "num"), use.names = FALSE)
  tagOldMissVec <- unlist(lapply(stemMissL, `[[`, "num"), use.names = FALSE)
  dupBoth <- intersect(unique(tagOldTreeVec[!is.na(tagOldTreeVec)]),
                       unique(tagOldMissVec[!is.na(tagOldMissVec)]))
  if (length(dupBoth) > 0L) {
    warning(sprintf(
      paste0("multStem: valor(es) de 'tagOld' presente(s) em 'tree' e em ",
             "'rep_miss': %s. Os dois registros foram mantidos; ",
             "confira se ha duplicacao."),
      paste(sort(dupBoth), collapse = ", ")),
      call. = FALSE)
  }

  ## stack only after resolving 'tagOld' per source
  multStemDf <- .bindRows(c(lapply(stemTreeL, `[[`, "df"),
                            lapply(stemMissL, `[[`, "df")))

  ## ------------------------------------------------------------------ ##
  ## 4. fieldSession.csv                                                ##
  ##    drop 'instanceID' and any '*generate_note*' column              ##
  ## ------------------------------------------------------------------ ##
  fsCols <- setdiff(names(fieldSession), dropKeys)
  fsCols <- fsCols[!grepl("instanceID",    fsCols, ignore.case = TRUE)]
  fsCols <- fsCols[!grepl("generate_note", fsCols, ignore.case = TRUE)]

  fieldSessionOut <- cbind(
    .contextDf(fieldSession, keyEnv),
    fieldSession[, fsCols, drop = FALSE]
  )

  ## ------------------------------------------------------------------ ##
  ## 5. subquad.csv                                                     ##
  ## ------------------------------------------------------------------ ##
  subquadOut <- cbind(
    .contextDf(subquadDf, keyEnv),
    subquadDf[, setdiff(names(subquadDf), dropKeys), drop = FALSE]
  )

  ## ------------------------------------------------------------------ ##
  ## 6. coverSubq.csv                                                   ##
  ## ------------------------------------------------------------------ ##
  coverOut <- cbind(
    .contextDf(coverDf, keyEnv),
    data.frame(
      coverType = .getCol(coverDf, "name_cover"),
      coverArea = .getCol(coverDf, "area_cover"),
      stringsAsFactors = FALSE
    )
  )

  ## ------------------------------------------------------------------ ##
  ## 7. trees.csv                                                       ##
  ## ------------------------------------------------------------------ ##
  tr <- treesDf
  nTr <- nrow(tr)

  ## tag_old: "sem info" -> NA
  tagOld <- as.character(.getCol(tr, "tag_old"))
  tagOld[grepl("sem info", tagOld, ignore.case = TRUE)] <- NA

  ## dx / dy: subquad base + mapxy offset, only for rows with mapxy.
  ## subquad looks like "<prefix>_<X>x<Y>"; mapxy looks like "x = X; y = Y"
  ##
  ## Note: both 'sel_subquad' and 'mapxy' may live only on the parent
  ## record (subquad repeat), not on the tree repeat. We therefore use
  ## .fieldSelfOrAncestor (self or ancestor) rather than .getCol (which
  ## only sees the own record and returned NA, zeroing dx/dy).
  subqVal  <- as.character(.fieldSelfOrAncestor(tr, "sel_subquad", keyEnv))
  mapxyVal <- as.character(.fieldSelfOrAncestor(tr, "mapxy",       keyEnv))

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
        paste(.asMediaPath(r), collapse = "; ")
    })
  }

  ## dbhDif with a single decimal; htDif sourced from ht_dif, also one decimal
  dbhDif <- .round1(.getCol(tr, "difdbh"))
  htDif  <- .round1(.getCol(tr, "ht_dif"))

  treesOut <- cbind(
    .contextDf(tr, keyEnv),
    data.frame(
      obsType  = .getCol(tr, "tree_type"),
      tag      = .getCol(tr, "num_tag"),
      tagOK    = .getCol(tr, "tag_ok"),
      tagOld   = tagOld,
      tagMap   = .getCol(tr, "tree_tag_map"),
      pom      = .getCol(tr, "dbh_pom"),
      dbh      = .getCol(tr, "dbh_new"),
      ## dbhOld right after dbh
      dbhOld   = .getCol(tr, "old_dbh"),
      ## single decimal
      dbhDif   = dbhDif,
      dbhCheck = .getCol(tr, "dbh_new_check"),
      dbhObs   = .getCol(tr, "dbh_cause"),
      ht       = .getCol(tr, "ht_new"),
      ## htOld right after ht
      htOld    = .getCol(tr, "old_ht"),
      ## htDif from ht_dif, single decimal
      htDif    = htDif,
      htCheck  = .getCol(tr, "ht_new_check"),
      nstem    = .getCol(tr, "new_nstem"),
      nstemOld = .getCol(tr, "old_nstem"),
      nstemObs = .getCol(tr, "nstem_diff"),
      ## confirmId -> idOk
      idOk     = .getCol(tr, "info_id"),
      fam      = .getCol(tr, "fam_final"),
      species  = .getCol(tr, "sp_final"),
      mapOk    = .getCol(tr, "tree_map_conf"),
      qxy      = .getCol(tr, "map_final"),
      qxyOld   = .getCol(tr, "old_xy"),
      dx       = dx,
      dy       = dy,
      ## media path for the new tag picture
      pictureNewTag = .asMediaPath(.getCol(tr, "new_tag_picture")),
      pictures = pictures,
      stringsAsFactors = FALSE
    )
  )

  ## ------------------------------------------------------------------ ##
  ## 8. idCheck.csv                                                     ##
  ## ------------------------------------------------------------------ ##
  infoId <- as.character(.getCol(tr, "info_id"))
  selId  <- is.na(infoId) | infoId != "yes"

  idCheckOut <- cbind(
    .contextDf(tr, keyEnv),
    data.frame(
      tag       = .getCol(tr, "num_tag"),
      obsType   = .getCol(tr, "tree_type"),
      idRecruits = .getCol(tr, "recruta_id"),
      famOld    = .getCol(tr, "old_fam"),
      spOld     = .getCol(tr, "old_sp"),
      famOk     = .getCol(tr, "fam_final"),
      spOk      = .getCol(tr, "sp_final"),
      famNew    = .getCol(tr, "new_fam"),
      genNew    = .getCol(tr, "new_gen"),
      spNew     = .getCol(tr, "new_sp"),
      detType   = .getCol(tr, "det_type"),
      idType    = .getCol(tr, "indet_type"),
      toCollect = .getCol(tr, "fita_coleta_depois"),
      pictures  = pictures,
      stringsAsFactors = FALSE
    )
  )
  idCheckOut <- idCheckOut[selId, , drop = FALSE]

  ## ------------------------------------------------------------------ ##
  ## 9. missTrees.csv / treesFound.csv                                  ##
  ##    treesNotFound.csv is NOT created any more.                      ##
  ## ------------------------------------------------------------------ ##
  missTrue <- suppressWarnings(as.numeric(.getCol(treeNFDf, "miss_true")))
  missConf <- as.character(.getCol(treeNFDf, "miss_conf"))

  ## missTrees: miss_true == 1 & miss_conf == "miss"
  selMiss <- !is.na(missTrue) & missTrue == 1 &
    !is.na(missConf) & missConf == "miss"
  missTreesOut <- cbind(
    .contextDf(treeNFDf, keyEnv),
    data.frame(
      tag         = .getCol(treeNFDf, "miss_num"),
      missConfirm = rep("tree_not_found", nrow(treeNFDf)),
      stringsAsFactors = FALSE
    )
  )
  missTreesOut <- missTreesOut[selMiss, , drop = FALSE]

  ## treesFound: miss_true == 1 & miss_conf != "miss"
  selFound <- !is.na(missTrue) & missTrue == 1 &
    (is.na(missConf) | missConf != "miss")

  ## ------------------------------------------------------------------ ##
  ## 9b. treesFound.csv                                                 ##
  ##     Same columns/names as trees.csv. The source is the 'rep_miss'  ##
  ##     repeat, whose columns carry the "_miss" suffix; that suffix    ##
  ##     is stripped below so that field names match those used when    ##
  ##     building trees.csv.                                            ##
  ##                                                                    ##
  ##     Source differences relative to trees.csv (same output name,    ##
  ##     different source field):                                       ##
  ##       obsType = "plaqueada" / "plaq_dead"  (from 'miss_alive')     ##
  ##       tag     = 'miss_num' overwritten by 'new_tag' when present   ##
  ##       tagOld  = 'miss_num'                                         ##
  ##       tagMap  = 'miss_tag_key' ("tag_<n>.1" -> <n>)                ##
  ##       fam     = 'new_fam' ; species = 'new_sp'                     ##
  ##       mapOk   = 'tagmap_conf' ; qxy = 'mapxy'                      ##
  ## ------------------------------------------------------------------ ##
  tnNames <- names(treeNFDf)
  tnNames <- sub("_miss$", "", tnNames)
  tnNames <- sub("miss$",  "", tnNames)
  tn <- treeNFDf
  names(tn) <- tnNames
  keep <- !grepl("note_name", names(tn))
  tn <- tn[, keep, drop = FALSE]

  ## 'KEY'/'PARENT_KEY' are not dropped here: .fieldSelfOrAncestor
  ## needs 'PARENT_KEY'. Those columns simply do not enter the final
  ## treesFoundOut assembly.

  nTf <- nrow(tn)

  ## context (also reused for the idCheck rows below)
  ctxF <- .contextDf(tn, keyEnv)

  ## ---- obsType ----------------------------------------------------- ##
  ## alive ("yes") -> "plaqueada"; 'miss_alive' missing -> "plaq_dead"
  missAlive <- as.character(.getCol(tn, "miss_alive"))
  obsTypeF  <- rep(NA_character_, nTf)
  obsTypeF[!is.na(missAlive) & missAlive == "yes"] <- "plaqueada"
  obsTypeF[is.na(missAlive)]                       <- "plaq_dead"

  ## ---- tag / tagOld ------------------------------------------------ ##
  ## tag = miss_num, overwritten by new_tag when present;
  ## tagOld = miss_num (old tag)
  tagNumF <- as.character(.getCol(tn, "miss_num"))
  tagNewF <- as.character(.getCol(tn, "new_tag"))
  tagF    <- tagNumF
  hasNewF <- !is.na(tagNewF) & nzchar(tagNewF)
  tagF[hasNewF] <- tagNewF[hasNewF]

  ## ---- tagMap from miss_tag_key ("tag_<n>.1" -> <n>) --------------- ##
  tagKeyF <- as.character(.getCol(tn, "miss_tag_key"))
  tagMapF <- suppressWarnings(as.integer(gsub("tag_|\\.1", "", tagKeyF)))

  ## ---- dx / dy: subquad base + mapxy offset (same as trees) --------- ##
  subqValF  <- as.character(ctxF$subquad)
  mapxyValF <- as.character(.fieldSelfOrAncestor(tn, "mapxy", keyEnv))

  baseXF <- suppressWarnings(as.numeric(sub("^.*_([0-9.]+)x.*$",       "\\1", subqValF)))
  baseYF <- suppressWarnings(as.numeric(sub("^.*_[0-9.]+x([0-9.]+).*$", "\\1", subqValF)))
  mapXF  <- suppressWarnings(as.numeric(sub("^.*x\\s*=\\s*([-0-9.]+).*$", "\\1", mapxyValF)))
  mapYF  <- suppressWarnings(as.numeric(sub("^.*y\\s*=\\s*([-0-9.]+).*$", "\\1", mapxyValF)))

  hasMapF <- !is.na(mapxyValF) & nzchar(mapxyValF)
  dxF <- rep(NA_real_, nTf)
  dyF <- rep(NA_real_, nTf)
  dxF[hasMapF] <- round(baseXF[hasMapF] + mapXF[hasMapF], 1)
  dyF[hasMapF] <- round(baseYF[hasMapF] + mapYF[hasMapF], 1)

  ## ---- pictures (every *picture* column, except the tag photo) ------ ##
  picColsF <- names(tn)[grepl("picture", names(tn), ignore.case = TRUE) &
                          !grepl("tag_picture", names(tn), ignore.case = TRUE)]
  picturesF <- if (nTf == 0L) {
    character(0)
  } else if (length(picColsF) == 0L) {
    rep(NA_character_, nTf)
  } else {
    apply(tn[, picColsF, drop = FALSE], 1, function(r) {
      r <- as.character(r)
      r <- r[!is.na(r) & nzchar(r)]
      if (length(r) == 0L) NA_character_ else
        paste(.asMediaPath(r), collapse = "; ")
    })
  }

  treesFoundOut <- cbind(
    ctxF,
    data.frame(
      obsType  = obsTypeF,
      tag      = tagF,
      tagOK    = .getCol(tn, "tag_ok"),
      tagOld   = tagNumF,
      tagMap   = tagMapF,
      pom      = .getCol(tn, "dbh_pom"),
      dbh      = .getCol(tn, "dbh_new"),
      dbhOld   = .getCol(tn, "old_dbh"),
      dbhDif   = .round1(.getCol(tn, "difdbh")),
      dbhCheck = .getCol(tn, "dbh_new_check"),
      dbhObs   = .getCol(tn, "dbh_cause"),
      ht       = .getCol(tn, "ht_new"),
      htOld    = .getCol(tn, "old_ht"),
      htDif    = .round1(.getCol(tn, "difht")),
      htCheck  = .getCol(tn, "ht_new_check"),
      nstem    = .getCol(tn, "new_nstem"),
      nstemOld = .getCol(tn, "old_nstem"),
      nstemObs = .getCol(tn, "nstem_diff"),
      idOk     = .getCol(tn, "info_id"),
      fam      = .getCol(tn, "new_fam"),
      species  = .getCol(tn, "new_sp"),
      mapOk    = .getCol(tn, "tagmap_conf"),
      qxy      = .getCol(tn, "mapxy"),
      qxyOld   = .getCol(tn, "old_xy"),
      dx       = dxF,
      dy       = dyF,
      pictureNewTag = .asMediaPath(.getCol(tn, "new_tag_picture")),
      pictures = picturesF,
      stringsAsFactors = FALSE
    )
  )
  treesFoundOut <- treesFoundOut[selFound, , drop = FALSE]

  ## ------------------------------------------------------------------ ##
  ## 9c. idCheck.csv also receives the treesFound.csv records that     ##
  ##     need identification: 'info_id' != "yes" and                   ##
  ##     'obsType' == "plaqueada". Only columns already present in     ##
  ##     idCheck.csv are filled (no new column is created).            ##
  ## ------------------------------------------------------------------ ##
  idOkF <- as.character(.getCol(tn, "info_id"))
  condIdFound <- selFound &
    (is.na(idOkF) | idOkF != "yes") &
    !is.na(obsTypeF) & obsTypeF == "plaqueada"

  if (any(condIdFound)) {
    tfId <- tn[condIdFound, , drop = FALSE]
    nIdF <- nrow(tfId)

    idFoundOut <- cbind(
      ctxF[condIdFound, , drop = FALSE],
      data.frame(
        tag        = tagF[condIdFound],
        obsType    = obsTypeF[condIdFound],
        idRecruits = rep(NA_character_, nIdF),
        famOld     = .getCol(tfId, "old_fam"),
        spOld      = .getCol(tfId, "old_sp"),
        famOk      = rep(NA_character_, nIdF),
        spOk       = rep(NA_character_, nIdF),
        famNew     = .getCol(tfId, "new_fam"),
        genNew     = .getCol(tfId, "new_gen"),
        spNew      = .getCol(tfId, "new_sp"),
        detType    = .getCol(tfId, "det_type"),
        idType     = .getCol(tfId, "indet_type"),
        toCollect  = .getCol(tfId, "fita_coleta_depois"),
        pictures   = picturesF[condIdFound],
        stringsAsFactors = FALSE
      )
    )

    idCheckOut <- .bindRows(list(idCheckOut, idFoundOut))
  }

  ## ------------------------------------------------------------------ ##
  ## 10. multStem.csv                                                   ##
  ##                                                                    ##
  ## Secondary stems from the two XForm branches (census trees:         ##
  ## 'subquad/tree/sec_stem'; found trees:                              ##
  ## 'subquad/rep_miss/sec_stem_miss'). Column names were already       ##
  ## unified in step 3b. Note:                                          ##
  ##   * the stem sequential number is 'mult_stem_seq' (after the       ##
  ##     "_miss" strip), never 'mult_stem_sec'.                         ##
  ##   * the parent tree tag is not in 'sec_stem'/'sec_stem_miss': it   ##
  ##     comes from ancestor 'num_tag' (tree) or 'miss_num'             ##
  ##     (rep_miss). A re-tagged found tree stores the new number in    ##
  ##     ancestor 'miss_new_tag.new_tag_miss', which replaces           ##
  ##     'miss_num' in 'tag' while 'tagOld' keeps the old tag.          ##
  ##                                                                    ##
  ## 'tagOld' was already resolved in step 3b, per source.              ##
  ## ------------------------------------------------------------------ ##
  ms <- multStemDf

  ## dbh (mm -> cm -> pap)
  dbhMm <- suppressWarnings(as.numeric(.getCol(ms, "dbh_mm_sec")))
  dbhCm <- suppressWarnings(as.numeric(.getCol(ms, "dbh_cm_sec")))
  papCm <- suppressWarnings(as.numeric(.getCol(ms, "pap_cm_sec")))

  dbh <- rep(NA_real_, nrow(ms))
  dbh[!is.na(dbhMm)] <- dbhMm[!is.na(dbhMm)]
  i2 <- is.na(dbh) & !is.na(dbhCm); dbh[i2] <- dbhCm[i2] / 10
  i3 <- is.na(dbh) & !is.na(papCm); dbh[i3] <- papCm[i3] / (pi * 10)
  dbh <- round(dbh, 1)

  ## ------- tag ------------------------------------------------------ ##
  ## sec_stem      -> ancestor 'num_tag'  (repeat 'tree')
  ## sec_stem_miss -> ancestor 'miss_num' (repeat 'rep_miss')
  tagNum  <- as.character(.fieldSelfOrAncestor(ms, "num_tag",  keyEnv))
  tagMiss <- as.character(.fieldSelfOrAncestor(ms, "miss_num", keyEnv))
  tagNew  <- as.character(.fieldSelfOrAncestor(ms, "miss_new_tag.new_tag_miss",
                                               keyEnv))

  ## tag base: 'num_tag' when present, otherwise 'miss_num'
  tagBase <- tagNum
  fill    <- is.na(tagBase) | !nzchar(tagBase)
  tagBase[fill] <- tagMiss[fill]
  tagBase[is.na(tagBase)] <- ""

  ## tag: same as tagBase, but replaced by 'new_tag_miss' when present
  tag    <- tagBase
  hasNew <- !is.na(tagNew) & nzchar(tagNew)
  tag[hasNew] <- tagNew[hasNew]
  tag[is.na(tag)] <- ""

  ## ------- tagOld --------------------------------------------------- ##
  ## Resolved in step 3b, per source ('tag_old' from 'tree' or
  ## 'miss_num' from 'rep_miss'), with NA for empty / "sem info".
  tagOld <- .getCol(ms, "tagOld")

  ## ------- stem ----------------------------------------------------- ##
  ## Stem sequential number, keeping the XForm spelling.
  ## After the "_miss" strip the name is 'mult_stem_seq' in both branches.
  stemSeq <- as.character(.getCol(ms, "mult_stem_seq"))
  stemSeq[is.na(stemSeq)] <- ""

  if (nrow(ms) > 0L) {
    message(sprintf("multStem: %d registro(s); tag preenchido=%d; stem preenchido=%d",
                    nrow(ms), sum(nzchar(tag)), sum(nzchar(stemSeq))))
  }

  multStemOut <- cbind(
    .contextDf(ms, keyEnv),
    data.frame(
      tag     = tag,
      tagOld  = tagOld,
      stem    = stemSeq,
      tagMult = .getCol(ms, "tag_plaq_sec"),
      pom     = .getCol(ms, "sec_pom"),
      dbh     = dbh,
      ht      = .getCol(ms, "sec_ht_new"),
      ## media path
      picture = .asMediaPath(.getCol(ms, "sec_picture")),
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
  ##     existing files are never overwritten; only records that are    ##
  ##     not already present are appended; record counts are reported   ##
  ## ------------------------------------------------------------------ ##

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

      lg <- .writeOneWithMerge(dd, dirOut, nm)
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
      dd <- .readCsvSafe(tf)
      if (nrow(dd) == 0L) return(NULL)
      if (!"quad"    %in% names(dd)) dd$quad    <- sub("^data", "", basename(dirname(tf)))
      if (!"subquad" %in% names(dd)) dd$subquad <- NA_character_
      if (!"tag"     %in% names(dd)) dd$tag     <- NA_character_
      dd
    })
    treesAll <- .bindRows(treesList)
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
      oldTags <- .readCsvSafe(tagsUsedPath)
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
        oldRep <- .readCsvSafe(tagsRepeatPath)
        if (nrow(oldRep) > 0L) tagsRepeat <- .bindRows(list(oldRep, tagsRepeat))
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
  ## 14. filesDataMerged.csv at the root of expDir                      ##
  ##     Incremental record of the base names already merged, so the    ##
  ##     next run can skip them (see step 1b).                          ##
  ## ------------------------------------------------------------------ ##
  filesReaded <- unique(.baseName(csvFiles))
  filesReaded <- filesReaded[!is.na(filesReaded) & nzchar(filesReaded)]

  filesReaded <- unique(c(mergedBase, filesReaded))
  filesReaded <- filesReaded[!is.na(filesReaded) & nzchar(filesReaded)]

  utils::write.csv(data.frame(fileNames = filesReaded),
                   filesMergedPath, row.names = FALSE, na = "")
  message(sprintf("filesDataMerged.csv: %d nome(s) base registrado(s).",
                  length(filesReaded)))

  ## ------------------------------------------------------------------ ##
  ## 15. Flatten every "media" folder into expDir/media                 ##
  ## ------------------------------------------------------------------ ##

    mediaExpDir <- file.path(expDir, "media")
    mediaOrigDir <- file.path(csvDir, "media")
    filesOrig <- list.files(mediaOrigDir, recursive = TRUE, full.names = TRUE)
    filesExp <- list.files(mediaExpDir, recursive = TRUE, full.names = TRUE)

    filesOrigName <- basename(filesOrig)
    filesExpName <- basename(filesExp)

    filesToExp <- filesOrig[!filesOrigName %in% filesExpName]
    for (mf in filesToExp) {
      ## never overwrite a media file that already exists
      file.copy(mf, file.path(mediaExpDir, basename(mf)), overwrite = FALSE)
    }



  invisible(expDir)
}
