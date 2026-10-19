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
#' By default every CSV in \code{csvDir} is (re)processed on each run --- the
#' row-level de-duplication inside \code{.writeOneWithMerge} keeps the result
#' idempotent. The \code{filesDataMerged.csv} file is only an audit log now.
#' Pass \code{force = TRUE} to delete the previous per-quadrat outputs and the
#' control files before merging, so the resulting CSVs are rebuilt from
#' scratch (this is what fixes an old/incorrect output produced by an earlier
#' version of \code{mergeCSV}).
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
#' @param force Logical. When TRUE, the previous per-quadrat outputs
#'   (\code{data*}/) and the control files (\code{tagsUsed.csv},
#'   \code{tagsRepeat.csv}, \code{filesDataMerged.csv}) are removed before
#'   merging, so the outputs are rebuilt from scratch. This is the recommended
#'   way to fix a stale output (e.g. an old \code{multStem.csv} with empty
#'   \code{tag}/\code{stem} columns written by a previous version of
#'   \code{mergeCSV}). Default FALSE.
#'
#' @return Invisibly, the path to \code{expDir}.
#' @export
mergeCSV <- function(csvDir, expDir = NULL, force = FALSE) {

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

  ## ------------------------------------------------------------------ ##
  ## 0. force = TRUE: wipe previous outputs so they are rebuilt         ##
  ## ------------------------------------------------------------------ ##
  if (isTRUE(force)) {
    oldDirs <- list.dirs(expDir, recursive = FALSE, full.names = TRUE)
    if (length(oldDirs) > 0L) {
      oldDirs <- oldDirs[startsWith(basename(oldDirs), "data")]
      for (d in oldDirs) unlink(d, recursive = TRUE)
    }
    unlink(file.path(expDir, "tagsUsed.csv"))
    unlink(file.path(expDir, "tagsRepeat.csv"))
    unlink(file.path(expDir, "filesDataMerged.csv"))
    message("force = TRUE: previous per-quadrat outputs and control ",
            "files were removed.")
  }

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

  ## 1b. NOTE: we no longer skip CSVs whose base name was already merged.
  ##
  ##     The old logic used `.baseName()` = gsub("-.*|\\.csv", "", f),
  ##     which returns just "treeCensusForm" for EVERY CSV of the form
  ##     (treeCensusForm.csv, treeCensusForm-subquad-tree.csv,
  ##     treeCensusForm-subquad-tree-sec_stem.csv, ...). As soon as one
  ##     run recorded "treeCensusForm" in filesDataMerged.csv, EVERY
  ##     subsequent run silently skipped all CSVs of that form, so a
  ##     corrected mergeCSV never had a chance to rewrite an earlier
  ##     incorrect output (e.g. an old multStem.csv with empty
  ##     tag/stem columns).
  ##
  ##     The row-level de-duplication inside `.writeOneWithMerge` is
  ##     already idempotent, so reprocessing a CSV is safe. The
  ##     `filesDataMerged.csv` file is kept as an audit log only.
  mergedBase <- character(0)

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

  ## multStem: combine sec_stem and sec_stem_miss, stripping "_miss" from names
  stemList <- lapply(c(stemF, stemMF), function(f) {
    d <- dataList[[f]]
    names(d) <- sub("_miss$", "", names(d))
    d
  })
  multStemDf <- .bindRows(stemList)

  dropKeys <- c("KEY", "PARENT_KEY")

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
  ## Importante: tanto 'sel_subquad' quanto 'mapxy' podem estar apenas no
  ## registro pai (repeat de subquad), e nao no repeat de arvores. Por isso
  ## buscamos com .fieldSelfOrAncestor (self ou ancestral), e nao com .getCol
  ## (que so enxerga o proprio registro e devolvia NA, zerando dx/dy).
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
      ## 
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
  for (pc in tnPicCols) tn[[pc]] <- .asMediaPath(tn[[pc]])

  treesFoundOut <- cbind(.contextDf(treeNFDf, keyEnv), tn)
  treesFoundOut <- treesFoundOut[selFound, , drop = FALSE]

  ## ------------------------------------------------------------------ ##
  ## 10. multStem.csv                                                   ##
  ##                                                                    ##
  ## multStem reune os fustes secundarios de DOIS ramos do XForm:      ##
  ##   - 'subquad/tree/sec_stem'             (arvores do censo)         ##
  ##   - 'subquad/rep_miss/sec_stem_miss'    (arvores encontradas)      ##
  ##                                                                    ##
  ## Como os dois ramos usam nomes de colunas diferentes (ja unificados ##
  ## acima por sub("_miss$","",names)), precisamos lembrar que:         ##
  ##   * o numero sequencial do fuste e 'mult_stem_seq' (apos o strip), ##
  ##     nunca 'mult_stem_sec'. O fallback para 'mult_stem_sec' fica    ##
  ##     apenas como defesa contra CSVs antigos;                        ##
  ##   * a placa da arvore-mae NAO esta em 'sec_stem'/'sec_stem_miss'. ##
  ##     No ramo 'tree' ela vem do ancestral 'num_tag'; no ramo         ##
  ##     'rep_miss' ela vem do ancestral 'miss_num'. Por isso usamos    ##
  ##     .fieldSelfOrAncestor (busca na cadeia KEY/PARENT_KEY), com     ##
  ##     fallback de 'num_tag' para 'miss_num'.                         ##
  ## ------------------------------------------------------------------ ##
  ms <- multStemDf

  dbhMm <- suppressWarnings(as.numeric(.getCol(ms, "dbh_mm_sec")))
  dbhCm <- suppressWarnings(as.numeric(.getCol(ms, "dbh_cm_sec")))
  papCm <- suppressWarnings(as.numeric(.getCol(ms, "pap_cm_sec")))

  dbh <- rep(NA_real_, nrow(ms))
  dbh[!is.na(dbhMm)] <- dbhMm[!is.na(dbhMm)]
  i2 <- is.na(dbh) & !is.na(dbhCm); dbh[i2] <- dbhCm[i2] / 10
  i3 <- is.na(dbh) & !is.na(papCm); dbh[i3] <- papCm[i3] / (pi * 10)
  dbh <- round(dbh, 1)

  ## tag: 'num_tag' (arvores do censo) com fallback para 'miss_num'
  ## (arvores encontradas dentro de 'rep_miss'). Ambos vivem em
  ## ancestrais do repeat de fustes, por isso buscamos via
  ## .fieldSelfOrAncestor.
  tagStem <- as.character(.fieldSelfOrAncestor(ms, "num_tag",  keyEnv))
  tagMiss <- as.character(.fieldSelfOrAncestor(ms, "miss_num", keyEnv))
  tagStem <- ifelse(is.na(tagStem) | !nzchar(tagStem), tagMiss, tagStem)
  tagStem[is.na(tagStem)] <- ""

  ## stem: 'mult_stem_seq' e o nome correto (apos sub("_miss$","",names)).
  ## Se por algum motivo o CSV antigo trouxe 'mult_stem_sec', caimos nele
  ## como fallback (defesa contra bancos gerados por versoes anteriores).
  stemSeq <- as.character(.getCol(ms, "mult_stem_seq"))
  if (nrow(ms) > 0L &&
      all(is.na(stemSeq) | !nzchar(stemSeq))) {
    altSeq <- as.character(.getCol(ms, "mult_stem_sec"))
    if (!all(is.na(altSeq) | !nzchar(altSeq))) {
      stemSeq <- altSeq
    }
  }
  stemSeq[is.na(stemSeq)] <- ""

  ## Diagnostic messages (very useful to spot a stale cache vs a
  ## real capture problem in readInstancesXML).
  if (nrow(ms) > 0L) {
    message(sprintf(
      "multStem: %d registros; num_tag preenchido=%d/%d; mult_stem_seq preenchido=%d/%d",
      nrow(ms),
      sum(nzchar(tagStem)),     nrow(ms),
      sum(nzchar(stemSeq)),     nrow(ms)
    ))
    if (sum(nzchar(stemSeq)) == 0L) {
      message("  -> nenhum mult_stem_seq preenchido. Verifique se os CSVs ",
              "'*-sec_stem.csv' / '*-sec_stem_miss.csv' em csvDir tem a coluna ",
              "'mult_stem_seq' com valores.")
    }
    if (sum(nzchar(tagStem)) == 0L) {
      message("  -> nenhum num_tag/miss_num preenchido. Verifique se o CSV ",
              "'*-subquad-tree.csv' (ou '*-subquad-rep_miss.csv') esta na ",
              "mesma pasta de csvDir e contem a coluna com o valor.")
    }
  }

  multStemOut <- cbind(
    .contextDf(ms, keyEnv),
    data.frame(
      tag     = tagStem,
      ## numero sequencial do fuste
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
    if (file.exists(tagsUsedPath) && !isTRUE(force)) {
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
      if (file.exists(tagsRepeatPath) && !isTRUE(force)) {
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
  ##     Audit log with the base names that appear in csvDir.           ##
  ##     It is NOT used any more to skip CSVs (see section 1b).         ##
  ## ------------------------------------------------------------------ ##
  filesReaded <- unique(.baseName(csvFiles))
  filesReaded <- filesReaded[!is.na(filesReaded) & nzchar(filesReaded)]

  filesReaded <- unique(c(mergedBase, filesReaded))
  filesReaded <- filesReaded[!is.na(filesReaded) & nzchar(filesReaded)]

  utils::write.csv(data.frame(fileNames = filesReaded),
                   file.path(expDir, "filesDataMerged.csv"),
                   row.names = FALSE, na = "")
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
