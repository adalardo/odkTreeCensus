############################################
### Alexandre Adalardo
### first version: 15 de outubro de 2018
### actual version: 25 de maio de 2025
############################################
# export data from odk collect using
# odkbriefcase java package
##############################
##' Export data from okd collect to a csv files 
##'
##' @param colDir ODK collect data directory, base name will be used for named the exported files.
##' @param stDir  ODK storage temporary directory address.
##' @param expDir Data export directory. 
##' @param formId ODK form id, defined in the settings of xls and xml forms.
##' @param startDate start date to export data from ODK Collect.
##' @param endtDate end date to export data from ODK Collect.
##' @return 'odkExp' returns csv files exported from odk collect using odkbriefcase java application. One file is create for each repeat in the form. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @examples
##' 
##' \dontrun{
##' odkExp(colDir = getwd(), stDir = getwd(), expDir = getwd(), form_id = "odkform")
##' }
#vb#' 
##' @export
##'
##'
##'
expODK <- function(colDir = getwd(), stDir = getwd(), expDir = getwd(), formId = NULL,  startDate = NULL, endDate = NULL )
{
    odkbriefcase <-  system.file("java", "ODK-Briefcase-v1.18.0.jar", package = "Rppsp")
    fileBaseName <- basename(colDir)
   ## exportando o formulario do odk collect
    odkstdir <- file.path(stDir, "ODK Briefcase Storage")
    expDirName <- file.path(expDir, fileBaseName)
    if(dir.exists(odkstdir))
    {
        unlink(odkstdir, recursive = TRUE)
    }

    if(any(grepl(paste(fileBaseName,".*\\.csv$", sep = ""), list.files(expDirName))))
    {
        yn <-  readline("File name already exists in the export directory, overwrite it (y/n): ")
        if(yn=="n")
        {
            stop("Try again with a new export file name")
        }
        if(yn=="y")
        {
            unlink(expDirName, recursive = TRUE)
        }
    }
    briefexp <- paste('java -jar ', '"',odkbriefcase,'"' , ' --pull_collect --storage_directory ', '"',stDir,'"', ' --odk_directory ', '"',colDir,'"', sep = "")
    system(briefexp)
    if(is.null(startDate) | is.null(endDate))
    {
        briefcsv <- paste('java -jar ',  '"', odkbriefcase,'"', ' --export --form_id ', formId, ' --storage_directory ', '"',stDir,'"', ' --export_directory ', '"',expDirName,'"', ' --export_filename ', fileBaseName, sep = "")
    } else{
    briefcsv <- paste('java -jar ',  '"', odkbriefcase,'"', ' --export --form_id ', formId, ' --storage_directory ', '"',stDir,'"', ' --export_directory ', '"',expDirName,'"', ' --export_filename ', fileBaseName, ' --export_start_date ', startDate , ' --export_end_date ', endDate, sep = "")
    }
    system(briefcsv)
    unlink(odkstdir, recursive = TRUE)
}

#######################################################
##' Join odk exported csv files on a single data frame object
##'
##' @param csvDir directory direction to ODK csv exported files
##' @param expDir Data export directory 
##' @param saveFile logical, if TRUE file will be saved in expDir
##' @return 'joinODK' returns txt files that merge ODK csv files exported and keep the csv files in the baseData directory. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @examples
##' 
##' \dontrun{
##' joinODK(csvDir = getwd(),  saveFile = TRUE, expDir = getwd())
##' }
##' 
##' @export
##'
##'
##'
##
## expDir <- "/home/aao/Ale2024/AleProjetos/PPPeic/censo2025/dadosPPSP/Assis/testExp/odkCSV"
joinODK <- function(csvDir = getwd(), expDir = getwd(),  saveFile = TRUE)
{
    ## base dir 
    baseDir <- dirname(csvDir)
    nameBase <- basename(csvDir)
    namesFiles <- sort(grep(".*\\.csv?", list.files(csvDir), value = TRUE), decreasing = TRUE)
#   csvNames <- sort(namesFiles, decreasing = TRUE)
    formName <- gsub(paste(nameBase,"|.csv", sep = ""), "", namesFiles)
    formName[formName == ""] <- "-base"
## ordenando os arquivos pelo nome dos formularios    
    formOrder <- rep(NA, length(formName))
    formOrder[1] <- grep("base", formName)
    formOrder[2] <- grep("tree", formName) 
    formOrder[3] <- grep("subquad", formName) 
    formOrder[4] <- grep("sec_fuste$", formName)
    formOrder[5] <- grep("sec_fuste_miss", formName)
    formOrder[6] <- grep("rep_miss", formName) 
    formName <- formName[formOrder]
    namesFiles <- namesFiles[formOrder]
    for(j in 1:length(namesFiles))
    {
        assign(paste("form", j, sep = ""), read.table(file.path(csvDir, namesFiles[j]), header = TRUE, as.is = TRUE, sep = ",", quote = '"'))
    }
    qsize <- unique(gsub("size","quad", form1$local.work_size))[1]
    form3 <- form3[which(form3[, qsize] != ''), ]
    form1names <- c('today', 'start', 'end', 'local.plot_name','local.work_size' ,'equipe.equipe_nomes','equipe.equipe_other01','equipe.equipe_other02', 'equipe.lider', 'parcela.quadrat','KEY')
    form3names <- c('quad5', 'quad10', 'sel_subquad', 'tag_selected','newtag_selected' , 'PARENT_KEY', 'KEY')
    form01 <- merge(form1[,form1names], form3[form3names], by.x = "KEY", by.y = "PARENT_KEY", all = TRUE)
    form01 <- form01[!(form01$tag_selected == "" & form01$newtag_selected == ""),]
    form2names00 <- c( 'ttree.tree_type', 'ttree.recruta_id',  'ttag.tag_ok', 'dead_tree.tag_dead', 'dead_tree.dead_obs', 'dead_tree.dead_picture',  'tree_recenso.arvmap_conf', 'confplaq_map.ileg_conf', 'map.mapxy','tree_recenso.old_dx', 'tree_recenso.old_dy',  'tag_new.new_tag', 'tag_new.new_tag_picture', 'tag_old', 'num_tag', 'new_alt.alt_new', 'tree_recenso.old_alt' , 'new_alt.difalt', 'new_alt.alt_met', 'new_alt.alt_check_note', 'new_alt.alt_new_check', 'new_alt.alt_cause', 'new_alt.alt_cause_other', 'new_alt.dif_altOK',  'new_dap.dap_pom', 'new_dap.pom_type', 'new_dap.tipo_med',  'new_dap.dap_new', 'tree_recenso.old_dap' ,'new_dap.difdap', 'new_dap.diffdap_prop',  'new_dap.one_fuste', 'new_dap.nfuste_ok', 'tree_recenso.old_nfuste' , 'new_dap.check_dbh.dap_new_check', 'new_dap.check_dbh.dap_cause', 'new_dap.check_dbh.dap_cause_other', 'new_dap.check_dbh.dif_dapOK', 'new_dap.check_dbh.min_dbhOK', 'nfuste_diff', 'info_id', 'new_id.new_fam', 'tree_recenso.old_fam' ,'new_id.new_gen', 'new_id.new_sp', 'tree_recenso.old_sp' , 'new_id.det_type', 'new_id.indet_type', 'new_id.id_same', 'new_id.nickname_morfo', 'new_id.id_picture', 'new_id.id_picture02', 'new_id.id_picture03', 'new_id.id_picture04', 'new_id.id_picture05', 'conf_info.map_final', 'conf_info.alt_final', 'conf_info.dap_final', 'conf_info.fam_final', 'conf_info.sp_final', 'conf_info.treetype_label', 'conf_info.tagdead_label', 'conf_info.id_type_name', 'conf_info.obs_tree', 'conf_info.tree_picture01', 'conf_info.tree_picture02', 'conf_info.tree_picture03', 'conf_info.tree_picture04', 'conf_info.tree_picture05', 'PARENT_KEY', 'KEY')
    form2names <-  form2names00[(form2names00 %in% names(form2))]
    tree <- form2[, form2names]
    snames2 <- gsub("^.*\\.", "", form2names)
    names(tree) <- snames2
######## Miss trees ##################
    missAlive <- form6[which(form6$miss_found.miss_alive == "yes"),]
    missDead <- form6[which(form6$miss_found.miss_dead != ""),]
    notFound <- form6[which(form6$conf_miss.miss_conf == "miss"),]
######## Miss found alive ##################
    if(nrow(missAlive)>0)
    {
        foundANames00 <- c('miss_num', 'miss_found.tag_ok_miss','miss_found.arvmap_conf_miss', 'miss_found.old_dx_miss', 'miss_found.old_dy_miss', 'miss_found.old_xy_miss' ,'conf_miss.old_dap_miss', 'conf_miss.old_alt_miss', 'conf_miss.old_fam_miss', 'conf_miss.old_sp_miss', 'miss_new_tag.new_tag_miss', 'miss_new_tag.new_tag_picture_miss', 'map_miss.mapxy_miss', 'new_alt_miss.alt_new_miss', 'new_alt_miss.difalt_miss', 'new_alt_miss.alt_met_miss', 'new_alt_miss.alt_check_note_miss', 'new_alt_miss.alt_new_check_miss', 'new_alt_miss.alt_cause_miss', 'new_alt_miss.alt_cause_other_miss', 'new_alt_miss.dif_altOK_miss', 'new_dap_miss.dap_pom_miss', 'new_dap_miss.pom_type_miss', 'new_dap_miss.tipo_med_miss', 'new_dap_miss.dap_new_miss', 'new_dap_miss.difdap_miss', 'new_dap_miss.diffdap_prop_miss', 'new_dap_miss.one_fuste_miss', 'new_dap_miss.nfuste_ok_miss', 'new_dap_miss.check_dbh_miss.dap_new_check_miss', 'new_dap_miss.check_dbh_miss.dap_cause_miss', 'new_dap_miss.check_dbh_miss.dap_cause_other_miss', 'new_dap_miss.check_dbh_miss.dif_dapOK_miss', 'new_dap_miss.check_dbh_miss.min_dbhOK_miss', 'nfuste_diff_miss', 'info_id_miss', 'new_id_miss.new_fam_miss', 'new_id_miss.new_gen_miss', 'new_id_miss.new_sp_miss', 'new_id_miss.det_type_miss', 'new_id_miss.indet_type_miss', 'new_id_miss.id_same_miss', 'new_id_miss.nickname_morfo_miss', 'new_id_miss.id_picture_miss', 'new_id_miss.id_picture02_miss', 'new_id_miss.id_picture03_miss', 'new_id_miss.id_picture04_miss', 'new_id_miss.id_picture05_miss', 'PARENT_KEY', 'KEY')
        foundANames <- foundANames00[foundANames00 %in% names(missAlive)]
        foundAlive <- missAlive[, foundANames]
        sNamesFA <- gsub("^.*\\.|conf_miss-|_miss|miss_found-", "", foundANames)
        names(foundAlive) <- sNamesFA
        names(foundAlive)[grep("miss_num", names(foundAlive))] <- "tag_old"
        foundAlive$num_tag <- foundAlive$tag_old
        newtagT <- !is.na(foundAlive$new_tag)
        foundAlive$num_tag[newtagT] <- foundAlive$new_tag[newtagT]
        foundAlive$dap_final <- foundAlive$dap_new
        foundAlive$dap_final[!is.na(foundAlive$dap_new_check)] <- foundAlive$dap_new_check[!is.na(foundAlive$dap_new_check)]
        foundAlive$alt_final <- foundAlive$alt_new
        foundAlive$alt_final[!is.na(foundAlive$alt_new_check)] <- foundAlive$alt_new_check[!is.na(foundAlive$alt_new_check)]
        foundAlive$tree_type <- "found_alive"
        tree <- merge(tree, foundAlive, by = intersect(names(tree), names(foundAlive)), all = TRUE)
    }
#### end Miss-Found Alive
########################## Miss Found Dead #####################
    if(nrow(missDead)>0)
    {
        foundDNames00 <- c('miss_num', 'miss_found.miss_dead','PARENT_KEY', 'KEY')
        foundDNames <- foundDNames00[foundDNames00 %in% names(missDead)]
        foundDead <- missDead[, foundDNames]
        sNamesFD <- gsub("^.*\\.|_miss", "", foundDNames)
        names(foundDead) <- sNamesFD
        names(foundDead)[grep("miss_num", names(foundDead))] <- "tag_old"
        foundDead$num_tag <- foundDead$tag_old
        names(foundDead)[grep("miss_dead", names(foundDead))] <- "tag_dead"
        foundDead$tree_type <- "found_dead"
        tree <- merge(tree, foundDead, by = intersect(names(tree), names(foundDead)), all = TRUE)
    }
################################################################
    if(nrow(notFound)>0)
    {
        notFoundNames00 <- c("miss_num", 'conf_miss.miss_conf', 'PARENT_KEY', 'KEY')
        notFoundNames <- notFoundNames00[notFoundNames00 %in% names(notFound)] 
        notFound <- notFound[, notFoundNames]
        sNamesNF <- gsub("^.*\\.|_miss", "", notFoundNames)
        names(notFound) <- sNamesNF
        names(notFound)[grep("miss_num", names(notFound))] <- "tag_old"
        notFound$num_tag <- notFound$tag_old
        names(notFound)[grep("miss_conf", names(notFound))] <- "tree_type"
        tree <- merge(tree, notFound, by = intersect(names(tree), names(notFound)), all = TRUE)
    }
#############################################################
    names(tree)[names(tree) =='KEY'] <- "key_tree"
    names(tree)[names(tree) =='PARENT_KEY'] <- "key_quad"
    ## names(tree)[names(tree) =='confplaq_map.ileg_conf'] <- "confTagMap"
    #sqSize <- gsub("size", "", form01$local.work_size)
    names01 <- c("today", "local.plot_name", "equipe.lider","parcela.quadrat", qsize ,"KEY.y")
    #form01$quad10
    treequad <- merge(form01[, names01], tree, by.x="KEY.y", by.y="key_quad", all = TRUE)
    names(treequad)[which(names(treequad) =='KEY.y')] <- "key_quad"
    names(treequad)[which(names(treequad) =='parcela.quadrat')] <- "quadrat"
    names(treequad)[names(treequad) == qsize] <- "subquad"
    sq <- gsub("quad_", "", treequad$subquad)
    ###########################################################
    ###### New maps: for mapping errors or new trees
    ###########################################################
    startxy <- strsplit(sq, "x")
    stx <- as.numeric(sapply(startxy, function(x){x[1]}))
    sty <- as.numeric(sapply(startxy, function(x){x[2]}))
    iMap <- which(treequad$arvmap_conf == "map_no" | treequad$tree_type =="sem_info")
    ndx <- sapply(strsplit(treequad$mapxy[iMap], ";"), FUN= function(x){as.numeric(gsub("x = ", "", x[1])) }) + stx[iMap]
    ndy <- sapply(strsplit(treequad$mapxy[iMap], ";"), FUN= function(x){as.numeric(gsub("y = ", "", x[2]))}) + sty[iMap]
    treequad$new_dy <- treequad$new_dx <- NA
    treequad$new_dx[iMap] <- ndx
    treequad$new_dy[iMap] <- ndy
    ##############################################
    ################## FUSTE MULTIPLOS ###########
    ##############################################
    treequad$new_nfuste <- 1
    treequad$new_secDap <- NA
    treequad$new_secAlt <- NA
############################    
## added June 2025
############################
    treequad$sec_oldTag <- NA
    treequad$sec_secondTagPicture <- NA
    treequad$sec_firstTagPicture <- NA
#############################
    names(form5) <- gsub("_miss$", "", names(form5))
    if(nrow(form5) > 0 | nrow(form4) > 0)
    {
        fusteMult <- merge(form4, form5, by = intersect(names(form4), names(form5)), all = TRUE)
        fusteMult$dap2mm <- fusteMult$dap_mm_sec
        fusteMult$dap2mm[!is.na(fusteMult$dap_cm_sec)] <- fusteMult$dap_cm_sec[!is.na(fusteMult$dap_cm_sec)] *10
        fusteMult$dap2mm[!is.na(fusteMult$pap_cm_sec)] <- round(form4$pap_cm_sec[!is.na(form4$pap_cm_sec)] *10/pi)
        oneKey <- unique(fusteMult$PARENT_KEY)
        for(i in 1:length(oneKey))
        {
            k <- oneKey[i]
            secF <- fusteMult[fusteMult$PARENT_KEY == k,]
            nf <- nrow(secF) + 1
            ## colocando os daps e alt em ordem 
            ## dap01 <- treequad$dap_final[treequad$key_tree == k]
            ## alt01 <- treequad$alt_final[treequad$key_tree == k]
            ## mdap <- sort(as.numeric(c(dap01,secF$dap2mm)), decreasing=TRUE)
            ## malt <- sort(as.numeric(c(alt01,secF$sec_alt_new)), decreasing=TRUE)
            ## dapsec <- paste(mdap[-1], collapse = ";")
            ## altsec <- paste(malt[-1], collapse = ";")
            ## if(dap01 != mdap[1])
            ## {
            ##     treequad[treequad$key_tree == k, c("dap_final")] <- mdap[1]
            ## }
            ## if(alt01 != malt[1])
            ## {
            ##     treequad[treequad$key_tree == k, c("alt_final")] <- malt[1]
            ## }
            dapsec <- paste(sort(as.numeric(secF$dap2mm), decreasing=TRUE), collapse = ";")
            altsec <- paste(sort(as.numeric(secF$sec_alt_new), decreasing=TRUE), collapse = ";")
            ########## June 2025 ######
            oldtagsec <- paste(sort(as.numeric(secF$tag_plaq_sec), decreasing=TRUE), collapse = ";")
            sectagpicture <- paste(secF$sec_picture, collapse = ";")
            firsttagpicture <- paste(secF$fuste01_picture, collapse = ";")
            ###########################
            treequad[treequad$key_tree == k, c("new_nfuste")] <- nf
            treequad[treequad$key_tree == k,  "new_secDap"] <-  dapsec
            treequad[treequad$key_tree == k,  "new_secAlt"] <-  altsec
            ### added June 2025 #######
            treequad[treequad$key_tree == k, "sec_oldTag"] <- oldtagsec
            treequad[treequad$key_tree == k, "sec_secondTagPicture"] <- sectagpicture
            treequad[treequad$key_tree == k, "sec_firstTagPicture"] <- firsttagpicture
            ###########################
         }
    }
###############################
#### tirando dados duplicados
###############################
    dupltag <- treequad$num_tag[duplicated(treequad$num_tag, incomparables=NA)]
    dupligual <- c( )
    for(i in dupltag)
    {
        postag <- which(treequad$num_tag == i)
        dupldata <- treequad[postag, c("dap_final","alt_final", "fam_final", "sp_final")]
        dupligual <- c(dupligual, postag[duplicated(dupldata)])
    }
    treequad <- treequad[!(1:nrow(treequad) %in% dupligual),]
######################################
## aqui para restringir dados de saida
#######################################
    nres00 <- c("today", "equipe.lider", "tree_type", "tag_ok","tag_old", "new_tag", "num_tag", "tag_dead" , "quadrat", "subquad","old_tag","old_dx", "new_dx", "old_dy", "new_dy", "old_alt", "alt_final", "new_secAlt" ,"old_nfuste", "new_nfuste", "old_dap", "dap_final","new_secDap", "old_fam", "old_sp", "new_fam", "new_sp", "dif_dapOK", "dif_altOK")
    nres  <- nres00[nres00 %in% names(treequad)]
    treeres <- treequad[, nres]
    t01 <- unique(treeres$today)[1]
    tsep <- strsplit(t01, split = "\\. |, | ")[[1]]
    today <- paste(tsep[c(2,1,3)], sep = "", collapse = "")
###################
###### save file
###################
    if(saveFile)
    {
        qname <- paste(unique(treequad$quadrat), collapse = "_")
        dname <- file.path(expDir, qname)
        if(!dir.exists(file.path(dname, "baseData")))
        {
            #dir.create(dname)
            dir.create(file.path(dname, "baseData"), recursive=TRUE)
        }
        file.rename(from = file.path(csvDir, namesFiles), to =  file.path(dname, "baseData", paste(nameBase, formName, qname,".csv", sep="")))
        if(dir.exists(file.path(csvDir, "media")) & file.path(csvDir, "media") != file.path(dname, "media"))
            {
                media.names <- list.files(file.path(csvDir, "media"))
                if(!dir.exists(file.path(dname, "media")))
                {
                    dir.create(file.path(dname, "media"), recursive=TRUE)
                }
                if(file.path(csvDir, "media") != file.path(dname, "media"))
                {
                    file.rename(from = file.path(csvDir, "media", media.names), to = file.path(dname, "media", media.names))
                    unlink(file.path(csvDir, "media"), recursive = TRUE)
                }
            }
        if(length(list.files(csvDir)) ==0)
        {
            unlink(csvDir, recursive = TRUE)
        }
        write.table(treequad, file.path(dname, paste("tree",qname, "_" ,today, ".txt", sep="")), sep="\t", row.names=FALSE)
        write.table(treeres, file.path(dname, paste("treeResumo",qname,"_" ,today, ".txt", sep="")), sep="\t", row.names=FALSE)
    }
### INCLUIR UMA SAIDA DE LEITURA DE DADOS EM UM DIRETORIO ESPECIFICO
invisible(treeres)
}
################
#### Merge Data
################
#merge tree main data files tree*.txt, treeMiss*.txt and treeResumo*.txt from a base directory recursively. In the basedir should have only subdirectories that is going to merge. This files should go to audit too.
##############################
##' Merge exported data for all subplots  
##'
##' @param expDir Data exported directory where the subplots data are arranged in subdirectories. 
##' @param mergeMedia Logical, if is 'TRUE' média is grouping in 'allMedia' directory but original media was not deleted.
##' @param zipMedia Logical if is 'TRUE' the 'allMedia' directory is compressed and deleted. In order to run needs 7-zip installed on the system (\url{https://www.7-zip.org/download.html})
##' @param saveFile Lógical, if is 'TRUE' save files in the 'expDir'.
##' @return 'mergeData' save two text files('treeRes*.txt' and 'treeAll*.txt') if 'saveFile' is 'TRUE' or/and the resume data in an assign object. . 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @examples
##' 
##' \dontrun{
##' mergeData(expDir = getwd(), mergeMedia = TRUE, zipMedia = FALSE)
##' }
##' 
##' @export
##'
##'
##'
mergeData <- function(expDir = getwd(), mergeMedia = TRUE, saveFile = TRUE, zipMedia = FALSE)
{
    dirs <- dir(expDir, recursive = FALSE)
    allfiles <- list.files(expDir, recursive=TRUE)
    datafiles <- allfiles[- c(grep("All", allfiles), grep("baseData", allfiles), grep("auditData", allfiles), grep("media", allfiles))]
    #missindex <- grep("treeMiss", datafiles)
    resindex <- grep("Resumo", datafiles)
    treefiles <- file.path(expDir, datafiles[- resindex])
    treefiles <- grep("\\/tree", treefiles, value=TRUE) 
    #missfiles <- file.path(basedir,datafiles[missindex]) 
    resfiles <- file.path(expDir, datafiles[resindex])
    # first files
    tree1 <- read.table(treefiles[1], header=TRUE, as.is=TRUE, sep="\t", quote = '"')   
    res1 <- read.table(resfiles[1], header=TRUE, as.is=TRUE, sep="\t", quote = '"')
    treeNames <- names(tree1)
    resNames <- names(res1)
    if(length(treefiles)>1)
    {
        for(i in 2: length(treefiles))
        {
            tree0 <- read.table(treefiles[i], header=TRUE, as.is=TRUE, sep="\t", quote = '"')
            tree1 <- merge(tree1, tree0, all =TRUE)
            res0 <- read.table(resfiles[i], header=TRUE, as.is=TRUE, sep="\t", quote = '"')
            res1 <- merge(res1, res0, all = TRUE)
        }
    }
########################################
    if(saveFile)
    {
        write.table(tree1, file = file.path(expDir, paste("treeAll_", format(Sys.time(), "%d%b%Y"), ".txt", sep = "")), row.names=FALSE, sep= "\t")
        write.table(res1, file = file.path(expDir,paste("treeResAll_", format(Sys.time(), "%d%b%Y"), ".txt", sep = "")), row.names=FALSE, sep= "\t")
    }    
   # exclude found missed tree and save if exists
    if(mergeMedia)
    {
        dirmedia<- file.path(expDir, "allMedia")  
        mediafiles <- file.path(expDir, grep("media", allfiles, value =TRUE))
        medianames <- sapply(strsplit(mediafiles, "/"), function(x){x[length(x)]})
        dir.create(dirmedia)
        mediavf<- file.copy(from = mediafiles, dirmedia)
        if(zipMedia)
        {
            cat("O processo de compressão de arquivos necessita ter o 7-zip instalado no sistema (https://www.7-zip.org/download.html)\n")
        media7z <-paste("7z a", file.path(expDir, "allmedia"), file.path(dirmedia, "*"))
        system(media7z)
        unlink(dirmedia, recursive = TRUE)
        }
    }
    invisible(res1)
}
######################
#### Censo Data Audit
######################
#############################
##' Check census data with previous census 
##'
##' @param base_name Files prefix name and directory direction without extension
##' @param form_rep Form repeat sections names in order
##' @param dir_exp Data export directory 
##' @param data_type Long or short version of the data
##' @return 'odkexp' returns csv files export from odk collect. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @examples
##' 
##' \dontrun{
##' censoAudit(censofile = "treeA01.txt", olddata = "/home/aao/Ale2016/AleProjetos/PPPeic/censo2025/openDataKit/peic09.csv")
##' }
##' 
##' @export
##'
##'
##'
censoAudit <- function(expDir = getwd(), olddata = "peic09.csv", allsubdir = TRUE)
{
    abasal <- function(x){sum(((x)/2)^2 * pi, na.rm=TRUE)}

    filelist <- list.files(expDir, recursive = allsubdir)
    filelist <- filelist[grep("tree", filelist)]
    filetree <- file.path(expDir, filelist[! (grepl("baseData", filelist) | grepl("treeRes",filelist)| grepl("treeMiss",filelist) | grepl("treeDupl",filelist))])
    auddir <- file.path(expDir, paste("auditData", format(Sys.time(), "%d%b%Y"), sep="" ))
    if(dir.exists(auddir))
    {
        yndir <- readline("Audit directory already exist. The directory was created today.\n Are you sure you want to delete it? (y/n): ")
        yndir <- toupper(substr(yndir,1,1))
        if(yndir == "N")
        {
            stop(paste("Delete", auddir, "if you want to audit data again!") )
        }else{
            unlink(auddir, recursive = TRUE)
        }
    }
    dir.create(auddir, recursive=TRUE)
    tfile <- strsplit( olddata, "\\.")[[1]]
    tfile <- tfile[length(tfile)]
    sc <- ifelse(tfile=="csv",  ",", "\t")
###########################
### ACTUAL CENSUS DATA
###########################
    datatree <- mergeData(basedir = expDir, save.files = FALSE, media.merge= FALSE)
    tree <- datatree$tree
    tree$key_tree <- gsub(".*/sub", "sub", tree$key_tree)
    tree <- tree[,-c(grep("key_quad", names(tree)))]
    tree$quadrat<- as.factor(tree$quadrat)
## tree miss
    filelistMiss <- filelist[ grep("treeMiss", filelist)]
    if(length(filelistMiss)>0)
    {
        tmiss <- datatree$treeMiss
        tmiss$quadrat <- factor(tmiss$quadrat, levels = levels(tree$quadrat))
    }
##########################
## PREVIOUS CENSUS DATA
##########################
    censo09 <- read.table(olddata, header=TRUE, as.is=TRUE, sep=sc, quote = '"')
    dquad09 <- censo09[censo09$quad %in= unique(tree$quadrat), ] # so os quadrats com dados 2025
    dquad09A <- dquad09[dquad09$status %in% c("A","AS") ,]
    ntquad09 <- table(dquad09A$quad)
    q09D <- table(dquad09[dquad09$status=="D","quad"])
    q09NE <- table(dquad09[dquad09$status=="NE","quad"])
    ab09 <- tapply(dquad09A$dbhcm, dquad09A$quad, abasal)
#####################################
### Dados nao confirmados em campo
#####################################
    confno <- sum(is.na(tree$conf_ok))
    tagconfno <- tree$num_tag[is.na(tree$conf_ok)]
    if(confno > 0)
    {
        fnotconf <- file.path(auddir, "datanotconfirmed.txt")
        if(file.exists(fnotconf))
        {
            confyes <- readline(paste("File", fnotconf, "already exist. Overwrite it (y/n): " ))
        } 
        if(file.exist(fnotconf) | confyes==yes)
        {
            naoconf <- tree[tree$num_tag %in% tagconfno, ]
            write.table(naoconf, , sep="\t", row.names=FALSE)
            message(paste("datanotconfirmed.txt file saved in dir:", auddir))
        }
    }
#####################################
### DADOS DE TAG NA (confirmar)
#####################################
## dados num_tag = NA. Normalmente, trata-se de morta com placa, mas precisa ter o numero ou posicao da arvores
    tagNA <- is.na(tree$num_tag)
    ntagNA <- sum(tagNA)
    if(ntagNA > 0)
    {
        tagNAdata <- tree[tagNA,]
        write.table(tagNAdata, file.path(auddir, "tagNA.txt"), sep="\t", row.names=FALSE)
        message(paste("tagNA.txt file saved in dir:", auddir))

    }
#####################################
### DADOS DE TAG REPETIDO (confirmar)
#####################################
    reptag <- duplicated(tree$num_tag, incomparables= NA)
    nreptag <- sum(reptag)
    tagrep <- NULL
    if(nreptag> 0)
    {
        tagrep <- tree[reptag,"num_tag"]
        dtagrep<- tree[tree$num_tag %in% tagrep, ]
        data.tagrepetido <- dtagrep[order(dtagrep$num_tag),]  ### saida para a tela
        write.table(data.tagrepetido, file.path(auddir, "tagrepeated.txt"), sep="\t", row.names=FALSE)
        message(paste("tagrepeated.txt file saved in dir:", auddir))

    }
###############################
### DESAPARECIDAS ENCONTRADAS
###############################
    tagenc <- NULL
    if(exists('tmiss'))
    {
        tenc  <- tree$num_tag %in% tmiss$miss_num
        #tagenc <- NULL
        if(sum(tenc)>0)
        {
            tagenc <- tree[tenc, "num_tag"]
            dmenc <- tmiss[tmiss$miss_num %in% tagenc, c("miss_num", "miss_type", "quadrat", "quad5x5")]
            dtenc <-  tree[tree$num_tag %in% tagenc,c("num_tag", "tree_type", "quadrat", "quad5x5", "old_dx", "old_dy", "map2025","new_dx2025", "new_dy2025")]
            denc <-cbind(dmenc,dtenc[match( dmenc$miss_num, dtenc$num_tag),])
            missNF <- tmiss[! tmiss$miss_num %in% denc$miss_num, ]
#source("/home/aao/Ale2016/AleProjetos/Rppsp/R/quadrat.R")
#newind <- subquad(denc$new_dx2025, denc$new_dy2025)
## abaixo os comandos da funcao 'subquad'
            pos <- seq(5,15, by= 5)
            dx.ind <- apply(as.matrix(sapply(pos, function(x){ifelse(denc$new_dx2025 >= x, 5,0)})), 2, sum)
            dy.ind <- apply(as.matrix(sapply(pos, function(x){ifelse(denc$new_dy2025 >= x, 5,0)})), 2, sum)
            dxy <- paste(dx.ind, "x",dy.ind, sep="")
            dxy[grep("NA",dxy) ] <- NA
#denc$indnew <- dxy
            namap <- is.na(denc$new_dx2025)
            names(denc)[7:8] <- c("quad2025", "subq2025")
            samequad <- denc$quadrat == denc$quad2025 & denc$quad5x5 == denc$subq2025
            denc$erroType <- NA
#denc$ver<- no
### Mesmo subq , nao remapeada
            denc$erroType[namap & samequad] <- "sq_nr"  ### incluir na saida
### Outro subquad, nao mapeada
            denc$erroType[namap & !samequad] <- "oq_nr" ## incluir na saida
### Mesmo subq, remapeada
            denc$erroType[! namap & samequad] <- "sq_yr"## incluir na saida
### Outro subquad e remapeada
            denc$erroType[!namap & !samequad] <- "oq_yr" ## incluir na saida
            error.definitions <- data.frame(error= c("sq", "oq", "yr", "nr"), definition= c("same quadrat", "other quadrat", "remaped", "notremaped"),stringsAsFactors = FALSE )
            attributes(denc)$error.definitions <- error.definitions
################
            write.table(denc, file.path(auddir, "misstreefound.txt"), sep="\t", row.names=FALSE)
        message(paste("misstreefound.txt file saved in dir:", auddir))
            print(error.definitions)
################
        } else{
            missNF <- tmiss
        }
        mquadMiss   <- table(missNF$quadrat)
#        m10qMiss <- table(missNF$quadrat[missNF$miss_dbhcm > 10])
    }else{
        missNF <- NULL
        mquadMiss  <- rep(0, nlevels(tree$quadrat))
#        m10qMiss <- 0
    }
################################################
###  Estatistica quadrats
################################################
### estatistica por quadrado de 20x20
    reptagquad <- tapply(reptag, tree$quadrat, sum)
    quadsort <- sort(unique(as.character(tree$quadrat)))


    
    tabtype <- table(tree$quadrat, factor(tree$tree_type, levels= c("plaq_dead", "plaq_ileg", "plaqueada", "sem_info", "sem_plaq_gr")))
    if(nrow(tabtype) >1)
    {
        vivas <- apply(tabtype[,-1], 1, sum)
    } else {vivas = sum(tabtype[, -1])}
    if(!exists('tmiss'))
    {
       mquadMiss <- tapply(!(dquad09A$tag %in% tree$tree_tag), as.factor(dquad09A$quad), sum)
    }

    
    lider <- tapply(tree$equipe.lider, tree$quadrat, function(x){paste(unique(x), collapse = "_")})
    nsubq <- tapply(tree$quad5x5, tree$quadrat, function(x){length(unique(x))})
    datequad <- tapply(tree$today, tree$quadrat, function(x){paste(unique(x), collapse = ";")})
    nnewtag <- tapply((! is.na(tree$new_tag)), tree$quadrat, sum)
################################
##### MORTALITY
################################
    mquad <- tapply(tree$tree_type == "plaq_dead", tree$quadrat, sum) + mquadMiss
    mrquad <- round((1- ((ntquad09 - mquad)/ntquad09)^(1/8)) *100, 2)
#    mr10q <- round((1- ((ntquad09 - m10q)/ntquad09)^(1/8))*100,2)
################################
##### RECRUITMENT 
################################
#table(tree$tree_type)
    daps <- as.numeric(tree$dap2025[tree$tree_type == "sem_info" & is.na(tree$tree_tag)])
    qds <- tree$quadrat[tree$tree_type == "sem_info" & is.na(tree$tree_tag)]
    recquad <- tapply(tree$tree_type == "sem_info" & is.na(tree$tree_tag), tree$quadrat, sum)
    maxdaprec <-  tapply(daps, qds, max, na.rm=TRUE)
    meandaprec <-  tapply(daps, qds, mean, na.rm=TRUE)
    daprecmais5 <-  tapply(daps, qds,function(x){sum(x>50, na.rm=TRUE)})
    tagrec5 <- tree$num_tag[tree$tree_type == "sem_info" & is.na(tree$tree_tag) & tree$dap2025> 50]
    if(sum(daprecmais5, na.rm=TRUE)> 0)
    {
        listmais5 <- tree[tree$num_tag %in% tagrec5,]
        write.table(listmais5, file.path(auddir, "newmorethan5cmdbh.txt"), sep="\t", row.names=FALSE)
        message(paste("newmorethan5cmdbh.txt file saved in dir:", auddir))
    }
###############################
##### BASAL AREA
###############################
    ab18 <- tapply(tree$dap2025/10, tree$quadrat, abasal)
    abdiff <- round((ab18 - ab09)/ab09,3)
## relative growth rate in diameter for main stem
    dap09 <- as.numeric(sapply(strsplit(tree$old_dap, ";"), function(x){x[1]}))
    rgrdap <- round((tree$dap2025 - dap09)/dap09, 3)
## diminuiu mais de 5% ou mais de 10mm
#cdap[c(which(rgrdap < -0.10) , which((tree$dap2025 - dap09)< -10)),]
    rgbless <-  tree[c(which(rgrdap < -0.10) , which((tree$dap2025 - dap09)< -10)),]
    if(nrow(rgbless)> 0)
    {
        rgbless$erroType <- "negative growth"
    }
## aumentou mais de 20% ou mais de 50mm 
#cdap[c(which(rgrdap > 0.5) , which((tree$dap2025 - dap09) > 100)),]
    rgbmore <-  tree[c(which(rgrdap > 0.5) , which((tree$dap2025 - dap09) > 100)),]
    if(nrow(rgbmore)> 0)
    {
        rgbmore$erroType <- "rapid growth"
    }
    rgbcheck <- rbind(rgbless, rgbmore)
   
    if(nrow(rgbcheck)> 0)
    {
        write.table(rgbcheck, file.path(auddir, "atipicalgrowth.txt"), sep="\t", row.names=FALSE)
        message(paste("atipicalgrowth.txt file saved in dir:", auddir))
    }
### quadrat resume
    summaryQuad <- data.frame(quad = quadsort, date = datequad[quadsort],  lider = lider[quadsort],  nsubq = nsubq[quadsort], nvivas = as.vector(vivas[quadsort]) , nvivas09 = as.vector(ntquad09[quadsort]), mortaPlaca = tabtype[quadsort, "plaq_dead"], placaIleg = tabtype[quadsort, "plaq_ileg"], vivaPlaca = tabtype[quadsort, "plaqueada"], semPlacaGr = tabtype[quadsort, "sem_plaq_gr"] , nMiss = mquadMiss[quadsort] ,newtag=as.vector(nnewtag[quadsort]), mortas = as.vector(mquad[quadsort]), taxaMorte = as.vector(mrquad[quadsort]), abasal =  round(as.vector(ab18[quadsort]),1), abasal09= round(as.vector(ab09[quadsort]),1), recr18 = as.vector(recquad[quadsort]))
    write.table(summaryQuad, file.path(auddir, "summaryQuadrat.txt"), sep="\t", row.names=FALSE)
    message(paste("summaryQuadrat.txt file saved in dir:", auddir))
#########################
## Subquadrats faltantes    
#########################
    quadinc <- summaryQuad[summaryQuad$nsubq !=16, "quad"]
    subq <- paste(rep(c(0,5,10, 15), each=4), "x", rep(c(0,5,10, 15), 4), sep= "")
    quads <- tree[tree$quadrat %in% quadinc,c("quadrat", "quad5x5") ]
    qq <- sq <- vector()
for(i in quadinc)
    {
        ss <- subq [!(subq  %in% quads$quad5x5[quads$quadrat ==i])]
        sq <- c(sq, ss)
        qq <- c(qq, rep(i, length(ss)))

    }
    squadno <- data.frame(quad = qq, subq = sq)
    if(nrow(squadno)> 0)
    {
        write.table(squadno, file.path(auddir, "subquadfalta.txt"), sep="\t", row.names=FALSE)
        message(paste("subquadfalta.txt file saved in dir:", auddir))
    }
####################
#### Audit all trees
####################
    allaudit <- tree[((tree$num_tag %in% unique(c(tagconfno, tagrep, tagenc , tagrec5,  rgbcheck$num_tag)) )| tagNA), ]
    #c("today", "quad", "tree_type", "tree_tag", "num_tag", "old_dx","old_dy", "new_dx2025", "new_dy2025", "old_dap","dap2025", "alt2025","fam2025", "sp2025", "key_tree")
    errortype <- rep(NA, nrow(allaudit))
    errortype[allaudit$num_tag %in% tagrep] <- "tag number duplicated"
    if(length(nreptag)>0)
    {
        for(i in tagrep)
        {
            allaudit$num_tag[which(allaudit$num_tag == i)] <- paste(allaudit$num_tag[which(allaudit$num_tag == i)], 1: sum(allaudit$num_tag ==i, na.rm=TRUE), sep=".0")
        }
    }
    errortype[is.na(allaudit$num_tag)] <- paste(errortype[is.na(allaudit$num_tag)],"tag NA", sep="; ")
    errortype[allaudit$num_tag %in% tagenc] <- paste(errortype[allaudit$num_tag %in% tagenc],"tag miss found", sep="; ")
    errortype[allaudit$num_tag %in% tagrec5] <- paste(errortype[allaudit$num_tag %in% tagrec5],"recruit > 5cm", sep="; ")
    errortype[allaudit$num_tag %in% rgbcheck$num_tag] <- paste(errortype[allaudit$num_tag %in% rgbcheck$num_tag],"atypical growth", sep="; ")
    errortype <- sub("NA; ", "", errortype)
    allaudit$errorType <- errortype
    write.table(allaudit, file.path(auddir, "treeaudit.csv"), sep=",", row.names=FALSE)
    message(paste("treeaudit.csv file saved in dir:", auddir))
}








##################################################
########### read CSV audit Data ##################
##################################################
##' Read odk exported audit csv files to a single data frame
##'
##' @param base_name Files prefix name and directory direction without extension
##' @param form_rep Form repeat sections names in order
##' @param dir_exp Data export directory 
##' @param data_type Long or short version of the data
##' @return 'odkexp' returns csv files export from odk collect. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @examples
##' 
##' \dontrun{
##' odkexp(odkbriefcase = "odkbriefcase.jar", dir_st = getwd(), dir_col = getwd(), dir_exp = getwd(), form_id = "odkform", file_name = "odkdata")
##' }
##' 
##' @export
##'
##'
##'
readAudit.csvODK <- function(base_files,  save_file = TRUE, dir_exp = getwd(), file_name = NULL)  #base_name = "auditPeic"
{
    ## base dir
    base_dir <- dirname(base_files)
    file0 <- base_files[! (base_files %in% grep(paste(form_rep,collapse="|"), base_files, value=TRUE))]
    ## base form file
    ## nrep <- length(form_rep)
    ## names.csv <- paste(base_name, c("",rep("-",nrep)),c("", form_rep), ".csv", sep="")
    for(j in 0:length(formNames))
    {
        if(j==0)
        {
         assign(paste("form", j, sep=""), read.table(file0, header=TRUE, as.is=TRUE, sep=",", quote = '"')) 
        } else{
        assign(paste("form", j, sep=""), read.table(grep(formName[j], base_files, value=TRUE), header=TRUE, as.is=TRUE, sep=",", quote = '"'))}
    }
    
    form0names <- c('today', 'start', 'end', 'equipe.equipe_nomes', 'equipe.equipe_nomes_other', 'equipe.lider', 'parcela.quadrat', 'parcela.piquete', 'parcela.obs_quad', 'KEY')
    form1names <- c('tag_aud',  'error', 'dap2025','alt2025','dx2025','dy2025','fam2025','sp2025','arv_aud.dap_conf', 'arv_aud.alt_conf', 'arv_aud.xy_conf', 'arv_aud.id_conf', 'arv_aud.diag_aud', 'arv_aud.diag_aud_other', 'tipo_med',  'aud_dap.dap2025_mm', 'aud_dap.dap2025_cm', 'aud_dap.pap2025_cm', 'aud_dap.one_fuste',  'diag_tag.diag_tag_new', 'diag_tag.diag_tag_foto', 'diag_tag.diag_tag_obs',  'aud_alt.aud_alt0', 'aud_alt.aud_alt1', 'aud_xy.aud_x', 'aud_xy.aud_y',  'aud_id.aud_fam', 'aud_id.aud_gen', 'aud_id.aud_sp', 'aud_id.aud_coleta', 'aud_id.aud_id_picture', 'aud_id.aud_id_same', 'PARENT_KEY', 'KEY', 'SET.OF.tree_audit')
    form01 <- merge(form0[,form0names], form1[form1names], by.x="KEY", by.y="PARENT_KEY", all = TRUE)
    form01names <- names(form01)

    

 
    treenames <- gsub("equipe\\.", "", form01names)
    treenames <- gsub("parcela\\.", "", treenames)
    treenames <- gsub("arv_aud\\.", "", treenames)
    treenames <- gsub("diag_tag.diag_", "aud_", treenames)
    treenames <- gsub("aud_dap\\.", "aud_", treenames)
    treenames <- gsub("aud_alt\\.", "", treenames)
    treenames <- gsub("aud_xy\\.", "", treenames)
    treenames <- gsub("aud_id\\.", "", treenames)
    tree <- form01
 

    names(tree) <- treenames
    names(tree)[names(tree) =='KEY.y'] <- "key_tree"
    names(tree)[names(tree) =='KEY'] <- "key_quad"
    

    
    tree$aud_nfuste <- 1
    tree$aud_dap2025_sec_mm <- NA
    form2$dap2mm <- form2$dap_mm_sec
    form2$dap2mm[!is.na(form2$dap_cm_sec)] <- form2$dap_cm_sec[!is.na(form2$dap_cm_sec)] *10
    oneKey <- unique(form2$PARENT_KEY)
    if(length(oneKey)>0)
    {
        for(i in 1:length(oneKey))
        {
            k <- oneKey[i]
            secF <- form2[form2$PARENT_KEY == k,]
            nf <- nrow(secF) + 1          
            mdap <- sort(as.numeric(secF$dap2mm), decreasing=TRUE)
            dapsec <- paste(mdap, collapse = ";")
            tree[tree$key_tree == k, c("aud_nfuste", "aud_dap2025_sec_mm")] <- c( nf, dapsec) 
        }
    }
###############################
## ####### tirando dados repetidos 
## ###############################
##     dupltag <- treequad$num_tag[duplicated(treequad$num_tag, incomparables=NA)]
##     dupligual <- c( )
##     for(i in dupltag)
##     {
##         postag <- which(treequad$num_tag == i)
##         dupldata <- treequad[postag, c("dap2025","alt2025", "fam2025", "sp2025")]
##         dupligual <- c(dupligual,postag[duplicated(dupldata)])
##     }
##     treequad <- treequad[!(1:nrow(treequad) %in= dupligual),]
## #################################
##     nres <- c("today", "equipe.lider", "tree_type", "tag_ok","tree_tag", "new_tag", "num_tag", "tag_dead" , "quadrat", "quad5x5","old_dx", "old_dy","new_dx2025", "new_dy2025", "old_alt", "alt2025","old_nfuste", "nfuste2025", "old_dap", "dap2025","dap2025_sec_mm", "old_fam", "old_sp", "fam2025", "sp2025", "coleta", "difdapOK", "difaltOK" ,"obs_tree")
##     nres  <- nres[nres %in% names(treequad)]
##     treeres <- treequad[, nres]
## ###################
# ### Incluir uma saida de objeto em lista com o treemiss e tree
    if(save_file)
    {
        line <- substr(unique(tree$quadrat),0,1)
        colu <- as.numeric(substr(unique(tree$quadrat),2,3))
        rlc <- tapply(colu, line, range)
        qname <- paste(paste(names(rlc), sapply(rlc, function(x){paste(x, collapse="_")}), sep=""), collapse = "x")
        
        dname <- file.path(dir_exp, qname)
        if(!dir.exists(file.path(dname, "baseData")))
        {
            #dir.create(dname)
            dir.create(file.path(dname, "baseData"), recursive=TRUE)
        }
#        file.rename(from = names.csv, to=  file.path(dname, "baseData", paste("audPeic",c("",form_rep), qname,".csv", sep="")))
        if(dir.exists(file.path(base_dir, "media")) & file.path(base_dir, "media") != file.path(dname, "media"))
            {
                media.names <- list.files(file.path(base_dir, "media"))
                if(!dir.exists(file.path(dname, "media")))
                {
                    dir.create(file.path(dname, "media"), recursive=TRUE)
                }
                if(file.path(base_dir, "media") != file.path(dname, "media"))
                {
                    file.rename(from = file.path(base_dir, "media", media.names), to = file.path(dname, "media", media.names))
                    unlink(file.path(base_dir, "media"), recursive = TRUE)
                }
            }
        
##         if(sum(grepl("miss", form_rep)) != 0)
##         {
##             write.table(treequad, file.path(dname, paste("tree",qname,".txt", sep="")), sep="\t", row.names=FALSE)
##             write.table(treeres, file.path(dname, paste("treeResumo",qname,".txt", sep="")), sep="\t", row.names=FALSE)
##             write.table(treemiss, file.path(dname, paste("treeMiss",qname,".txt", sep="")), sep="\t", row.names=FALSE)
##         }else{
##             write.table(treequad, file.path(dname, paste("treeNoMiss",qname,".txt", sep="")), sep="\t", row.names=FALSE)
##             write.table(treeres, file.path(dname, paste("treeResumoNoMiss",qname,".txt", sep="")), sep="\t", row.names=FALSE)
##          }
        write.table(tree, file.path(dname, paste("aud_tree", qname,".txt", sep="")), sep="\t", row.names=FALSE)
    }
## #
        ## INCLUIR UMA SAIDA DE LEITURA DE DADOS EM UM DIRETORIO ESPECIFICO
invisible(tree)
}
###



####################
## Presta Conta OKD
####################
##' Create html accountability report from prestaConta okd collect form data 
##'
##' @param dataDir character string, directory where a odk briefcases exports csv files.
##' @param dataName character string, odk briefcase data exported prefix file name 
##' @param dirExp character string, directory where html report and data files will be created.
##' @param valPar numeric vector length 2. Value paid for assistant and lider field workers
##' @param valFixo data frame with 3 variables, name of field workers,  job role  and  montly paiment.  
##' @param pagaFixo logic. Should include fix montly payment (valFixo) in the report? 
##' @param saveFile logic. Should save report data? 
##' @param fileRmd character string indicating the location of rmd template file. 

##' @return 'prestaConta' returns html report file and can save data in csv format. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @examples
##' 
##' \dontrun{
##' prestaConta(dataDir = getwd(), dataName="prestaConta", dirExp = getwd(), valParc = c(70,100), valFixo = data.frame(nome= c("Renan"), cargo = c("coordena"),valor = c(1500)), pagaFixo = FALSE,  saveFile=FALSE, fileRmd = system.file("rmd", "prestaConta.rmd", package = "Rppsp"))
##' }
##' 
##' @export 
##'
##'
##'
prestaConta <- function(dataDir, dataName="prestaConta", dirExp = getwd(), valParc = c(70,100), valFixo = data.frame(nome= c("Renan"), cargo = c("coordena"),valor = c(1500)), pagaFixo = FALSE,  saveFile=TRUE, fileRmd = system.file("rmd", "prestaConta.rmd", package = "Rppsp"))
{
    ifnum0 <- function(x){ifelse(length(x) == 0, 0, sum(x))} 
    require(rmarkdown)
    require(knitr)
    pc <- read.table(file.path(dataDir, paste(dataName, "-event_fin.csv", sep="")), header=TRUE, as.is=TRUE, sep=",", quote = '"')
    pc[is.na(pc)] <- ""
    pc$nEv <- 1:nrow(pc)
    ini <- read.table(file.path(dataDir, paste(dataName, ".csv", sep="")), header=TRUE, as.is=TRUE, sep=",", quote = '"' )
#str(ini)
    saldoIni <- ini[,grep("start_month.saldo", names(ini))]
    names(saldoIni) <-  sapply((strsplit(names(saldoIni), "\\.")),  function(y){y[length(y)]})
    pcnames <- c('nEv','ev_ini.data_ev', 'ev_ini.type_ev', 'event_value.val_ev', 'event_value.cred_deb', 'event_value.val_real', 'ev_cash.foto_desp_cash', 'ev_cash.eq_desp_cash', 'ev_cash.desp_tipo_cash', 'ev_cash.cred_tipo_cash', 'ev_cash.val_saq_cash', 'ev_cash.obs_cash', 'ev_card.nota_sn_card', 'ev_card.foto_desp_card', 'ev_card.eq_desp_card', 'ev_card.desp_tipo_card', 'ev_card.obs_card', 'ev_bank.foto_trans_cc', 'ev_bank.eq_desp_bank', 'ev_bank.desp_tipo_bank', 'ev_bank.cred_tipo_cc', 'ev_bank.obs_bank', 'field_pago', 'field_pago_new',  'eq_field.eq_campo.eq_lider', 'eq_field.new_name.eq_new_lider', 'eq_field.eq_campo.eq_auxiliar', 'eq_field.new_name.eq_new_name', 'eq_field.parcela.quadrat', 'eq_field.parcela.nsubq', 'pago_eq')
    pcontas <- pc[, pcnames]
    lnames <- strsplit(names(pcontas), "\\.")
    newnames <-sapply( lnames, function(y){y[length(y)]})
    names(pcontas) <- newnames
######################
### juntando Eventos 
######################
## juntando tipo de eventos
    pcontas$tipoEv <- apply(pcontas[,c("desp_tipo_cash", "desp_tipo_card", "desp_tipo_bank") ], 1, paste, collapse="")
    pcontas$tipoEv[pcontas$tipoEv == ""] <- pcontas$type_ev[pcontas$tipoEv == ""]
## juntando comprovantes
    fotos <- pcontas[,grep("foto", names(pcontas))]
    fotos[is.na(fotos)] <- ""
    pcontas$comprova <- apply(fotos, 1, paste, collapse="")
    ## juntando observacoes
    obs <- pcontas[,grep("obs", names(pcontas))]
    obs[is.na(obs)]<- ""
    pcontas$obsEv <- apply(obs, 1, paste, collapse="")
#nchar(pcontas$obsEv)
## juntando despesas equipe
    pcontas$EqDesp <- apply(pcontas[,grep("eq_desp", names(pcontas))], 1, paste, collapse="")
#### novos nomes de equipe ########
    pcontas$eq_lider[pcontas$eq_lider=="new_lider"] <- pcontas$eq_new_lider[pcontas$eq_lider=="new_lider"]
    pcontas$eq_auxiliar[pcontas$eq_auxiliar=="new_aux"] <- pcontas$eq_new_name[pcontas$eq_auxiliar=="new_aux"]
    pcontas$field_pago[pcontas$field_pago=="new_aux"] <- pcontas$field_pago_new[pcontas$field_pago=="new_aux"]
#names(pcontas)
    eventos <- pcontas[, c("nEv","data_ev", "type_ev", "cred_deb", "val_real", "EqDesp", "eq_lider", "eq_auxiliar","eq_new_name","quadrat", "nsubq", "tipoEv", "comprova","obsEv")]
    dateInt <- range(as.Date(eventos$data_ev, format="%b %d, %Y"))
    dateIni <- format(dateInt[1], "%d de %B de %Y")
    dateEnd <- format(dateInt[2], "%d de %B de %Y")
############################################
## resumo de evento de despesa (sem pessoal)
############################################
    resEv <- pcontas[pcontas$tipoEv !="field", c("nEv", "data_ev", "cred_deb","type_ev", "tipoEv", "val_real","EqDesp","obsEv", "comprova")]
    resEv$obsEv <- substr(resEv$obsEv, 1, 20)
    resEv$comprova[resEv$comprova != ""] <- paste('<a href ="', file.path(dataDir, resEv$comprova[resEv$comprova != ""]), '">link</a>')
#######################################    
## SEPARANDO EVENTO CREDITO E DEBITO ##
#######################################
    credData<- resEv[resEv$cred_deb =="cred",]
    debData <- resEv[resEv$cred_deb == "deb",]
### ADIANTAMENTO EQUIPE
    adEq <- pcontas[pcontas$tipoEv =="pago_eq" & pcontas$EqDesp != "all", c("tipoEv", "val_ev", "EqDesp","field_pago","pago_eq","obsEv")]
######################################################
## Aqui tirando o nome do evento do campo observacao
######################################################
   # nomeAd <-  adEq$field_pago
    if(nrow(adEq)>0)
    {
        adVal <- tapply(adEq$val_ev,toupper(adEq$field_pago), sum)
        adData <- data.frame(nome = names(adVal), adianta = as.numeric(adVal), stringsAsFactors = FALSE) 
        adData <- adData[order(adData$nome),]
    }else{adData <- NULL}
## Agregando debito e debito por type_ev
    tipoTrans <- paste(resEv$cred_deb, resEv$type_ev, sep="_")
    totalTipo <- aggregate(resEv$val_real, list(tipo=tipoTrans), sum)
    names(totalTipo)[2] <- "valor"
## Debitos separados entre equipe e outros
    debTotal <- sum(debData$val_real)
    debSemEq <- ifnum0(debData[debData$tipoEv != "pago_eq" ,'val_real'])
    pagoAdEq <- ifnum0(debData[debData$tipoEv == "pago_eq" ,'val_real'])
    resDeb <- tapply(debData$val_real, debData$tipoEv, sum)
###############################################################
### contando os quadrats feitos por evento
#### incluindo nome de novos integrantes como lider ou auxiliar
################################################################    
    parc <- pcontas[pcontas$tipoEv =="field", c("nEv", "data_ev", "eq_lider", "eq_auxiliar", "quadrat", "nsubq")  ]
    if(nrow(parc)==0)
    {
        parc[1,] = 0
        
    }    
    parc$nsubq <- as.numeric(parc$nsubq)
    #parc$eq_lider[parc$eq_lider =="new_lider"] <- parc$eq_new_name[parc$eq_lider =="new_lider"]
    #parc$eq_auxiliar[parc$eq_auxiliar =="new_aux"] <- parc$eq_new_name[parc$eq_auxiliar =="new_aux"]
## valor por evento lider e aux
    parc$val_lider <- (parc$nsubq) * (valParc[1]/16)
    parc$val_aux <- (parc$nsubq) * (valParc[2]/16)
## organizando os dados
    parc_lider <- parc[,  !(names(parc) %in% c("eq_auxiliar", "val_aux"))]
    names(parc_lider)[c(3,6)] <- c("equipe", "valor")
    parc_lider$cargo <- "lider"
    parc_aux <- parc[,  !(names(parc) %in% c("eq_lider", "val_lider"))]
    names(parc_aux)[c(3, 6)] <- c("equipe", "valor")
    parc_aux$cargo <- "auxiliar"
    eqCampo <- rbind(parc_lider, parc_aux)
    eqCampo$equipe <- tolower(eqCampo$equipe)
    eqCampo$valor <- round(eqCampo$valor,2)
    eqCampo<- eqCampo[order(eqCampo$equipe),]
####################
## Resumo por pessoa
####################
    resEq <- aggregate(eqCampo$valor, list(nome=eqCampo$equipe, cargo = eqCampo$cargo), sum)
    names(resEq)[3] <-"valor"
    resEq$totalSubq <- aggregate(eqCampo$nsubq,list(nome=eqCampo$equipe, cargo = eqCampo$cargo), sum)$x
    quads <- aggregate(eqCampo$quadrat,list(nome=eqCampo$equipe, cargo = eqCampo$cargo), function(x){ paste(sort(unique(x)), collapse = ", ")})$x
#ncharquad <- nchar(quads)
    resEq$nQuads <- round(resEq$totalSubq/16,2)
    resEq$nome <- toupper(resEq$nome)
    resEq$valor <- 0
    resEq$valor[resEq$cargo =="auxiliar"] <- round(resEq$totalSubq[resEq$cargo =="auxiliar"] * 70/16,2)
    resEq$valor[resEq$cargo =="lider"] <- round(resEq$totalSubq[resEq$cargo =="lider"] * 100/16,2)
    resEq$quads <- quads
############################
## Calculo Renan coordenacaoo
## Para o Mes de Fev combinado 1500 fixos + parcelas feitas
## A partir de Marco 1000 Renan, 250 Haron e Carlota
###########################
## nRenan <- sum(parc$nsubq[parc$eq_lider != "renan" & parc$eq_auxiliar != "renan" ])/16
## valorCoord <-  round(nRenan*20, 2)
## dadoCoord <- data.frame(nome= c("RENAN","RENAN"), cargo = c("coordena", "fixo"),valor = c(valorCoord, 2000))
    if(pagaFixo)
    {
        valFixo$nome <- toupper(valFixo$nome)
        valFixo$totalSubq = ""
        valFixo$nQuads = ""
        valFixo$quads= ""
        resEq <- rbind(resEq, valFixo)
    }
    resPes <- tapply(resEq$valor, resEq$nome, sum)
    resPes <- data.frame(nome = names(resPes), valor = as.numeric(resPes))
    eqTotal <- sum(resPes$valor)         
    fimEq <- resPes
    fimEq$adianta <- 0
    fimEq$adianta[match(adData$nome, fimEq$nome)] <- adData$adianta
    fimEq$transf <- fimEq$valor -  fimEq$adianta
###################################
### totais Gerais e Diarias Fapesp
###################################
    gastoTotal <- (-1* debSemEq) + eqTotal
    ndia <- ceiling(gastoTotal/380)
    ndiapes <- ceiling(ndia/2) ## aqui serao so Renan e Haron assinando. O Danilo saiu fora
### Totais Gerais
    totalCash <- saldoIni$saldo_cash_ini + ifnum0(totalTipo$valor[totalTipo$tipo =="cred_cash"])
    totalBank <- saldoIni$saldo_cc_ini + ifnum0(totalTipo$valor[totalTipo$tipo =="cred_bank"])
### Parcelas
    npar <- sum(parc$nsubq)/16
## Saldos Atuais
    saldoCash <- totalCash + ifnum0(totalTipo$valor[totalTipo$tipo =="deb_cash"])
    saldoConta <- totalBank + ifnum0(totalTipo$valor[totalTipo$tipo =="deb_bank"]) + ifnum0(totalTipo$valor[totalTipo$tipo =="deb_card"]) - ifnum0(credData$val_real[credData$tipoEv =="cash"])
    saldoTotal <- saldoCash + saldoConta
    pagarEq <-sum(fimEq$transf[fimEq$transf>0])

filePC <- file.path(dirExp, paste("relatorio", format(Sys.Date(), format= "%d%b%y"),".html", sep= "") )
render( "/home/aao/Ale2016/AleProjetos/Rppsp/inst/rmd/prestaConta.rmd", output_file = filePC )
    if(saveFile)
    {
        fileGeral <- file.path(dirExp, paste("eventosGeral", format(Sys.Date(), format= "%d%b%y"),".csv", sep= "") )
        write.table(eventos, file=fileGeral, row.names=FALSE, sep= "," )
        fileEquipe <- file.path(dirExp, paste("eventosEquipe", format(Sys.Date(), format= "%d%b%y"),".csv", sep= "") )
        write.table(eqCampo, file=fileEquipe, row.names=FALSE, sep= "," )
    }    
}

