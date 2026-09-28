## data <- read.table("/home/aao/Ale2026/AleProjetos/PPEIC/dados/dadosCenso2019/censoPeic2019.csv", header = TRUE, sep = ",")
## splitX = 5; splitY = 5; maxX=20; maxY = 20; subPlotCodeX = LETTERS[1:16]; subPlotCodeY= sprintf("%02d", 0:15)
#source("R/quadrat.R")

#' Prepare Last Census Data for ODK Forms
#'
#' Processes and formats data from the last forest census to be used in
#' Open Data Kit (ODK) forms. It validates/creates required columns, computes
#' local subquadrat coordinates, absolute plot coordinates (\code{gx}, \code{gy}),
#' and ODK tag keys.
#'
#' @param data A \code{data.frame} containing the census records. It must include
#'   the following fields:
#'   \describe{
#'     \item{quad}{Subplot or quadrat code where the individual or stem is located.}
#'     \item{tag}{Identification number or code of the tree/stem.}
#'     \item{dbh}{Diameter at breast height of the stem in millimeters (mm).}
#'     \item{ht}{Tree height in meters.}
#'     \item{dx}{X-coordinate (m) of tree location within the quadrat (column axis).}
#'     \item{dy}{Y-coordinate (m) of tree location within the quadrat (row axis).}
#'     \item{fam}{Botanical family.}
#'     \item{gen}{Botanical genus. If missing, extracted from \code{species}.}
#'     \item{species}{Species name (genus and specific epithet).}
#'     \item{nstem}{Number of stems for the tree; can be \code{NA} or omitted if records are per-stem.}
#'     \item{status}{Status code of the individual/stem: \code{"A"} (alive), \code{"D"} (dead), or \code{"M"} (missing).}
#'     \item{pom}{Point of measurement for DBH in meters. If missing, defaults to 1.3.}
#'     \item{date}{Date of measurement in the last census.}
#'   }
#' @param splitX Numeric. Dimension of the subquadrat along the X-axis (default: 5).
#' @param splitY Numeric. Dimension of the subquadrat along the Y-axis (default: 5).
#' @param maxX Numeric. Maximum dimension of the quadrat along the X-axis (default: 20).
#' @param maxY Numeric. Maximum dimension of the quadrat along the Y-axis (default: 20).
#' @param subPlotCodeX Character vector. Codes used along the X-axis of the plot grid (default: \code{LETTERS[1:16]}).
#' @param subPlotCodeY Character vector. Codes used along the Y-axis of the plot grid (default: \code{sprintf("\%02d", 0:15)}).
#' @param dirData Character. Directory path where the output CSV file will be saved (default: \code{getwd()}).
#' @param saveFile Logical. If \code{TRUE} (default), writes the processed data frame to \code{lastCensus.csv} in \code{dirData}.
#'
#' @return A \code{data.frame} with the original census columns augmented with subquadrat
#'   indices, global coordinates (\code{gx}, \code{gy}), DBH in cm (\code{dbhcm}), and ODK tag keys (\code{tag_key}).
#'   Returned invisibly.
#' @export
lastCensusODK <- function(data, splitX = 5, splitY = 5, maxX=20, maxY = 20, subPlotCodeX = LETTERS[1:16], subPlotCodeY= sprintf("%02d", 0:15), dirData = getwd(), saveFile = TRUE)
{
    nd <- names(data)
    if((!"gen" %in% nd) & "species" %in% nd)
    {
        data$gen <- sapply(strsplit(data$species, " "), function(x){x[[1]]})
    }
    if((!"pom" %in% nd))
    {
        data$pom <- 1.3
    }
    data$dbhcm <- data$dbh/10
    qData <- splitPlot(dx = data$dx, dy = data$dy, splitX = splitX, splitY = splitY, maxX = maxX, maxY = maxY)
    data$qx <- data$dx - as.integer(qData$qX)
    data$qy <- data$dy - as.integer(qData$qY)
    data$subquad <- paste(data$quad, qData$qXY, sep = "_")
    subPlotXY <- subplotXY(xcode = subPlotCodeX, ycode = subPlotCodeY, xsub = maxX, ysub= maxY)
    mplot <- match( data$quad, subPlotXY$subplot)
    data$xlim <- subPlotXY$xlim[mplot]
    data$ylim <- subPlotXY$ylim[mplot]
    data$gx <- data$dx + data$xlim
    data$gy <- data$dy + data$ylim
    data$tag_key <- paste("tag_", data$tag, ".1", sep ="")
    data <- data[order(data$tag), ]
    if(saveFile)
    {
        write.table(data, file.path(dirData,"lastCensus.csv"), row.names = FALSE, sep = ",")

    }
    invisible(data)
}

#' Generate Species List for ODK Selection
#'
#' Extracts and formats a comprehensive list of botanical families, genera, and
#' species from processed census data, adding placeholder entries for unidentified
#' taxa (\code{sp_indet}, \code{gen_indet}, and \code{fam_indet}).
#'
#' @param data A \code{data.frame} output from \code{\link{lastCensusODK}}, containing
#'   at least \code{species}, \code{fam}, and \code{gen} columns.
#' @param dirData Character. Directory path where the output file will be saved (default: \code{getwd()}).
#' @param saveFile Logical. If \code{TRUE} (default), writes the species list to \code{splist.csv} in \code{dirData}.
#'
#' @return A \code{data.frame} with columns \code{fam}, \code{gen}, and \code{species}. Returned invisibly.
#' @export
spList <- function(data, dirData = getwd(), saveFile = TRUE)
{
    uniqSp <- unique(data[,c("species", "fam")])
    uniqSp <- uniqSp[ - grep("indet", uniqSp$species),]
    uniqSp$gen <- sapply(strsplit(uniqSp$species, split =  " "), FUN = function(x){x[1]})
    uniqSp <- uniqSp[order(uniqSp$fam, uniqSp$gen),]
    fam =  sort(unique(uniqSp$fam))
    famIndet <- data.frame(fam = fam, gen = "gen_indet", species = paste(substr(fam,1,4), "sp_indet", sep = "_"))
    gen <- unique(uniqSp[,c("fam", "gen")])
    gen$species <- paste(gen$gen, "sp_indet")
    splist <- rbind(c(fam = "fam_indet", gen = "gen_indet", species = "sp_indet"), famIndet, gen, uniqSp)
    if(saveFile)
    {
        write.table(splist, file.path(dirData,"splist.csv"), row.names = FALSE)
    }
    invisible(splist)
}

#' Generate Subquadrat Tag List
#'
#' Aggregates tree tags grouped by subquadrat identifier.
#'
#' @param data A \code{data.frame} output from \code{\link{lastCensusODK}}, containing
#'   the columns \code{subquad} and \code{tag}.
#' @param dirData Character. Directory path where the output file will be saved (default: \code{getwd()}).
#' @param saveFile Logical. If \code{TRUE} (default), writes the table to \code{subqtags.csv} in \code{dirData}.
#'
#' @return A \code{data.frame} with two columns: \code{subq} (subquadrat identifier)
#'   and \code{tags} (space-separated tag numbers within each subquadrat). Returned invisibly.
#' @export
subqTags <- function(data, dirData = getwd(), saveFile = TRUE)
{
    subq <- sort(unique(data$subquad))
    subqtags <- data.frame(subq= subq, tags = NA)
    nsubq <- length(subq)
    for(i in 1:nsubq)
    {
        sq <- subqtags$subq[i]
        subqtags$tags[i] <- paste(data$tag[which(data$subquad == sq)], collapse = " ")
    }
    if(saveFile)
    {
        write.table(subqtags, file.path(dirData,"subqtags.csv"), row.names = FALSE, sep = ",")
    }
    invisible(subqtags)
}

## source("R/quadrat.R")

#' Build Subquadrat Mapping Data with Buffer
#'
#' Filters and extracts tree coordinates and attributes per subquadrat including
#' a buffer zone around the borders. This list is commonly used to generate SVG maps
#' for field orientation in ODK Collect.
#'
#' @param data A \code{data.frame} output from \code{\link{lastCensusODK}} containing
#'   \code{quad}, \code{xlim}, \code{ylim}, \code{gx}, \code{gy}, \code{tag}, \code{dbh}, and \code{status}.
#' @param subplotCodes Character vector. Subplot codes to include (default: all unique \code{quad} in \code{data}).
#' @param splitX Numeric. Dimension of the subquadrat along the X-axis (default: 5).
#' @param splitY Numeric. Dimension of the subquadrat along the Y-axis (default: 5).
#' @param maxX Numeric. Maximum dimension of the quadrat along the X-axis (default: 20).
#' @param maxY Numeric. Maximum dimension of the quadrat along the Y-axis (default: 20).
#' @param buffer Numeric. Buffer zone width around the subquadrat borders in meters (default: 2).
#'
#' @return A named \code{list} where each element corresponds to a subquadrat and contains a \code{data.frame}
#'   with columns \code{tag}, \code{dx}, \code{dy}, \code{dbh}, and \code{status}. The list includes attributes
#'   \code{splitX}, \code{splitY}, \code{maxX}, \code{maxY}, and \code{buffer}.
#' @export
mapData <- function(data, subplotCodes = unique(data$quad), splitX = 5, splitY = 5, maxX = 20, maxY = 20, buffer = 2)
{
    subplotxy <- unique(data[, c("quad", "xlim", "ylim")])
    subplotxy <- subplotxy[order(subplotxy$quad, subplotxy$xlim, subplotxy$ylim),]
    names(subplotxy)[1] <- "subplot" 
    splitQuadXY <- splitPlotXY(subplotxy, splitX = splitX, splitY = splitY, maxX = maxX, maxY = maxY)
    subquads <- splitQuadXY[splitQuadXY$subplot %in% subplotCodes,]
    xmin <- subquads$xMin - buffer
    xmax <- subquads$xMin + splitX + buffer
    ymin <- subquads$yMin - buffer
    ymax <- subquads$yMin + splitY + buffer
    mapDataList <- list()
    for(i in 1: nrow(subquads))
    {
        treeTF <- data$gx >= xmin[i] & data$gx < xmax[i] & data$gy >= ymin[i] & data$gy < ymax[i]
        tags0 <- data$tag[treeTF]
        dx0 <- data[treeTF, "gx"] - subquads$xMin[i]
        dy0 <- data[treeTF, "gy"] - subquads$yMin[i]
        dbh0 <- data[treeTF, "dbh"]
        status0 <- data[treeTF, "status"]
        mapDataList[[i]] <- data.frame(tag = tags0, dx = dx0, dy = dy0, dbh = dbh0, status = status0)
    }
    names(mapDataList) <- paste(subquads$subplot, subquads$subquad, sep = "_")
    attr(mapDataList, 'splitX') <- splitX
    attr(mapDataList, 'splitY') <- splitY
    attr(mapDataList, 'maxX') <- maxX
    attr(mapDataList, 'maxY') <- maxY
    attr(mapDataList, 'buffer') <- buffer
    return(mapDataList)
}

#' Generate Subquadrat Key Mapping for ODK
#'
#' Creates a key lookup table matching subquadrat labels to ODK tag keys
#' from the mapping list generated by \code{\link{mapData}}.
#'
#' @param dataMap A named list produced by \code{\link{mapData}}.
#' @param dirData Character. Directory path where the output file will be saved (default: \code{getwd()}).
#' @param saveFile Logical. If \code{TRUE} (default), writes the table to \code{subqkey.csv} in \code{dirData}.
#'
#' @return A \code{data.frame} with columns \code{subq} and \code{tag_key}. Returned invisibly.
#' @export
subqKey <- function(dataMap, dirData = getwd(), saveFile = TRUE )
{
    tags <-  unlist(sapply(dataMap, function(x) as.integer(x$tag)))
    ntags <- sapply(tags, length)
    subqkey <- data.frame(subq = names(tags), tag_key = paste("tag_", tags, ".1", sep = ""))
    if(saveFile)
    {
        write.table(subqkey, file.path(dirData,"subqkey.csv"), row.names = FALSE)
    }
    invisible(subqkey)
}
