##############################
##' Functions to calculate subplot index, code and coordinates
##'
##' 'splitPlot' create a code for subquadrats of any dimension
##' 
##' 'subplotXY' calculate the x and y coordinates for subplots
##' 
##' 'xyMin' calculate x and y coordinates for a vector of suplots
##'
##' 'subplotIndex' calculate x and y coordinates for a vector of suplots
##'
##' @name subplot
##' @param dx x coordinate for subplot 
##' @param dy y coordinate inside subplots 
##' @param gx x coordinate for the all plot
##' @param gy y coordinate for the all plot 
##' @param sub.size size of the subplots
##' @param max.x maximum x coordinate
##' @param max.y maximum y coordinate  
##' @param plot.code subplot codes
##' @param splitX x size of subquadrats
##' @param splitY y size of subquadrats, default equal do 'splitX'
##' @param maxX  maximum potential value for 'dx' 
##' @param maxY  maximum potential value for 'dy'


##' @return return a new coordinates gx and gy or plot code in the format '0x0' representing origin for subplot
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso 
##' \url{http://labtrop.ib.usp.br}
##' @references \url{https://opendatakit.org/} 
##' @examples
##' 
##' \dontrun{
##' splitPlot(dx = runif(10, 0,20), dy = runif(10, 0, 20), split.x = 5)
##' xcode <-  LETTERS[1:16]
##' ycode <-  c(paste(0, 0:9, sep = ""), 10:15)
##' xyMin(subplot_code = paste(sample(xcode, size = 25, replace = TRUE), sample(ycode, size = 25, replace = TRUE), sep = ""))  
##' }
##' @rdname subplot
##' @export
splitPlot <- function(dx, dy, splitX = 5, splitY = splitX, maxX = 20, maxY = maxX)
{
    posX <- seq(splitX, maxX - splitX, by= splitX)
    posY <- seq(splitY, maxY - splitY, by= splitY)
    dX <- apply(sapply(posX, function(x){ifelse(dx >= x, splitX,0)}), 1, sum)
    dY <- apply(sapply(posY, function(x){ifelse(dy >= x, splitY,0)}), 1, sum)
    dXY <- paste(dX, "x",dY, sep="")
    dXY[grep("NA", dXY)] <- NA
    return(dXY) 
}
##' @rdname subplot
##' @export
subplotXY <- function(xcode = LETTERS[1:16], ycode= c(paste(0, 0:9, sep = ""), 10:15), xsub = 20, ysub = 20)
{
    nx <- length(xcode)
    ny <- length(ycode) 
    col <- rep(xcode, each = ny)
    lin <- rep(ycode, nx)
    subplot <- paste(col, lin, sep = "")
    xlim <- rep(seq(0, (nx - 1) * xsub, by = xsub), each = ny )
    ylim <- rep(seq(0, (ny - 1) * ysub, by = ysub), nx)
    return(data.frame(subplot, xlim, ylim))
}
##' @rdname subplot
##' @export
xyMin <- function(subplot_code, xymin = subplotXY())
{
    seq.code <- match(subplot_code, xymin$subplot)
    data.frame(subplot_code, xmin = xymin$xlim[seq.code], ymin = xymin$ylim[seq.code])
}

##' @rdname subplot
##' @export
splitPlotXY <- function(subplotxy, splitX = 5, splitY = splitX, maxX = 20, maxY = maxX)
{
    posX <- seq(0, maxX - splitX, by= splitX)
    posY <- seq(0, maxY - splitY, by= splitY)
    posXseq <- rep(posX, each = length(posY))
    posYseq <- rep(posY, length(posY))
    quadLabel <- paste("quad_", posXseq, "x", posYseq, sep = "")
    quadXY <- data.frame(subquad = quadLabel, qx = posXseq, qy = posYseq  )
    splitQuadXY<- merge(quadXY, subplotxy, all = TRUE)
    splitQuadXY$xMin <- splitQuadXY$xlim + splitQuadXY$qx
    splitQuadXY$yMin <- splitQuadXY$ylim + splitQuadXY$qy
    splitQuadXY <- splitQuadXY[,c("subplot", "subquad", "xMin", "yMin", "qx", "qy")]
    attr(splitQuadXY, 'splitX') <- splitX
    attr(splitQuadXY, 'splitY') <- splitY
    attr(splitQuadXY, 'maxX') <- maxX
    attr(splitQuadXY, 'maxY') <- maxY
    return(splitQuadXY)   
}
##' @rdname subplot
##' @export
splitPlotData <- function(censoData, subplotCodes, splitQuadXY, buffer = 2, dbh = "dbh", gx = "gx", gy = "gy", status = "status")
{
    splitX <- attr(splitQuadXY, 'splitX') 
    splitY <- attr(splitQuadXY, 'splitY') 
    maxX <- attr(splitQuadXY, 'maxX') 
    maxY <- attr(splitQuadXY, 'maxY')
    #buffer <- attr(splitQuadXY, 'buffer') 
    subquads <- splitQuadXY[splitQuadXY$subplot %in% subplotCodes,]
    xmin <- subquads$xMin - buffer
    xmax <- subquads$xMin + splitX + buffer
    ymin <- subquads$yMin - buffer
    ymax <- subquads$yMin + splitY + buffer
    subquadsList <- list()
    for(i in 1: nrow(subquads))
    {
        treeTF <- censoData$gx >= xmin[i] & censoData$gx < xmax[i] & censoData$gy >= ymin[i] & censoData$gy < ymax[i]
        tags0 <- censoData$tag[treeTF]
        dx0 <- censoData[treeTF, gx] - subquads$xMin[i]
        dy0 <- censoData[treeTF, gy] - subquads$yMin[i]
        dbh0 <- censoData[treeTF, dbh]
        status0 <- censoData[treeTF, status]
        subquadsList[[i]] <- data.frame(tag = tags0, dx = dx0, dy = dy0, dbh = dbh0, status = status0)
    }
    names(subquadsList) <- paste(subquads$subplot, subquads$subquad, sep = "")
    attr(subquadsList, 'splitX') <- splitX
    attr(subquadsList, 'splitY') <- splitY
    attr(subquadsList, 'maxX') <- maxX
    attr(subquadsList, 'maxY') <- maxY
    attr(subquadsList, 'buffer') <- buffer
    return(subquadsList)
}

## ANTIGA FUNCAO index.map. Nao sei pq esta comentada... 
## subplotIndex <- function(dx, dy, size.split = 5, max.size = 20)
## {
##     pos <- seq(size.split, max.size - size.split, by= size.split)
##     dy.ind <- apply(sapply(pos, function(x){ifelse(dy >= x, 1,0)}), 1, sum)
##     dx.ind <- rep(NA, length(dy.ind))
##     max.ind = 0
##     for(i in sort(unique(dy.ind)))
##     {
##         if(i%%2 == 0)
##         {
##             dx.ind[dy.ind==i] <- max.ind + rank(dx[dy.ind==i])
##         }
##         if(i%%2 != 0)
##         {
##             dx.ind[dy.ind==i] <- max.ind + rank(1/(dx[dy.ind==i]))
##         }
##      max.ind= max(dx.ind, na.rm=TRUE)   
##     }
##     return(dx.ind)
## }
