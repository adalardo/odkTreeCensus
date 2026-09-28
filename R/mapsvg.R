############################################
### Alexandre Adalardo 15 de outubro de 2018
### versao: 28 de setembro de 2026
#############################################
# permanent plot maps
#####################
##' @title Graphic of trees maped 
##' @description Map of trees from permanent plot based on cartesian coordinates 
##' @param mapData list with data for each subquadrat with a buffer zone. 
##' @param subPlotCode Character string that indetified subplot code.
##' @param svgSave logical, if TRUE the device is exported to a svg file. 
##' @param wd2save character string indicating the directory to save the svg file
##' @param dx name of the variável in censoData that contains the subplot x coordinate of the trees.
##' @param dy name of the variável in censoData that contains the subplot y coordenate of the trees.
##' @param tag character string with the name of the variável in censoData that contains the tag code that identifies trees.
##' @param status character string with the name of the variable with the status of the tree ("A" = alive, "D" = dead).
##' @param dbh character string with the name of the variable diameter at breast high.
##' @param mapSize size of the screen device window in inchs.
##' @param vpSize proportion of plot area in 'mapSize'.
##' @param fontSize numeric defining the font size.
##' @param diagonal logical, if TRUE the diagonals will be ploted. 
##' @return 'svgGrid' returns figure device and export a svg file. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link[gridSVG]{gridSVG}} 
##' \url{http://labtrop.ib.usp.br}
##' @examples
##' \dontrun{
##'  svgMap(dataplot, svgSave = FALSE)
##' }
##' @importFrom grid viewport pushViewport gpar grid.text grid.rect grid.xaxis grid.yaxis grid.circle grid.segments
##' @importFrom gridSVG gridsvg
##' @export
##'
#################################
svgMap <- function(mapData, subPlotCode = "A00", svgSave = TRUE, wd2save = file.path(getwd(), subPlotCode), dx = "dx", dy = "dy",  tag = "tag", dbh = "dbh", status= "status", mapSize = c(13, 13), vpSize = c(0.9, 0.9), fontSize = 12, diagonal = FALSE)
{
    if(! exists("mapData"))
    {
        stop( "Não existe o objeto com os dados da parcela")
    }
    options(warn = -1)
    splitX <- attr(mapData, 'splitX') 
    splitY <- attr(mapData, 'splitY') 
    maxX <- attr(mapData, 'maxX') 
    maxY <- attr(mapData, 'maxY')
    buffer <- attr(mapData, 'buffer') 
    subquadNames <- names(mapData)
    indData <- grep(subPlotCode, subquadNames)
    for(j in  indData)
    {
        subquad <- mapData[[j]]
        subXY <- as.numeric(strsplit(subquadNames[j], "_|x")[[1]][c(2,3)])
        tag_key <- paste("tag_", subquad[,tag], sep="")
##############################
## plot here
##############################
        gridSVG::gridsvg(name =  file.path(wd2save, paste("map", subquadNames[j],".svg",sep="")), uniqueNames=FALSE, width = mapSize[1], height = mapSize[2])
        vptop <- grid::viewport(y=0.9, width=0.8, height=0.2)
        grid::grid.text(x=0.5, y=0.9, paste("Unidade de Trabalho", subquadNames[j] ) ,vp= vptop, gp=grid::gpar(fontsize = fontSize + 5))
        vp <- grid::viewport(width = vpSize[1], height = vpSize[2], xscale = c(- buffer,  splitX + buffer), yscale= c( - buffer, splitY + buffer))
        grid::pushViewport(vp)
        grid::grid.rect(gp = grid::gpar(col = "black"))
        grid::grid.xaxis(seq(- buffer, splitX + buffer, by = splitX/5), at=seq( - buffer,  splitX + buffer, by = splitX/5), gp=grid::gpar(fontsize= fontSize))
        grid::grid.yaxis(seq(-buffer, splitY + buffer, by = splitY/5), at=seq( - buffer,  splitY + buffer, by = splitY/5), gp=grid::gpar(fontsize= fontSize))

        for(i in 1:nrow(subquad))
        {
            grid::grid.text(paste(subquad[i, tag]), x= subquad[i, dx]+log(subquad[i, dbh])/20 ,y=subquad[i, dy]+log(subquad[i, dbh])/15, default.units="native", gp = grid::gpar(fontsize = fontSize- 2))
            grid::grid.circle(x= subquad[i, dx],y=subquad[i, dy], r= log(subquad[i, dbh])/20, default.units="native", gp=grid::gpar(fill=ifelse(subquad[i, status]=="A" | subquad[i, status]=="AS" ,rgb(0,1,0, 0.5), rgb(0,0,1,0.5)), col="black"), name = tag_key[i])
        }
        
        grid::grid.segments(x0= c(0,0, 0, splitX) , y0 = c(0, 0, splitY, splitY) , x1 =c( splitX, 0, splitX, splitX),  y1= c(0, splitY,  splitY, 0), default.units="native", gp= grid::gpar(lty = 2))
        if(subXY[1] == subXY[2] & diagonal)
        {
           grid::grid.segments(x0= 0 , y0 = 0 , x1 = splitX,  y1=  splitY, default.units="native", gp= grid::gpar(lty = 2)) 
        }
        if((subXY[1] != subXY[2]) & diagonal)
        {
           grid::grid.segments(x0= 0 , y0 = splitY , x1 = splitX,  y1= 0, default.units="native", gp= grid::gpar(lty = 2)) 
        }
        grid::grid.text(c(subXY[1], subXY[1]+ splitX) , x=  c(0,  splitX), y= c(-0.2, -0.2), default.units="native", gp = grid::gpar(fontsize = fontSize, col="red"))
        grid::grid.text(c(subXY[2], subXY[2] + splitY) , y =  c(0, splitY), x= c(-0.2, -0.2), default.units="native", gp = grid::gpar(fontsize = fontSize, col = "red"))
        if(svgSave)
        {
            if(!dir.exists(wd2save))
            {
                dir.create(wd2save)
            }
            dev.off()
        }
    }
}
#######################################
## svgGrid
#######################################
##' @title Grid for mapping 
##' @description A figure to select mapping position
##' @param censoData data frame from permanent plot tree censo cartezian data. 
##' @param subPlotCode Character string that indetified subplot code.
##' @param subqSize size of subquadrat.
##' @param gridSize numeric vector. Size of the grid cell for mapping. 
##' @param svgSave logical, if TRUE the device is exported to a svg file. 
##' @param wd2save character string indicating the directory to save the svg file
##' @param dx name of the variável in censoData that contains the subplot x coordinate
##' @param dy name of the variável in censoData that contains the subplot y coordenate of the trees.
##' @param tag name of variable with tag code.
##' @param dbhcm name of variable with diameter at breast height in cm.
##' @param status name of the variable with the status of the tree ("A" = alive, "D" = dead).
##' @param subquad a character string of the name of the subquad variable, representing some subunit of the subplot.
##' @param mapSize size of the figure in inchs.
##' @param vpSize viewport size.
##' @param fontSize font size.
##' @param diagonal logical, if TRUE the diagonals will be plot. 
##' @return 'svgGrid' returns figure device and export a svg file. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link[gridSVG]{gridSVG}} 
##' \url{http://labtrop.ib.usp.br}
##' @examples
##' \dontrun{
##'  svgGrid(dataplot, svgSave = FALSE)
##' }
##'
##' @importFrom grid viewport pushViewport gpar grid.text grid.rect grid.xaxis grid.yaxis grid.circle grid.abline
##' @importFrom gridSVG gridsvg
##' @export
##'
#################################
svgGrid <- function(censoData, subPlotCode = "A00", subqSize = 5, gridSize = 0.2, svgSave = TRUE, wd2save = file.path(getwd(), subPlotCode), dx = "dx", dy = "dy",  tag = "tag", dbhcm = "dbhcm", status= "status", subquad = "subquad", mapSize = c(13,13), vpSize = c(0.8, 0.8), fontSize = 12, diagonal = FALSE)
{
    if(! exists("censoData"))
    {
        stop( "Não existe o objeto com os dados da parcela")
    }
    options(warn = -1)
    subqNames <- sort(unique(grep(subPlotCode, censoData[ , subquad], value = TRUE)))
    for(j in subqNames)
    {
        sqData <- censoData[censoData[,subquad] == j, ]
        sqxy <- as.numeric(strsplit(j, split= "_|x")[[1]][c(2,3)])
        sqData$sx <- sqData$dx - sqxy[1]
        sqData$sy <- sqData$dy - sqxy[2]
        if(nchar(subqSize) == 1)
        {
            subqNum <- paste(0, subqSize, sep = "")
        }
        else
        {
            subqNum = subqSize
        }
 
#############
## plot here
#############
        gridSVG::gridsvg(name = file.path(wd2save, paste("grid", subqNum,"_", j,".svg",sep="")) , uniqueNames=FALSE, width = mapSize[1], height = mapSize[2])
        vptop <- grid::viewport(y=0.9, width=0.9, height=0.2)
        grid::grid.text(x=0.5, y=0.9, paste(j,  "- grid de mapeamento", subqNum,  "x",subqNum,"m"), vp= vptop, gp=grid::gpar(fontsize = fontSize + 2))
        vp <- grid::viewport(width = vpSize[1], height = vpSize[2], xscale=c(0,subqSize), yscale=c(0, subqSize))
        grid::pushViewport(vp)
        grid::grid.rect(gp = grid::gpar(col = "black"))
        grid::grid.xaxis(at=seq(0, subqSize, by=.5), gp = grid::gpar(fontsize = fontSize, tcl = NA))
        grid::grid.yaxis(at=seq(0, subqSize, by=.5), gp = grid::gpar(fontsize = fontSize, tcl = NA))
        xseq = rep(seq(0.0, subqSize - gridSize, by = gridSize), each = subqSize/gridSize)
        yseq = rep(seq(0.0, subqSize - gridSize, by = gridSize), subqSize/gridSize)
        loc_key_10 = paste("x = ",sprintf("%1.1f", xseq), "; y = ", sprintf("%1.1f", yseq),"; ", sep="")
        grid::grid.circle(x= sqData$sx, y= sqData$sy, r= log(sqData[,dbhcm])/20, default.units="native", gp=grid::gpar(fill= c(rgb(0,0,1,0.5), rgb(0,1,0, 0.5))[grepl("A", sqData[,status]) + 1], col="black"))
        grid::grid.text(paste(sqData[, tag]), x= sqData[, "sx"]+ log(sqData[, dbhcm])/8 , y=sqData[, "sy"] + log(sqData[, dbhcm])/15, default.units="native", gp = grid::gpar( fontsize= fontSize - 2))
#####################
##  DIAGONAL:
#####################
        if(sqxy[1] == sqxy[2] & diagonal)
        {
            grid::grid.abline(gp = grid::gpar(lwd = 1.5, col = "blue") )
        }    
        if(sqxy[1] != sqxy[2] & diagonal)
        {
            grid::grid.abline(10, -1, gp = grid::gpar(lwd = 1.5, col = "blue") )
        }    
##############
## Grid
##############
        for(i in 1: length(loc_key_10))
        {
            grid::grid.rect(x = xseq[i]+0.1,y = yseq[i]+0.1, width =.2, height=.2, gp=grid::gpar(fill = rgb(0,1,0, .2),lwd =0.1),  default.units="native", name = loc_key_10[i])
        }
        if(svgSave)
        {
            if(!dir.exists(wd2save))
            {
                dir.create(wd2save)
            }
            dev.off()
        }    
    }
}
##############################
# audit plot maps
##############################
##############################
##' Map of trees from permanent plot based on cartesian coordinates 
##' 
##' @param audit auditory data frame from permanent plot tree censo data. 
##' @param quad a character string or factor representing some subunit of the permanent plot.
##' @param save.svg logical true to save a svg file
##' @param wd character string indicating directory towhere the file will be saved. 
##' @param dx between 0 to max.size . Tree mapping X coordinate.
##' @param dy between 0 to max.size . Tree mapping Y coordinate.
##' @param tag tag identifier variable name.
##' @param dap diameter at breast height variable name.
##' @param error error type variable name.
##' @param mapSize map size in inches.
##' @return 'mapsvg' returns tree mapped svg mapped with svg pattern ids. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link[gridSVG]{gridSVG}} 
##' \url{http://labtrop.ib.usp.br}
##' @examples
##' \dontrun{
##' dataplot <- data.frame(dx = runif(100, 0,20), dy = runif(100,0,20), quad = rep(paste("quad", 0:1, sep="_"), each=50), dap =rnbinom(100,10,0.5), status="A", tag = 1:100)
##' auditsvg(dataplot, save.svg = FALSE)
##' }
##' 
##' @importFrom grid viewport pushViewport gpar grid.text grid.rect grid.xaxis grid.yaxis grid.circle
##' @importFrom gridSVG grid.export
##'
##' @export
##' 
auditsvg <- function(audit, quad = "A00", save.svg = TRUE, wd = getwd(), dx = "new_dx2018", dy = "new_dy2018",  tag = "num_tag", dap = "dap2018", error = "errorType", mapSize = c(13,13))
{
    options(warn = -1)
    dataquad <- audit[audit$quadrat == quad,]
    xyna <- is.na(dataquad[,dx]) | is.na(dataquad[,dy])
    xy <- dataquad[, c(dx, dy)]
    xy[xyna, ] <- dataquad[xyna,c("old_dx", "old_dy")]
    dbh0 <- dataquad[,dap]
    dbh0[is.na(dbh0)] <- 10 
    tag_key <- paste("tag_", dataquad[,tag], sep="")
    tipo <- as.factor(dataquad[, error])
    utipo <- levels(tipo)
    ntipo <- length(utipo)
###########################
## plot here
###########################
    dev.new(width = mapSize[1], height = mapSize[2])
    vptop <- grid::viewport(y=0.9, width=0.8, height=0.2)
    grid::grid.text(x=0.5, y=0.9, paste(" Auditoria Parcela ", quad) ,vp= vptop, gp=grid::gpar(fontsize = 20))
    vp <- grid::viewport(width = 0.8, height = 0.8, xscale=c(0, 20), yscale=c(0, 20))
    grid::pushViewport(vp)
    grid::grid.rect(gp = grid::gpar(col = "black"))
    grid::grid.xaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=grid::gpar(fontsize=15))
    grid::grid.yaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=grid::gpar(fontsize=15))
    cols <- c(rgb(0,0,0, 0.3), rgb(0,1,0, 0.5), rgb(0,0,1, 0.5), rgb(1,1,0, 0.5), rgb(1,0,1, 0.5), rgb(0,1,1, 0.5))
    int <- 1
    for(i in 1:nrow(dataquad))
    {
        grid::grid.circle(x= xy[i, 1],y=xy[i, 2], r= log(dbh0[i])/10, default.units="native", gp=grid::gpar(fill= cols[tipo[i]], col="black"), name = tag_key[i])
        grid::grid.text(paste(dataquad[i, tag]), x= xy[i, 1] + (log(dbh0[i])/8) ,y = xy[i, 2] + (int *log(dbh0[i])/8), default.units="native", gp = grid::gpar(cex = 1.2))
        int = int * -1
    }
    grid::grid.circle(x= c(1.5, 6.5, 11.5, 16.5)[1:ntipo],y =-1.7, r= 0.3, default.units="native", gp=grid::gpar(fill= cols[1:ntipo], col="black"))
    grid::grid.text(utipo[1:ntipo], x = (c(1.5, 6.5, 11.5, 16.5)+ .5)[1:ntipo]   , y= -1.7,  gp = grid::gpar(cex = 1.2), default.units="native", just= "left")

    if(save.svg)
    {
        gridSVG::grid.export(file.path(wd, paste("auditmap",quad,".svg",sep="")) , uniqueNames=FALSE)
    }
#######################################
# plot end
#######################################        
}
##############################
# audit plot maps
##############################
##############################
##' Map of trees from permanent plot based on cartesian coordinates 
##' 
##' @param audit auditory data frame from permanent plot tree censo data. 
##' @param quad a character string or factor representing some subunit of the permanent plot.
##' @param save.svg logical true to save a svg file
##' @param wd character string indicating directory towhere the file will be saved. 
##' @param dx between 0 to max.size . Tree mapping X coordinate.
##' @param dy between 0 to max.size . Tree mapping Y coordinate.
##' @param tag tag identifier variable name.
##' @param dap diameter at breast height variable name.
##' @param error error type variable name.
##' @param mapSize map size in inches.
##' @return 'mapsvg' returns tree mapped svg mapped with svg pattern ids. 
## me' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link[gridSVG]{gridSVG}} 
##' \url{http://labtrop.ib.usp.br}
##'
##' \dontrun{
##' dataplot <- data.frame(dx = runif(100, 0,20), dy = runif(100,0,20), quad = rep(paste("quad", 0:1, sep="_"), each=50), dap =rnbinom(100,10,0.5), status="A", tag = 1:100)
##' ordersvg(dataplot, save.svg = FALSE)
##' }
##' 
##' @importFrom grid viewport pushViewport gpar grid.text grid.rect grid.xaxis grid.yaxis grid.circle grid.lines
##' @importFrom gridSVG grid.export
##'
##' @export
ordersvg <- function(audit, quad = "A00", save.svg = TRUE, wd = getwd(), dx = "new_dx2018", dy = "new_dy2018",  tag = "num_tag", dap = "dap2018", error = "errorType", mapSize = c(13,13))
{
    options(warn = -1)
    dataquad <- audit[audit$quadrat == quad,]
    xyna <- is.na(dataquad[,dx]) | is.na(dataquad[,dy])
    xy <- dataquad[, c(dx, dy)]
    xy[xyna, ] <- dataquad[xyna,c("old_dx", "old_dy")]
    dbh0 <- dataquad[, dap]
    dbh0[is.na(dbh0)] <- 10 
    tag_key <- paste("tag_", dataquad[,tag], sep="")
    tipo <- as.factor(dataquad[, error])
    utipo <- levels(tipo)
    ntipo <- length(utipo)
    idmap <- index.map(dx=xy[,dx], dy=xy[,dy])
    xy <- xy[order(idmap),]
    ntag <- nrow(xy)
###########################
## plot here
###########################
    dev.new(width = mapSize[1], height = mapSize[2])
    vptop <- grid::viewport(y=0.9, width=0.8, height=0.2)
    grid::grid.text(x=0.5, y=0.9, paste(" Auditoria Parcela ", quad) ,vp= vptop, gp=grid::gpar(fontsize = 20))
    vp <- grid::viewport(width = 0.8, height = 0.8, xscale=c(0, 20), yscale=c(0, 20))
    grid::pushViewport(vp)
    grid::grid.rect(gp = grid::gpar(col = "black"))
    grid::grid.xaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=grid::gpar(fontsize=15))
    grid::grid.yaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=grid::gpar(fontsize=15))
    cols <- c(rgb(0,0,0, 0.3), rgb(0,1,0, 0.5), rgb(0,0,1, 0.5), rgb(1,1,0, 0.5), rgb(1,0,1, 0.5), rgb(0,1,1, 0.5))
    int <- 1
    for(i in 1:ntag)
    {
        if(i < ntag)
        {
            grid::grid.lines(x = c(xy[i,1], xy[i+1, 1]), y = c(xy[i,2], xy[i+1, 2]), default.units="native", gp= grid::gpar(col=rgb(0,0,0,.2), lwd=3, lty=2))
        }
        grid::grid.circle(x= xy[i, 1],y=xy[i, 2], r= log(dbh0[i])/10, default.units="native", gp=grid::gpar(fill= cols[tipo[i]], col="black"), name = tag_key[i])
        grid::grid.text(paste(dataquad[i, tag]), x= xy[i, 1] + (log(dbh0[i])/8) ,y = xy[i, 2] + (int *log(dbh0[i])/8), default.units="native", gp = grid::gpar(cex = 1.2))
        grid::grid.text(as.character(i), x= xy[i, 1] ,y = xy[i, 2], default.units="native", gp = grid::gpar(cex = 1.5, col=rgb(1,1,1)))
        int = int * -1
    }
    grid::grid.circle(x= c(1.5, 6.5, 11.5, 16.5)[1:ntipo],y =-1.7, r= 0.3, default.units="native", gp=grid::gpar(fill= cols[1:ntipo], col="black"))
    grid::grid.text(utipo[1:ntipo], x = (c(1.5, 6.5, 11.5, 16.5)+ .5)[1:ntipo]   , y= -1.7,  gp = grid::gpar(cex = 1.2), default.units="native", just= "left")

    if(save.svg)
    {
        gridSVG::grid.export(file.path(wd, paste("ordermap",quad,".svg",sep="")) , uniqueNames=FALSE)
    }
#######################################
# plot end
#######################################        
}

selSubq <- function(xmax = 20, ymax = 20, subqX= 5, subqY = 5, mapSize = c(10,10), fontSize = 14, vpSize = c(0.8, 0.8), wd2save = getwd())
{
    options(warn = -1)
    if(nchar(subqX) == 1)
    {
        subqNum <- paste(0, subqX, sep = "")
    }
    else
    {
        subqNum = subqX
    }
    gridSVG::gridsvg(name = file.path(wd2save, paste("subPar", subqNum,".svg",sep="")) , uniqueNames=FALSE, width = mapSize[1], height = mapSize[2])
    vp <- grid::viewport(width = vpSize[1], height = vpSize[2], xscale=c(0,xmax), yscale=c(0, ymax))
    grid::pushViewport(vp)
    grid::grid.rect(gp = grid::gpar(col = "black"))
    grid::grid.xaxis(at=seq(0, xmax, by = subqX), gp = grid::gpar(fontsize = fontSize))
    grid::grid.yaxis(at=seq(0, ymax, by= subqY), gp = grid::gpar(fontsize = fontSize))
    xseq = rep(seq(0, xmax - subqX, by = subqX), each = xmax/subqX)
    yseq = rep(seq(0, xmax - subqY, by = subqY), ymax/subqY)
    quadkey = paste("subq_", xseq, "x", yseq, sep="")
##############
## Grid
##############
    for(i in 1: length(xseq))
    {
        grid::grid.rect(x = xseq[i] + subqX/2, y = yseq[i]+ subqY/2, width = subqX , height= subqY, gp=grid::gpar(fill = rgb(0, .5, 0, 0.8), lwd =0.1),  default.units="native", name = quadkey[i])
    }
    if(!dir.exists(wd2save))
    {
        dir.create(wd2save)
    }
    dev.off()
    svg_lines <- readLines(file.path(wd2save, paste("subPar", subqNum,".svg",sep="")))
    svg_lines <- gsub('id="([^"]+?)\\.[0-9]+(\\.[0-9]+)*"', 'id="\\1"', svg_lines)
    writeLines(svg_lines, file.path(wd2save, paste("subPar", subqNum,".svg",sep="")) )
}
