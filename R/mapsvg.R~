############################################
### Alexandre Adalardo 15 de outubro de 2018
### versao: 15 de abril de 2025
#############################################
# permanent plot maps
#####################
##' @title Graphic of trees maped 
##' @description Map of trees from permanent plot based on cartesian coordinates 
##' @param mapData list with data for each subquadrat with a buffer zone. 
##' @param subPlotCode Character string that indetified subplot code.
##' @param svgSave logical, if TRUE the device is exported to a svg file. 
##' @param wd2save character string indicating the directory to save the svg fi
##' @param dx name of the variável in censoData that contains the subplot x coord
##' @param dy name of the variável in censoData that contains the subplot y coordenate of the trees.
##' @param tag character string with the name of the variável in censoData that contains the tag code that identifies trees.
##' @param status character string with the name of the variable with the status of the tree ("A" = alive, "D" = dead).
##' @param dbh character string with the name of the variable diameter at breast high.
##' @param subquad a character string of the name of the subquad variable, representing some subunit of the subplot.
##' @param mapSize size of the screen device window in inchs.
##' @param vpSize proportion of plot area in 'mapSize'.
##' @param fontSize numeric defining the font size.
##' @param diagonal logical, if TRUE the diagonals will be ploted. 
##' @return 'svgGrid' returns figure device and export a svg file. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link{gridSVG}} 
##' \url{http://labtrop.ib.usp.br}
##' @examples
##' \dontrun{
##'  svgMap(dataplot, svgSave =FALSE)
##' }
##' @import grid
##' @import gridSVG
##' @export
##'
#################################
svgMap <- function(mapData, subPlotCode = "A00", svgSave = TRUE, wd2save = file.path(getwd(), subPlotCode), dx = "dx", dy = "dy",  tag = "tag", dbh = "dbh", status= "status", mapSize = c(13,13), vpSize = c(0.9, 0.9), fontSize = 12, diagonal = FALSE)
{
    if(! exists("mapData"))
    {
        stop( "Não existe o objeto com os dados da parcela")
    }
    options(warn = -1)
    ## library("grid")
    ## library("gridSVG")
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
        arv_key <- paste("arv_", subquad[,tag], sep="")
##############################
## plot here
##############################
        dev.new(width = mapSize[1], height = mapSize[2])
        vptop<- viewport(y=0.9, width=0.8, height=0.2)
        grid.text(x=0.5, y=0.9, paste("Unidade de Trabalho", subquadNames[j] ) ,vp= vptop, gp=gpar(fontsize = fontSize + 5))
        vp <- viewport(width = vpSize[1], height = vpSize[2], xscale = c(- buffer,  splitX + buffer), yscale= c( - buffer, splitY + buffer))
        pushViewport(vp)
        grid.rect(gp = gpar(col = "black"))
        grid.xaxis(seq(- buffer, splitX + buffer, by = splitX/5), at=seq( - buffer,  splitX + buffer, by = splitX/5), gp=gpar(fontsize= fontSize))
        grid.yaxis(seq(-buffer, splitY + buffer, by = splitY/5), at=seq( - buffer,  splitY + buffer, by = splitY/5), gp=gpar(fontsize= fontSize))

        for(i in 1:nrow(subquad))
        {
            grid.text(paste(subquad[i, tag]), x= subquad[i, dx]+log(subquad[i, dbh])/20 ,y=subquad[i, dy]+log(subquad[i, dbh])/15, default.units="native", gp = gpar(fontsize = fontSize- 2))
            grid.circle(x= subquad[i, dx],y=subquad[i, dy], r= log(subquad[i, dbh])/20, default.units="native", gp=gpar(fill=ifelse(subquad[i, status]=="A" | subquad[i, status]=="AS" ,rgb(0,1,0, 0.5), rgb(0,0,1,0.5)), col="black"), name = arv_key[i])
        }
        
        grid.segments(x0= c(0,0, 0, splitX) , y0 = c(0, 0, splitY, splitY) , x1 =c( splitX, 0, splitX, splitX),  y1= c(0, splitY,  splitY, 0), default.units="native", gp= gpar(lty = 2))
        if(subXY[1] == subXY[2] & diagonal)
        {
           grid.segments(x0= 0 , y0 = 0 , x1 = splitX,  y1=  splitY, default.units="native", gp= gpar(lty = 2)) 
        }
        if((subXY[1] != subXY[2]) & diagonal)
        {
           grid.segments(x0= 0 , y0 = splitY , x1 = splitX,  y1= 0, default.units="native", gp= gpar(lty = 2)) 
        }
        grid.text(c(subXY[1], subXY[1]+ splitX) , x=  c(0,  splitX), y= c(-0.2, -0.2), default.units="native", gp = gpar(fontsize = fontSize, col="red"))
        grid.text(c(subXY[2], subXY[2] + splitY) , y =  c(0, splitY), x= c(-0.2, -0.2), default.units="native", gp = gpar(fontsize = fontSize, col = "red"))
        if(svgSave)
        {
            if(!dir.exists(wd2save))
            {
                dir.create(wd2save)
            }
            grid.export(file.path(wd2save, paste("map", subquadNames[j],".svg",sep="")) , uniqueNames=FALSE)
        }
##########
# plot end
##########      
    }
}
#######################################
## svgGrid
#######################################
##' @title Grid for mapping 
##' @description A figure to select mapping position
##' @param censoData data frame from permanent plot tree censo cartezian data. 
##' @param subPlotCode Character string that indetified subplot code.
##' @param gridSize numeric vector. Size of the grid cell for mapping. 
##' @param svgSave logical, if TRUE the device is exported to a svg file. 
##' @param wd2save character string indicating the directory to save the svg fi
##' @param dx name of the variável in censoData that contains the subplot x coord
##' @param dy name of the variável in censoData that contains the subplot y coordenate of the trees.
##' @param status  name of the variable with the status of the tree ("A" = alive, "D" = dead).
##' @param status the size of the subquadrat to map in each graphic.
##' @param subquad a character string of the name of the subquad variable, representing some subunit of the subplot.
##' @param mapSize size of the figure in inchs.
##' @param diagonal logical, if TRUE the diagonals will be plot. 
##' @return 'svgGrid' returns figure device and export a svg file. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link{gridSVG}} 
##' \url{http://labtrop.ib.usp.br}
##' @examples
##' \dontrun{
##'  svgGrid(dataplot, svgSave = FALSE)
##' }
##'
##' @import grid
##' @import gridSVG
##' @export
##'
#################################
svgGrid <- function(censoData, subPlotCode = "A00", subqSize = 10, gridSize = 0.2, svgSave = TRUE, wd2save = file.path(getwd(), subPlotCode), dx = "dx", dy = "dy",  tag = "tag", dbhcm = "dbhcm", status= "status", subquad = "subquad", mapSize = c(13,13), vpSize = c(0.9, 0.9), fontSize = 12, diagonal = FALSE)
{
    if(! exists("censoData"))
    {
        stop( "Não existe o objeto com os dados da parcela")
    }
    options(warn = -1)
    ## library("grid")
    ## library("gridSVG")
    subqNames <- sort(unique(grep(subPlotCode, censoData[ , subquad], value = TRUE)))
    for(j in subqNames)
    {
        sqData <- censoData[censoData[,subquad] == j, ]
        sqxy <- as.numeric(strsplit(j, split= "_|x")[[1]][c(2,3)])
        sqData$sx <- sqData$dx - sqxy[1]
        sqData$sy <- sqData$dy - sqxy[2]
#############
## plot here
#############
        dev.new( width = mapSize[1], height = mapSize[2]) #, fontsize = 12)
        vptop<- viewport(y=0.9, width=0.9, height=0.2)
        grid.text(x=0.5, y=0.9, paste(j,  "- grid de mapeamento", subqSize,  "x",subqSize,"m"), vp= vptop, gp=gpar(fontsize = fontSize + 5))
    vp <- viewport(width = vpSize[1], height = vpSize[2], xscale=c(0,10), yscale=c(0,10))
        pushViewport(vp)
        grid.rect(gp = gpar(col = "black"))
        grid.xaxis(at=seq(0, subqSize, by=.5), gp = gpar(fontsize = fontSize, tcl = NA))
        grid.yaxis(at=seq(0, subqSize, by=.5), gp = gpar(fontsize = fontSize, tcl = NA))
        xseq = rep(seq(0.0, subqSize - gridSize, by = gridSize), each = subqSize/gridSize)
        yseq = rep(seq(0.0, subqSize - gridSize, by = gridSize), subqSize/gridSize)
    loc_key_10 = paste("x = ",sprintf("%1.1f", xseq), "; y = ", sprintf("%1.1f", yseq),"; ", sep="")
        grid.circle(x= sqData$sx, y= sqData$sy, r= log(sqData[,dbhcm])/20, default.units="native", gp=gpar(fill= c(rgb(0,0,1,0.5), rgb(0,1,0, 0.5))[grepl("A", sqData[,status]) + 1], col="black"))
        grid.text(paste(sqData[, tag]), x= sqData[, "sx"]+ log(sqData[, dbhcm])/8 , y=sqData[, "sy"] + log(sqData[, dbhcm])/15, default.units="native", gp = gpar( fontsize= fontSize - 2))
#####################
##  DIAGONAL:
#####################
    if(sqxy[1] == sqxy[2])
    {
        grid.abline(gp = gpar(lwd = 1.5, col = "blue") )
    }    
    if(sqxy[1] != sqxy[2])
    {
        grid.abline(10, -1, gp = gpar(lwd = 1.5, col = "blue") )
    }    
##############
## Grid
##############
    for(i in 1: length(loc_key_10))
    {
        grid.rect(x = xseq[i]+0.1,y = yseq[i]+0.1, width =.2, height=.2, gp=gpar(fill = rgb(0,1,0, .2),lwd =0.1),  default.units="native", name = loc_key_10[i])
    }
    if(svgSave)
        {
            if(!dir.exists(wd2save))
            {
                dir.create(wd2save)
            }    
            grid.export(file.path(wd2save, paste("grid",subqSize,"_", j,".svg",sep="")) , uniqueNames=FALSE)
            
        }    
    }
}
##############################
# audit  plot maps
##############################
##############################
##' Map of trees from permanent plot based on cartesian coordinates 
##' 
##' @param dx between 0 to max.size . Tree mapping X coordinate.
##' @param dy between 0 to max.size . Tree mapping Y coordinate.
##' @param audit auditory data frame from permanent plot tree censo data. 
##' @param quad a character string or factor representing some subunit of the permanent plot.
##' @param save.svg logical true to save a svg file
##' @param wd character string indicating directory towhere the file will be saved. 
##' @return 'mapsvg' returns tree mapped svg mapped with svg pattern ids. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link{gridSVG}} 
##' \url{http://labtrop.ib.usp.br}
#' @examples
##' 
##' \dontrun{
##' dataplot <- data.frame(dx = runif(100, 0,20), dy = runif(100,0,20), quad = rep(paste("quad", 0:1, sep="_"), each=50), dap =rnbinom(100,10,0.5), status="A", tag = 1:100)
##' auditsvg(dataplot, save.svg =FALSE)
##' }
##' 
##' @import grid
##' @import gridSVG
##'
##' @export
##' 
auditsvg <- function(audit, quad = "A00", save.svg = TRUE, wd = getwd(), dx = "new_dx2018", dy = "new_dy2018",  tag = "num_tag", dap = "dap2018", error = "errorType", mapSize = c(13,13))
{
    options(warn = -1)
    ## library("grid")
    ## library("gridSVG")
    dataquad <- audit[audit$quadrat == quad,]
    xyna <- is.na(dataquad[,dx]) | is.na(dataquad[,dy])
    xy <- dataquad[, c(dx, dy)]
    xy[xyna, ] <- dataquad[xyna,c("old_dx", "old_dy")]
    dbh0 <- dataquad[,dap]
    dbh0[is.na(dbh0)] <- 10 
    arv_key <- paste("arv_", dataquad[,tag], sep="")
    tipo <- as.factor(dataquad[, error])
    utipo <- levels(tipo)
    ntipo <- length(utipo)
###########################
## plot here
###########################
    dev.new(width = mapSize[1], height = mapSize[2])
    vptop<- viewport(y=0.9, width=0.8, height=0.2)
    grid.text(x=0.5, y=0.9, paste(" Auditoria Parcela ", quad) ,vp= vptop, gp=gpar(fontsize = 20))
    vp <- viewport(width = 0.8, height = 0.8, xscale=c(0, 20), yscale=c(0, 20))
    pushViewport(vp)
    grid.rect(gp = gpar(col = "black"))
    grid.xaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=gpar(fontsize=15))
    grid.yaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=gpar(fontsize=15))
    cols <- c(rgb(0,0,0, 0.3), rgb(0,1,0, 0.5), rgb(0,0,1, 0.5), rgb(1,1,0, 0.5), rgb(1,0,1, 0.5), rgb(0,1,1, 0.5))
    int <- 1
        for(i in 1:nrow(dataquad))
        {
            grid.circle(x= xy[i, 1],y=xy[i, 2], r= log(dbh0[i])/10, default.units="native", gp=gpar(fill= cols[tipo[i]], col="black"), name = arv_key[i])
            grid.text(paste(dataquad[i, tag]), x= xy[i, 1] + (log(dbh0[i])/8) ,y = xy[i, 2] + (int *log(dbh0[i])/8), default.units="native", gp = gpar(cex = 1.2))
            int = int * -1
        }
    grid.circle(x= c(1.5, 6.5, 11.5, 16.5)[1:ntipo],y =-1.7, r= 0.3, default.units="native", gp=gpar(fill= cols[1:ntipo], col="black"))
    grid.text(utipo[1:ntipo], x = (c(1.5, 6.5, 11.5, 16.5)+ .5)[1:ntipo]   , y= -1.7,  gp = gpar(cex = 1.2), , default.units="native", just= "left")
        ## grid.text(c(xys[2], xys[2]+ 5) ,y=  c(0,1), x= c(-0.08, -0.08),  gp = gpar(cex=2.5, col = "red"))
        #grid.circle(x= a00_0x0$dx,y=a00_0x0$dy, r= log(dbh0)/20, default.units="native", gp=gpar(fill=c(rgb(0,1,0, 0.5),rgb(1,0,0,0.5))[as.factor(a00_0x0$status=="A")], col="gray")) ## this is a much more efficienty way to run the loop above but in order to svg ids write need it is necessary a for expression
        if(save.svg)
        {
            grid.export(file.path(wd, paste("auditmap",quad,".svg",sep="")) , uniqueNames=FALSE)
        }
#######################################
# plot end
#######################################        
}
##############################
# audit  plot maps
##############################
##############################
##' Map of trees from permanent plot based on cartesian coordinates 
##' 
##' @param dx between 0 to max.size . Tree mapping X coordinate.
##' @param dy between 0 to max.size . Tree mapping Y coordinate.
##' @param audit auditory data frame from permanent plot tree censo data. 
##' @param quad a character string or factor representing some subunit of the permanent plot.
##' @param save.svg logical true to save a svg file
##' @param wd character string indicating directory towhere the file will be saved. 
##' @return 'mapsvg' returns tree mapped svg mapped with svg pattern ids. 
##' @author Alexandre Adalardo de Oliveira \email{aleadalardo@gmail.com}
##' @seealso \code{\link{gridSVG}} 
##' \url{http://labtrop.ib.usp.br}
##'
##' \dontrun{
##' dataplot <- data.frame(dx = runif(100, 0,20), dy = runif(100,0,20), quad = rep(paste("quad", 0:1, sep="_"), each=50), dap =rnbinom(100,10,0.5), status="A", tag = 1:100)
##' auditsvg(dataplot, save.svg =FALSE)
##' }
##' 
##' @import grid
##' @import gridSVG
##'
##' @export
ordersvg <- function(audit, quad = "A00", save.svg = TRUE, wd = getwd(), dx = "new_dx2018", dy = "new_dy2018",  tag = "num_tag", dap = "dap2018", error = "errorType", mapSize = c(13,13))
{
    options(warn = -1)
    ## library("grid")
    ## library("gridSVG")
    dataquad <- audit[audit$quadrat == quad,]
    xyna <- is.na(dataquad[,dx]) | is.na(dataquad[,dy])
    xy <- dataquad[, c(dx, dy)]
    xy[xyna, ] <- dataquad[xyna,c("old_dx", "old_dy")]
    dbh0 <- dataquad[, dap]
    dbh0[is.na(dbh0)] <- 10 
    arv_key <- paste("arv_", dataquad[,tag], sep="")
    tipo <- as.factor(dataquad[, error])
    utipo <- levels(tipo)
    ntipo <- length(utipo)
    idmap <- index.map(dx=xy[,dx], dy=xy[,dy])
    xy <- xy[order(idmap),]
    narv <- nrow(xy)
###########################
## plot here
###########################
    dev.new(width = mapSize[1], height = mapSize[2])
    vptop<- viewport(y=0.9, width=0.8, height=0.2)
    grid.text(x=0.5, y=0.9, paste(" Auditoria Parcela ", quad) ,vp= vptop, gp=gpar(fontsize = 20))
    vp <- viewport(width = 0.8, height = 0.8, xscale=c(0, 20), yscale=c(0, 20))
    pushViewport(vp)
    grid.rect(gp = gpar(col = "black"))
    grid.xaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=gpar(fontsize=15))
    grid.yaxis(seq(0,20,by=5) , at=seq(0,20,by=5), gp=gpar(fontsize=15))
    cols <- c(rgb(0,0,0, 0.3), rgb(0,1,0, 0.5), rgb(0,0,1, 0.5), rgb(1,1,0, 0.5), rgb(1,0,1, 0.5), rgb(0,1,1, 0.5))
     int <- 1
        for(i in 1:narv)
        {
            if(i < narv)
            {
                grid.lines(x = c(xy[i,1], xy[i+1, 1]), y = c(xy[i,2], xy[i+1, 2]), default.units="native", gp= gpar(col=rgb(0,0,0,.2), lwd=3, lty=2))
            }
            grid.circle(x= xy[i, 1],y=xy[i, 2], r= log(dbh0[i])/10, default.units="native", gp=gpar(fill= cols[tipo[i]], col="black"), name = arv_key[i])
            grid.text(paste(dataquad[i, tag]), x= xy[i, 1] + (log(dbh0[i])/8) ,y = xy[i, 2] + (int *log(dbh0[i])/8), default.units="native", gp = gpar(cex = 1.2))
            grid.text(as.character(i), x= xy[i, 1] ,y = xy[i, 2], default.units="native", gp = gpar(cex = 1.5, col=rgb(1,1,1)))
            int = int * -1
        }
    grid.circle(x= c(1.5, 6.5, 11.5, 16.5)[1:ntipo],y =-1.7, r= 0.3, default.units="native", gp=gpar(fill= cols[1:ntipo], col="black"))
    grid.text(utipo[1:ntipo], x = (c(1.5, 6.5, 11.5, 16.5)+ .5)[1:ntipo]   , y= -1.7,  gp = gpar(cex = 1.2), , default.units="native", just= "left")
        ## grid.text(c(xys[2], xys[2]+ 5) ,y=  c(0,1), x= c(-0.08, -0.08),  gp = gpar(cex=2.5, col = "red"))
        #grid.circle(x= a00_0x0$dx,y=a00_0x0$dy, r= log(dbh0)/20, default.units="native", gp=gpar(fill=c(rgb(0,1,0, 0.5),rgb(1,0,0,0.5))[as.factor(a00_0x0$status=="A")], col="gray")) ## this is a much more efficienty way to run the loop above but in order to svg ids write need it is necessary a for expression
        if(save.svg)
        {
            grid.export(file.path(wd, paste("ordermap",quad,".svg",sep="")) , uniqueNames=FALSE)
        }
#######################################
# plot end
#######################################        
}
## teste
## audit <- read.table("/home/aao/Ale2016/AleProjetos/PPPeic/censo2018/auditDataAll/auditData15Jan2019/treeaudit.csv", header = TRUE, as.is=TRUE, sep=",")
## audit$new_dx2018[is.na(audit$new_dx201)] <- audit$old_dx[is.na(audit$new_dx2018)]
## audit$new_dy2018[is.na(audit$new_dy201)] <- audit$old_dy[is.na(audit$new_dy2018)]
## #indpos <- index.map(dx = audit$new_dx2018, dy= audit$new_dy2018)
## audit <- audit[!is.na(audit$num_tag),]
## quad = "B11"; save.svg = TRUE; dx = "new_dx2018"; dy = "new_dy2018";  tag = "num_tag"; dap = "dap2018"; error = "errorType"; mapSize = c(13,13)
