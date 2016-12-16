#' Set attributes and colors of a RasterLayer object.
#'
#' Set attributes and colors of a Raster object.
#' This is a wrapper around ratify() and colortable() functions from the raster package.
#'
#' @details
#'
#' The (optional) Color column of a rat table should have one of these formats:
#' \itemize{
#'   \item {R,G,B,alpha: } {4 numbers representing red, green, blue and alpha, separated by commas, and scaled between 0 and 255. See rgb() for details.}
#'   \item {R colour names: } {See colors() for options.}
#'   \item {hexadecimal colors: } {As returned by R functions such as rainbow(), heat.colors(), terrain.colors(), topo.colors(), gray(), etc.}
#' }
#'
#' @examples
#'
#' levels(myRaster) #retrieve raster attribute table
#' colortable(myRaster) #retrieve colortable
#'
#' @param x A Raster object.
#' @param rat A raster attribute table. This is a dataframe with ID, (optional) Color, and other columns. See raster::ratify() for details.
#' @export
setGeneric('rasterAttributes<-',function(x,value) standardGeneric('rasterAttributes<-'))
setReplaceMethod(
  f='rasterAttributes',
  signature="RasterLayer",
  definition=function(x,value){
  rat = value
  rat=subset(rat,select=c("ID",setdiff(names(rat),c("ID"))))
  rat=rat[order(rat$ID),]

  if(is.element("Color",names(rat))){
    rat$Color=as.character(rat$Color)
    rat$rgb=rat$Color
    if(length(strsplit(rat$Color[1],split=",")[[1]])==4){
      for(j in seq(length.out=nrow(rat))){
        cCol = as.numeric(strsplit(rat$Color[j],split=",")[[1]])
        rat$rgb[j] = rgb(red=cCol[1],green=cCol[2],blue=cCol[3],alpha=cCol[4],maxColorValue=255)
      }
    }else{
      if(!grepl("#",rat$Color[1],fixed=T)){
        rgbTab= col2rgb(rat$Color)
        rat$rgb=rgb(rgbTab["red",],rgbTab["green",],rgbTab["blue",],255,maxColorValue=255)
      }
    }
    colortable(x)=rat$rgb
    rat$rgb=NULL
  }

  x = raster::ratify(x)
  levels(x)=rat

  return(x)
})
