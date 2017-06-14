# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Set attributes and colors of a RasterLayer object.
#'
#' Set attributes and colors of a Raster object.
#' This is a wrapper around ratify() function from the raster package.
#'
#' @details
#'
#' The (optional) Color column of a rat table should have one of these formats:
#' \itemize{
#'   \item {alpha,R,G,B: } {4 numbers representing red, green, blue and alpha, separated by commas, and scaled between 0 and 255. See rgb() for details.}
#'   \item {R colour names: } {See colors() for options.}
#'   \item {hexadecimal colors: } {As returned by R functions such as rainbow(), heat.colors(), terrain.colors(), topo.colors(), gray(), etc.}
#' }
#'
#' @examples
#'
#' levels(myRaster) #retrieve raster attribute table
#'
#' @param raster A Raster object.
#' @param rat dataframe. A raster attribute table is a dataframe with ID, (optional) Color, and other columns. See raster::ratify() for details.
#' @export
setGeneric('rasterAttributes<-',function(raster,value) standardGeneric('rasterAttributes<-'))
setReplaceMethod(
  f='rasterAttributes',
  signature="RasterLayer",
  definition=function(raster,value){
  rat = value
  rat=subset(rat,select=c("ID",setdiff(names(rat),c("ID"))))
  rat=rat[order(rat$ID),]

  if(is.element("Color",names(rat))){
    rat$Color=as.character(rat$Color)
    rat$hexColor=rat$Color
    if(length(strsplit(rat$Color[1],split=",")[[1]])==4){
      for(j in seq(length.out=nrow(rat))){
        cCol = as.numeric(strsplit(rat$Color[j],split=",")[[1]])
        rat$hexColor[j] = rgb(red=cCol[2],green=cCol[3],blue=cCol[4],alpha=cCol[1],maxColorValue=255)
      }
    }else{
      if(!grepl("#",rat$Color[1],fixed=T)){
        rgbTab= col2rgb(rat$Color)
        rat$hexColor=rgb(rgbTab["red",],rgbTab["green",],rgbTab["blue",],255,maxColorValue=255)
      }
    }
    #colortable(x)=rat$hexColor
    #rat$rgb=NULL
  }

  raster = raster::ratify(raster)
  levels(raster)=rat

  return(raster)
})
