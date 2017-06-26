# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' levelplot of a categorical RasterLayer with a Raster Attribute Table
#'
#' Plot a RasterLayer with raster attributes set by spatialData() or ssimRatify().
#' This is a wrapper around the levelplot() function of the rasterVis package.
#'
#' @examples
#'
#' levels(myRaster) #Retrieve the raster attribute table
#' ssimLevelplot(myRaster,attribute="Name") #plot Name attribute
#'
#' @param raster A RasterLayer with a raster attribute table set by spatialData() or ssimRatify().
#' @param attribute character string. The attribute to be plotted. This must be a column name in the raster attribute table.
#' @export
setGeneric('ssimLevelplot',function(raster,attribute,...) standardGeneric('ssimLevelplot'))
setMethod(
  f='ssimLevelplot',
  signature="RasterLayer",
  definition=function(raster,attribute,...){
    #x = myRasters[[1]];attribute="StateLabelXID";title=NULL
    myLevels = raster::levels(raster)[[1]]
    myCols = unique(subset(myLevels,select=c(attribute,"hexColor")));myCols=myCols[order(myCols[,1]),]
    print(rasterVis::levelplot(raster,att=attribute,at=myCols[,attribute],col.regions=myCols$hexColor,par.settings=myCols,...))
})
