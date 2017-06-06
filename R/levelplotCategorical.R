#' levelplot of a categorical RasterLayer with a Raster Attribute Table
#'
#' Plot a RasterLayer with raster attributes set by spatialData() or rasterAttributes().
#' This is a wrapper around the levelplot() function of the rasterVis package.
#'
#' @examples
#'
#' levels(myRaster) #Retrieve the raster attribute table
#' levelplotCategorical(myRaster,attribute="Name") #plot Name attribute
#'
#' @param x A Raster object with a raster attribute table set by spatialData() or rasterAttributes().
#' @param attribute The attribute to be plotted. This must be a column name in the raster attribute table.
#' @export
setGeneric('levelplotCategorical',function(x,attribute,title=NULL) standardGeneric('levelplotCategorical'))
setMethod(
  f='levelplotCategorical',
  signature="RasterLayer",
  definition=function(x,attribute,title){
    #x = myRasters[[1]];attribute="StateLabelXID";title=NULL
    if(is.null(title)){
      title=x@title
    }
    myLevels = raster::levels(x)[[1]]
    myCols = unique(subset(myLevels,select=c(attribute,"hexColor")));myCols=myCols[order(myCols[,1]),]
    print(rasterVis::levelplot(x,att=attribute,at=myCols[,attribute],col.regions=myCols$hexColor,par.settings=myCols,main=title))
})
