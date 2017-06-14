setClassUnion("missingOrNULL", c("missing", "NULL"))
setClassUnion("missingOrNULLOrChar", c("missing", "NULL","character"))

# Information about an library
#
# Get basic information about a Library. 
# Use project(summary==T) and scenario(summary=T) to get similar information about Project/Scenario
#
# @param x An object containing info.
# @export
setGeneric('info',function(x) standardGeneric('info'))

# datasheets
#
# Gets datasheet summary info from an SsimLibrary, Project or Scenario.
#
# @details
# See \code{\link{datasheet}} for discussion of optional/empty/sheetName/lookupsAsFactors arguments.
# \itemize{
#   \item {If x/project/scenario identify a scenario: }{Returns library, project, and scenario scope datasheets.}
#   \item {If x/project/scenario identify a project (but not a scenario): }{Returns library and project scope datasheets.}
#   \item {If x/project/scenario identify a library (but not a project or scenario): }{Returns library scope datasheets.}
# }
#
# @param x An SsimLibrary, Project or Scenario object. Or a path to a SyncroSim library on disk.
# @param project Project name or id. Ignored if x is a Project.
# @param scenario Scenario name or id. Ignored if x is a Scenario.
# @param scope "scenario","project", "library", "all", or NULL.
# @param refresh If FALSE (default) names are retrieved from x@datasheetNames. If TRUE names are retrieved using a console call (slower).
# @return A dataframe of datasheet names.
# @examples
#
# @export
#Note: this function is now internal. Should now only be called from datasheet.
setGeneric('datasheets',function(x,project=NULL,scenario=NULL,scope=NULL,refresh=F) standardGeneric('datasheets'))

#Handles case where x is a path to an SyncroSim library on disk.
setMethod('datasheets', signature(x="character"), function(x,project,scenario,scope,refresh) {
  x = .ssimLibrary(x,create=F)
  out = .datasheets(x,project,scenario,scope,refresh)
  return(out)
})


#' Set spatial data
#'
#' Loads spatial data into the SyncroSim library.
#'
#' @details
#'
#' Metadata should be a dataframe that can be appended to datasheet(x,metadata$SheetName[1]), containing 1 row for each layer of data.
#' "SheetName" and at least one "FileName" columns are expected in metadata.
#' A "RasterLayerName" column in metadata is optional. If present, it should contain the name of each layer in the data raster stack.
#' If "RasterLayerName" is not included in metadata, names(data) should correspond to the FileNames in metadata.
#' 
#' Note: Spatial data will be appended if non-FileName columns (e.g. Iteration, Timestep, etc) differ between metadata and datasheet(x,metadata$SheetName[1]). Otherwise, the new spatial data will overwrite the old spatial data.
#'
#' @param x An SsimLibrary, Project or Scenario object. Or the path to a library on disk.
#' @param data A RasterLayer or RasterStack to load.
#' @param metadata A dataframe that can be appended to datasheet(x,metadata$SheetName[1]) - see details. 
#' @param project Project name or id.
#' @param scenario Scenario name or id.
#' @param breakpoint Set to TRUE when setting spatial data in a breakpoint function.
#' @param check Default is TRUE. Set FALSE to speed calculations my assuming metadata is valid.
#' @return A named list of success or failure reports.
#' @examples
#'
#' @export
setGeneric('loadSpatialData',function(x,data,metadata,project=NULL,scenario=NULL,breakpoint=F,check=T) standardGeneric('loadSpatialData'))
#Handles case where x is a path to an SyncroSim library on disk.
setMethod('loadSpatialData', signature(x="character"), function(x,data,metadata,project,scenario,breakpoint,check) {
  x = .ssimLibrary(x,create=F)
  out = loadSpatialData(x,data,metadata,project,scenario,breakpoint,check)
  return(out)
})

# Get or set a socket connection.
#
# @param x An ipAddress or BreakpointSession object. If NULL a default ip will be used.
# @param port For new connections only - a port number.
# @export
setGeneric('connection',function(x,...) standardGeneric('connection'))
# @describeIn connection Get a new connection.
setMethod('connection',signature(x="missingOrNULLOrChar"),
          function(x='127.0.0.1',port=13000) {
            #port=13000;ipAddress='127.0.0.1'
            ipAddress = x
            con = socketConnection(host = ipAddress, port=port,open="r+",encoding="UTF-8",blocking=T,server=F,timeout=4)
            ## S3 method for class 'connection'
            #open(con, open = "r", blocking = TRUE, ...)
            ## S3 method for class 'connection'
            #close(con, type = "rw", ...)
            #flush(con)
            #isOpen(con, rw = "")
            #isIncomplete(con)
            if(!isOpen(con)){
              stop(paste0('Problem connecting to the SyncroSim server. IP:',ipAddress," Port:",port))
            }
            return(con)
          })

