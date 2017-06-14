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

