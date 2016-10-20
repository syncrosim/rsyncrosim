setClassUnion("missingOrNULLOrChar", c("missing", "NULL","character"))
#' The path to an object on disk
#'
#' The path to a SyncroSim Session, SSimLibarary, Project or Scenario on disk.
#'
#' @param x An object containing a path.
setGeneric('path',function(x) standardGeneric('path'))

#' Information about an object
#'
#' Get basic information about a SyncroSim Session, SSimLibarary, Project or Scenario
#'
#' @param x An object containing info.
setGeneric('info',function(x) standardGeneric('info'))
#' Start or get a SyncroSim session.
#'
#' Methods to create a Syncrosim session or fetch one from a SSimLibrary, Project or Scenario object.
#' @param x=NULL A path to SyncroSim.Console.exe or an object containing a Session.
#'  If NULL the usual locations are searched.
#' @return An SyncroSim Session object containing a valid console path.
#' @examples
#' #Look for SyncroSim in the usual places
#' mySession = session()
#' path(mySession)
#'
#' #Specify a SyncroSim version
#' mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#'
#' #Get the session from an SSimLibrary
#' myLib = ssimLibrary(name="st-sim",model="st-sim")
#' session(myLib)
#' @export
setGeneric('session',function(x=NULL) standardGeneric('session'))
