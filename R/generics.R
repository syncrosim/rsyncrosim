setClassUnion("missingOrNULLOrChar", c("missing", "NULL","character"))
#' The name of the model associate with a SyncroSim object
#'
#' The name of the model associated with a SSimLibarary, Project or Scenario.
#'
#' @param x An object with an associated model name.
#' @export
setGeneric('modelName',function(x) standardGeneric('modelName'))

#' The path to a SyncroSim object on disk
#'
#' The path to a SyncroSim Session, SSimLibarary, Project or Scenario on disk.
#'
#' @param x An object containing a filepath.
#' @export
setGeneric('filepath',function(x) standardGeneric('filepath'))

#' Information about an object
#'
#' Get basic information about a SyncroSim Session, SSimLibarary, Project or Scenario
#'
#' @param x An object containing info.
#' @export
setGeneric('info',function(x) standardGeneric('info'))

#' Start or get a SyncroSim session.
#'
#' Methods to create a Syncrosim session or fetch one from a SSimLibrary, Project or Scenario object.
#' @param x A path to SyncroSim.Console.exe or an object containing a Session.
#'  If NULL the usual locations are searched.
#' @param silent Applies only if x is a path or NULL. If TRUE, warnings from the console are ignored. Otherwise they are printed.
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
#'
#' #assign a session to a SyncroSim library
#' session(myLib)=session()
#' @export
setGeneric('session',function(x=NULL,...) standardGeneric('session'))

#' Set a SyncroSim session.
#'
#' Set the Session of a SSimLibrary, Project or Scenario object.
#'
#' @param x= A SyncroSim Session.
#' @return An SyncroSim object containing a Session.
#' @examples
#' myLib = ssimLibrary()
#' session(ssimLibrary)<-session()
#' session(ssimLibrary)
#' @export
setGeneric('session<-',function(object,value) standardGeneric('session<-'))
