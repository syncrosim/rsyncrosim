setClassUnion("missingOrNULLOrChar", c("missing", "NULL","character"))
#' Create or open a library.
#'
#' Creates or opens an \code{\link{SSimLibrary}} object representing a SyncroSim library.
#'
#' @param model A model type or a SyncroSim Project or Scenario. Optional when loading an existing library using a name.
#' @export
setGeneric('ssimLibrary',function(model=NULL,...) standardGeneric('ssimLibrary'))
.ssimLibrary=ssimLibrary

#' The name of a SyncroSim project or scenario.
#'
#' The name of a SyncroSim Project or Scenario.
#'
#' @param x An object with a name.
#' @export
setGeneric('name',function(x) standardGeneric('name'))

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
#Internal version that will not be overwritten by function arguments of the same name.
#Exported to facilitate debugging during development.
#' @export
.filepath=filepath

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
#' # Look for SyncroSim in the usual places
#' mySession = session()
#' path(mySession)
#'
#' # Specify a SyncroSim version
#' mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#'
#' # Get the session from an SSimLibrary
#' myLib = ssimLibrary(name="stsim",model="stsim")
#' session(myLib)
#'
#' # Assign a session to a SyncroSim library
#' session(myLib)=session()
#' @export
setGeneric('session',function(x=NULL,...) standardGeneric('session'))
#Internal version that will not be overwritten by function arguments of the same name.
#Exported to facilitate debugging during development.
#' @export
.session=session

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
setGeneric('session<-',function(x,value) standardGeneric('session<-'))
