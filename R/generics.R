setClassUnion("missingOrNULLOrChar", c("missing", "NULL","character"))
#' The scenarios in a SyncroSim library or project.
#'
#' Get a list of scenarios in a SSimLibrary or Project.
#'
#' @param x An SSimLibrary or Project object, or an SSimLibrary name.
#' @param project A project name, id, or object.
#' @param names If FALSE, a list of \code{\link{Scenario}} objects is returned. If TRUE returns a dataframe containing the name,id and project id of each scenario.
#' @return By default returns a list of scenarios identified by id. Each element of the list contains a SyncroSim Scenario object. If names=T, returns a dataframe containing the name, id, and project id of each scenario.
#' @examples
#' myScenarios = scenarios(ssimLibrary(model="stsim",name="stsim"))
#' @export
setGeneric('scenarios',function(x,...) standardGeneric('scenarios'))
setMethod('scenarios', signature(x="character"), function(x,...) {
  x = .ssimLibrary(name=x)
  out = scenarios(x,...)
  return(out)
})
#' Create or open a library.
#'
#' Creates or opens an \code{\link{SSimLibrary}} object representing a SyncroSim library.
#'
#' @param model A model type or a SyncroSim Project or Scenario. Optional when loading an existing library using a name.
#' @export
setGeneric('ssimLibrary',function(model=NULL,...) standardGeneric('ssimLibrary'))

#' The name of a SyncroSim project or scenario.
#'
#' The name of a SyncroSim Project or Scenario.
#'
#' @param x An object with a name.
#' @export
setGeneric('name',function(x) standardGeneric('name'))

#' The id of a SyncroSim project or scenario.
#'
#' The id of a SyncroSim Project or Scenario.
#'
#' @param x An object with an id.
#' @export
setGeneric('id',function(x) standardGeneric('id'))

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

