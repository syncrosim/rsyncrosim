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

#' Set the project or scenario names.
#'
#' Set the name of a SyncroSim Project or Scenario.
#'
#' @param x A SyncroSim \code{\link{Project}} or \code{\link{Scenario}} object.
#' @param value The new name.
#' @export
setGeneric('name<-',function(x,value) standardGeneric('name<-'))


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

#' datasheets
#'
#' Gets datasheets from an SSimLibrary, Project or Scenario.
#'
#' @details
#' \itemize{
#'   \item {If x/project/scenario identify a scenario: }{Returns library, project, and scenario scope datasheets.}
#'   \item {If x/project/scenario identify a project (but not a scenario): }{Returns library and project scope datasheets.}
#'   \item {If x/project/scenario identify a library (but not a project or scenario): }{Returns library scope datasheets.}
#' }
#'
#' @param x An SSimLibrary, Project or Scenario object. Or a path to a SyncroSim library on disk.
#' @param project Project name or id. Ignored if x is a Project.
#' @param scenario Scenario name or id. Ignored if x is a Scenario.
#' @param names If TRUE (default) returns dataframe of sheet names. If false returns a named list of dataframes representing each datasheet.
#' @param empty If TRUE returns empty template dataframes.
#' @param scope "scenario","project", "library", or NULL.
#' @return A dataframe of datasheet names, or list of datasheets represented by dataframes.
#' @examples
#'
#' @export
setGeneric('datasheets',function(x,...) standardGeneric('datasheets'))
.datasheets=datasheets
#Handles case where x is a path to an SyncroSim library on disk.
setMethod('datasheets', signature(x="character"), function(x,project=NULL,scenario=NULL,...) {
  x = .getFromXProjScn(x,project,scenario)
  out = .datasheets(x,project,scenario,...)
  return(out)
})

#' Get a datasheet
#'
#' Gets Syncrosim datasheet.
#'
#' @details
#' If !empty & !optional: return required columns and columns containing data
#'
#' @param x An SSimLibrary, Project or Scenario object. Or the path to a library on disk.
#' @param name The sheet name
#' @param project Project name or id.
#' @param scenario Scenario name or id.
#' @param optional If FALSE (default) returns only required columns. If TRUE returns optional columns also.
#' @param empty If FALSE (default) returns data (if any). If FALSE returns empty dataframe of correct format.
#' @param sheetNames Output from datasheets(). Used internally to speed calculation of dependencies.
#' @param checkDependencies If TRUE (default) dependencies are checked. Set to FALSE to speed calculations.
#' @return A dataframe.
#' @examples
#'
#' @export
setGeneric('datasheet',function(x,name,project=NULL,scenario=NULL,optional=F,empty=F,sheetNames=NULL,checkDependencies=T) standardGeneric('datasheet'))
#Handles case where x is a path to an SyncroSim library on disk.
setMethod('datasheet', signature(x="character"), function(x,name,project,scenario,optional,empty,sheetNames,checkDependencies) {
  x = .getFromXProjScn(x,project,scenario)
  out = .datasheet(x,name,project,scenario,optional,empty,sheetNames,checkDependencies)
  return(out)
})

#' Set datasheets
#'
#' Loads datasheets into the SyncroSim library.
#'
#' @param x An SSimLibrary, Project or Scenario object. Or the path to a library on disk.
#' @param data A dataframe or named list of dataframes to load.
#' @param name The sheet name - required if data is a dataframe, ignored otherwise.
#' @param project Project name or id.
#' @param scenario Scenario name or id.
#' @return A named list of success or failure reports.
#' @examples
#'
#' @export
setGeneric('loadDatasheets',function(x,...) standardGeneric('loadDatasheets'))
#Handles case where x is a path to an SyncroSim library on disk.
setMethod('loadDatasheets', signature(x="character"), function(x,data,name=NULL,project=NULL,scenario=NULL,...) {
  x = .getFromXProjScn(x,project,scenario)
  out = .datasheet(x,data=data,name=name,project=project,scenario=scenario,...)
  return(out)
})

