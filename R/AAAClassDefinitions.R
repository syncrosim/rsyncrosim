#NOTE: Constructors for each class are defined in the R file bearing the name of the class (lower case). e.g. session.R, ssimLibrary.R, etc.

#' SyncroSim Session class
#'
#' A SyncroSim Session object contains a link to SyncroSim.
#' \code{SsimLibrary}, \code{Project} and \code{Scenario} objects contain a \code{Session} used to query and modify the object.
#'
#' @examples
#' #TODO - update examples
#' # Create or load a library using a non-default Session
#' mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#' myLib = ssimLibrary(name="stsim",session=mySession)
#' session(myLib)
#'
#' showMethods(class="Session",where=loadNamespace("rsyncrosim")) #Methods for the Session
#' filepath(mySession)   # Lists the folder location of syncrosim session
#' version(mySession)   # Lists the version of syncrosim session
#' modules(mySession)   # Dataframe of the modules installed with this version of syncrosim.
#' models(mySession) # Dataframe of the models installed with this version of syncrosim.
#'
#' # Add and remove modules
#' deleteModule("stsim-stock-flow",mySession) 
#' is.element("stsim-stock-flow",modules(mySsim)$shortName)
#' addModule("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg",mySession)
#' addModule(c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg"),mySession)
#' is.element("stsim-stock-flow",modules(mySsim)$shortName)
#'
#' # Create or load a library using a default Session
#' myLib = ssimLibrary(name="stsim")
#' session(myLib)
#' @slot filepath The path to SyncroSim
#' @slot silent If FALSE, all SyncroSim output with non-zero exit status is printed. Helpful for debugging. Default=TRUE.
#' @slot printCmd If TRUE, arguments passed to the SyncroSim console are also printed. Helpful for debugging. FALSE by default.
#' @slot defaultModel The name of a SyncroSim model type. "stsim" by default.
#' @name Session-class
#' @rdname Session-class
#' @export Session
Session <- setClass("Session", representation(filepath="character",silent="logical",printCmd="logical",defaultModel="character"))

#' SyncroSim Library class
#'
#' \code{SsimLibrary} object representing a SyncroSim Library.
#'
#' @seealso See \code{\link{ssimLibrary}} for options when creating or loading an SyncroSim library.
#' @examples
#' #TODO – update examples
#' # Create or load and query a SyncroSim Library.
#' myLibrary = ssimLibrary()
#' session(myLibrary)
#' filepath(myLibrary)
#' info(myLibrary)
#'
#' # Add or load a project, then get the SyncroSim Library associated with that Project
#' myProject = project(myLibrary,project="a project")
#' 
#' myLibrary = ssimLibrary(myProject)
#'
#' @slot session The SyncroSim Session.
#' @slot filepath The path to the library on disk.
#' @slot datasheetNames The names and scope of all datasheets in the library. Used to speed calculations.
#' @name SsimLibrary-class
#' @rdname SsimLibrary-class
#' @export SsimLibrary
SsimLibrary <- setClass("SsimLibrary", representation(session="Session",filepath="character",datasheetNames="data.frame"))

#' SyncroSim Scenario class
#'
#' \code{Scenario} object representing a SyncroSim Project.
#'
#' @seealso See \code{\link{scenario}} for options when creating or loading an SyncroSim Scenario.
#' @slot session The session associated with the library.
#' @slot filepath The path to the library on disk.
#' @slot datasheetNames Names and scope of all datasheets in library.
#' @slot projectId The project id.
#' @slot scenarioId The scenario id.
#' @slot parentId For a result scenario, this is the id of the parent scenario. 0 indicates this is not a result scenario.
#' @slot breakpoints An (optional) list of Breakpoint objects. See ?breakpoints for details.
#' @name Scenario-class
#' @rdname Scenario-class
#' @export Scenario
Scenario <- setClass("Scenario", contains="SsimLibrary",representation(projectId="numeric",scenarioId="numeric",parentId="numeric",breakpoints="list"))

#' SyncroSim Project class
#'
#' \code{Project} object representing a SyncroSim Project.
#'
#' @seealso See \code{\link{project}} for options when creating or loading an SyncroSim Project.
#' @slot session The session associated with the library.
#' @slot filepath The path to the library on disk.
#' @slot datasheetNames Names and scopes of datasheets in the library.
#' @slot projectId The project id
#' @name Project-class
#' @rdname Project-class
#' @export Project
Project <- setClass("Project", contains="SsimLibrary",representation(projectId="numeric"))
