#' Add module
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{addPackage}} instead.
#'
#' @param filename character string or vector of these. The path to an .ssimpkg 
#' file on disk, or a vector of filepaths.
#' @param session \code{\link{Session}} object
#' 
#' @keywords internal
#' 
#' @export
addModule <- function(filename, session = NULL) {
  lifecycle::deprecate_warn("1.2.11", "addModule()", "addPackage()")
  addPackage(filename, session)
}

#' Adds a package to SyncroSim
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{addPackage}} instead.
#'
#' @param filename character string.  The path to a SyncroSim package file
#' @param session \code{\link{Session}} object
#' 
#' @keywords internal
#' 
#' @export
addPackageFile <- function(filename, session = NULL) {
  lifecycle::deprecate_warn("1.2.11", "addPackageFile()", "addPackage()")
  addPackage(filename, session)
}

#' Installed base packages
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{package}} instead.
#'
#' @param ssimObject \code{\link{Session}} or \code{\link{SsimLibrary}} object
#' 
#' @keywords internal
#' 
#' @export
basePackage <- function(ssimObject = NULL) {
  lifecycle::deprecate_warn("1.2.11", "basePackage()", "package()")
  package(ssimObject, installed = "BASE")
}

#' Delete module or modules
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{removePackage}} instead.
#'
#' @param name character string or vector of these. A module or vector of modules 
#'     to remove. See modules() for options
#' @param session \code{\link{Session}} object
#' @param force logical. If \code{FALSE} (default), require confirmation from user
#' before deletion
#' 
#' @keywords internal
#' 
#' @export
deleteModule <- function(name, session = NULL, force = FALSE) {
  lifecycle::deprecate_warn("1.2.11", "deleteModule()", "removePackage()")
  removePackage(name, session, force)
}

#' Deletes a package from your SyncroSim installation
#' 
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{removePackage}} instead.
#' 
#' @param name character string or vector of these. A package or vector of 
#' packages to remove
#' @param session \code{\link{Session}} object
#' @param force logical. If \code{FALSE} (default), require confirmation from user
#' before deletion
#' 
#' @keywords internal
#' 
#' @export
deletePackage <- function(name, session = NULL, force = FALSE) {
  lifecycle::deprecate_warn("1.2.11", "deletePackage()", "removePackage()")
  removePackage(name, session, force)
}

#' Installed models
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' 
#' Models are now distributed in Packages; 
#' please use \code{\link{package}} instead.
#'
#' @param ssimObject \code{\link{Session}} or \code{\link{SsimLibrary}} object
#' 
#' @keywords internal
#' 
#' @export
model <- function(ssimObject = NULL) {
  lifecycle::deprecate_warn("1.2.11", "model()", "package()")
  package(ssimObject)
}

#' Installed modules
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' modules are now distributed in Packages; 
#' Please use \code{\link{package}} instead.
#'
#' @param session \code{\link{Session}} object
#' 
#' @keywords internal
#' 
#' @export
module <- function(session = NULL) {
  lifecycle::deprecate_warn("1.2.11", "module()", "package()")
  package(session)
}

#' SyncroSim DataSheet Input Folder
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{runtimeInputFolder}} instead.
#'
#' @param scenario \code{\link{Scenario}} object. A SyncroSim result Scenario
#' @param datasheetName character. The input Datasheet name
#' 
#' @keywords internal
#' 
#' @export
envInputFolder <- function(scenario, datasheetName) {
  lifecycle::deprecate_warn("1.2.11", "envInputFolder()", "runtimeInputFolder()")
  runtimeInputFolder(scenario, datasheetName)
}

#' SyncroSim DataSheet Output Folder
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{runtimeOutputFolder}} instead.
#'
#' @param scenario \code{\link{Scenario}} object. A SyncroSim result Scenario
#' @param datasheetName character. The output Datasheet name
#' 
#' @keywords internal
#' 
#' @export
envOutputFolder <- function(scenario, datasheetName) {
  lifecycle::deprecate_warn("1.2.11", "envOutputFolder()", "runtimeOutputFolder()")
  runtimeOutputFolder(scenario, datasheetName)
}

#' SyncroSim Temporary Folder
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{runtimeTempFolder}} instead.
#' 
#' @param folderName character. The folder name
#'
#' @keywords internal
#' 
#' @export
envTempFolder <- function(folderName) {
  lifecycle::deprecate_warn("1.2.11", "envTempFolder()", "runtimeTempFolder()")
  runtimeTempFolder(folderName)
}

#' Reports SyncroSim simulation progress
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{progressBar}} instead.
#'
#' @param iteration integer. The current iteration
#' @param timestep integer. The current timestep
#'
#' @keywords internal
#' 
#' @export
envReportProgress <- function(iteration, timestep) {
  lifecycle::deprecate_warn("1.2.11", "envReportProgress()", "progressBar()")
  progressBar(type = "report", iteration = iteration, timestep = timestep)
}

#' Begins a SyncroSim simulation
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{progressBar}} instead.
#'
#' @param totalSteps integer.  The total number of steps in the simulation
#' 
#' @keywords internal
#' 
#' @export
envBeginSimulation <- function(totalSteps) {
  lifecycle::deprecate_warn("1.2.11", "envBeginSimulation()", "progressBar()")
  progressBar(type = "report", totalSteps = totalSteps)
}

#' Steps a SyncroSim simulation
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{progressBar}} instead.
#' 
#' @keywords internal
#' 
#' @rdname envStepSimulation
#' @export
envStepSimulation <- function() {
  lifecycle::deprecate_warn("1.2.11", "envStepSimulation()", "progressBar()")
  progressBar(type = "step")
}

#' Ends a SyncroSim simulation
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{progressBar}} instead.
#' 
#' @keywords internal
#' 
#' @export
envEndSimulation <- function() {
  lifecycle::deprecate_warn("1.2.11", "envEndSimulation()", "progressBar()")
  progressBar(type = "end")
}