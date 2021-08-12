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
envInputFolder <- function(scenario, datasheetName) {
  envValidateEnvironment()
  return(envCreateScenarioFolder(scenario, ssimEnvironment()$InputDirectory, datasheetName))
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
envOutputFolder <- function(scenario, datasheetName) {
  envValidateEnvironment()
  return(envCreateScenarioFolder(scenario, ssimEnvironment()$OutputDirectory, datasheetName))
}

#' SyncroSim Temporary Folder
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{runtimeTempFolder}} instead.
#' 
#' @param folderName character. The folder name
#'
#' @keywords internal
envTempFolder <- function(folderName) {
  envValidateEnvironment()
  return(envCreateTempFolder(folderName))
}

#' Reports SyncroSim simulation progress
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{progressBar}} instead.
#'
#' @param iteration integer. The current iteration
#' @param timestep integer. The current timestep
#' 
#' @return
#' No returned value, used for side effects.
#' 
#' @keywords internal
envReportProgress <- function(iteration, timestep) {
  envValidateEnvironment()
  cat(sprintf("ssim-task-status=Simulating -> Iteration is %d - Timestep is %d\r\n", iteration, timestep))
  flush.console()
}

#' Begins a SyncroSim simulation
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{progressBar}} instead.
#'
#' @param totalSteps integer.  The total number of steps in the simulation
#' 
#' @keywords internal
envBeginSimulation <- function(totalSteps) {
  envValidateEnvironment()
  cat(sprintf("ssim-task-start=%d\r\n", totalSteps))
  flush.console()
}

#' Steps a SyncroSim simulation
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{progressBar}} instead.
#' 
#' @keywords internal
envStepSimulation <- function() {
  envValidateEnvironment()
  cat("ssim-task-step=1\r\n")
  flush.console()
}

#' Ends a SyncroSim simulation
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{progressBar}} instead.
#' 
#' @keywords internal
envEndSimulation <- function() {
  envValidateEnvironment()
  cat("ssim-task-end=True\r\n")
  flush.console()
}