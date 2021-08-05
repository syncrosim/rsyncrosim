#' Add module
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{addPackage}} instead.
#'
#' @param filename Character string or vector of these. The path to an .ssimpkg file on disk, or a vector of filepaths.
#' @param session Session.
#' 
addModule <- function(filename, session = NULL) {
  lifecycle::deprecate_warn("1.2.11", "addModule()", "addPackage()")
  addPackage(filename, session)
}

#' Adds a package to SyncroSim
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{addPackage}} instead.
#'
#' @param filename Character string.  The path to a SyncroSim package file.
#' @param session A \code{\link{Session}} object.
#' 
#' @return 
#' This function invisibly returns `TRUE` upon success (i.e.successful 
#' install) and `FALSE` upon failure.
addPackageFile <- function(filename, session = NULL) {
  lifecycle::deprecate_warn("1.2.11", "addPackageFile()", "addPackage()")
  addPackage(filename, session)
}

#' Installed base packages
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{package}} instead.
#'
#' @param ssimObject An object of class \code{\link{Session}} or \code{\link{SsimLibrary}}.
#' 
#' @return 
#' A dataframe of base packages (for Session) or named vector of character strings (for SsimLibrary).
basePackage <- function(ssimObject = NULL) {
  lifecycle::deprecate_warn("1.2.11", "basePackage()", "package()")
  package(ssimObject, installed = "BASE")
}

#' Delete module or modules
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use \code{\link{removePackage}} instead.
#'
#' @param name Character string or vector of these. A module or vector of modules 
#'     to remove. See modules() for options.
#' @param session \code{\link{Session}}.
#' @param force logical. If TRUE, delete without requiring confirmation from user.
#' 
#' @return 
#' Returns "saved" if successful, otherwise an error message.
deleteModule <- function(name, session = NULL, force = FALSE) {
  lifecycle::deprecate_warn("1.2.11", "deleteModule()", "removePackage()")
  removePackage(name, session, force)
}

#' Installed models
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' models are now distributed in Packages; 
#' Please use \code{\link{package}} instead.
#'
#' @param ssimObject \code{\link{Session}} or \code{\link{SsimLibrary}}.
#' 
#' @return 
#' A \code{dataframe} of models (for Session) or named vector of character strings 
#' (for \code{SsimLibrary}).
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
#' @param session \code{\link{Session}}.
#' 
#' @return 
#' Returns a \code{dataframe} of modules.
module <- function(session = NULL) {
  lifecycle::deprecate_warn("1.2.11", "module()", "package()")
  package(session)
}
