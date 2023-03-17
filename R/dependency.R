# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Get, set or remove Scenario dependency(s)
#' 
#' List dependencies, set dependencies, or remove dependencies from a SyncroSim
#' \code{\link{Scenario}}. Setting dependencies is a way of linking together
#' Scenario Datafeeds, such that a change in the Scenario that is the source 
#' dependency will update the dependent Scenario as well. 
#'
#' @details
#'
#' If \code{dependency==NULL}, other arguments are ignored, and set of existing dependencies 
#' is returned in order of precedence (from highest to lowest precedence).
#' Otherwise, returns list of saved or error messages for each dependency of each 
#' scenario.
#'
#' Note that the order of dependencies can be important - dependencies added most 
#' recently take precedence over existing dependencies. So, dependencies included 
#' in the dependency argument take precedence over any other existing dependencies. 
#' If the dependency argument includes more than one element, elements are ordered 
#' from lowest to highest precedence.
#'
#' @param scenario \code{\link{Scenario}} object, character string, integer, or 
#' vector of these. The Scenario object, name, or ID to which a dependency is to 
#' be added (or has already been added if \code{remove=TRUE}). Note that integer ids 
#' are slightly faster
#' @param dependency \code{\link{Scenario}} object, character string, integer, or 
#'     list/vector of these. The Scenario(s) that are the source of the dependency, 
#'     in order from lowest to highest precedence. If \code{NULL} (default) other arguments are 
#'     ignored and the list of existing dependencies is returned
#' @param remove logical. If \code{FALSE} (default) dependencies are added. If \code{TRUE},
#'  dependencies are removed
#' @param force logical. If \code{FALSE} (default) prompt before removing dependencies
#' 
#' @return 
#' If dependency is \code{NULL}, a data frame of existing dependencies, or list of these 
#' if multiple inputs are provided. If dependency is not \code{NULL}, the function 
#' invisibly returns a list bearing the names of the dependencies inputted and 
#' carrying a logical \code{TRUE} upon success (i.e.successful addition or deletion) 
#' and \code{FALSE} upon failure
#' 
#' @examples 
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, Project, and 2 Scenarios
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' myNewScenario <- scenario(myProject,
#'                           scenario = "my New Scenario")
#' 
#' # Set myScenario as a dependency of myNewScenario
#' dependency(myNewScenario, dependency = myScenario)
#' 
#' # Get all dependencies info
#' dependency(myNewScenario)
#' 
#' # Remove dependency
#' dependency(myNewScenario, dependency = myScenario, remove = TRUE)
#' 
#' # Force removal of dependency
#' dependency(myNewScenario, dependency = myScenario, remove = TRUE,
#'            force = TRUE)
#' }
#' 
#' @export
setGeneric("dependency", function(scenario, dependency = NULL, remove = FALSE, force = FALSE) standardGeneric("dependency"))

#' @rdname dependency
setMethod("dependency", signature(scenario = "character"), function(scenario, dependency, remove, force) {
  return(SyncroSimNotFound(scenario))
})

#' @rdname dependency
setMethod("dependency", signature(scenario = "Scenario"), function(scenario, dependency, remove, force) {
  x <- scenario
  cScn <- scenarioId(scenario)
  cScnName <- .name(scenario)
  outName <- paste0(cScnName, " [", cScn, "]")
  # get set of existing dependencies
  args <- list(list = NULL, dependencies = NULL, lib = .filepath(x), sid = cScn, csv = NULL)
  tt <- command(args, .session(x))
  if (!grepl("ID,Name", tt[1], fixed = TRUE)) {
    stop(tt[1])
  }
  dependencySet <- .dataframeFromSSim(tt)
  names(dependencySet)[names(dependencySet) == "iD"] <- "ScenarioID"
  
  # if no dependency, dependency info for each scenario
  if (is.null(dependency)) {
    return(dependencySet)
  }
  
  allScns <- .scenario(.ssimLibrary(x), summary = TRUE)
  
  success <- FALSE
  outResults <- list()
  
  if (is(dependency, "Scenario")) {
    dependency <- scenarioId(dependency)
  }
  
  for (j in seq(length.out = length(dependency))) {
    cDepRaw <- dependency[[j]]
    cDep <- NULL
    
    if (is(cDepRaw, "Scenario")) {
      cDep <- .scenarioId(cDepRaw)
    } 
    
    else if (is(cDepRaw, "character")) {
      if (!is.element(cDepRaw, allScns$Name)) {
        warning(cDepRaw, ": dependency scenario not found in library, wil be ignored.")
      }
      cDep <- allScns$ScenarioID[allScns$Name == cDepRaw]
      if (length(cDep) == 0) {
        stop("Could not find dependency scenario ", cDepRaw)
      }
      if (length(cDep) > 1) {
        stop("Found more than one scenario named ", cDepRaw, ". Please specify a dependency scenario id:", paste0(cDep, collapse = ","))
      }
    } 
    
    else if (is(cDepRaw, "numeric")) {
      if (!is.element(cDepRaw, allScns$ScenarioID)) {
        warning(cDepRaw, ": dependency scenario not found in library, will be ignored.")
      }
      cDep <- cDepRaw
    }
    
    if (is.null(cDep)) {
      stop("dependency must be a Scenario, character string, integer, or vector/list of these.")
    }
    cDepOutName <- paste0(allScns$Name[allScns$ScenarioID == cDep], " [", cDep, "]")
    
    # if Add
    if (!remove) {
      if ((nrow(dependencySet) > 0) && is.element(cDep, dependencySet$ScenarioID)) {
        # to guarantee order of provided dependency, remove then re-add
        args <- list(delete = NULL, dependency = NULL, lib = .filepath(x), sid = cScn, did = cDep, force = NULL)
        tt <- command(args, .session(x))
        
        if (is.na(tt[1])){
          warning("Scenario ", outName, ", Dependency ", cDepOutName, " error: ", tt[1])
        } else if (tt[1] != "saved"){
          warning("Scenario ", outName, ", Dependency ", cDepOutName, " error: ", tt[1])
        }
      }
      
      args <- list(create = NULL, dependency = NULL, lib = .filepath(x), sid = cScn, did = cDep)
      tt <- command(args, .session(x))
  
      if (is.na(tt[1])){
        warning("Scenario ", outName, ", Dependency ", cDepOutName, " error: ", tt[1])
        outResults[[cDepOutName]] <- FALSE
      } else if (tt[1] == "saved"){
        message(paste0("Dependency: <", cDepOutName, ">, added to scenario: <", outName, ">"))
        outResults[[cDepOutName]] <- TRUE
      } else {
        warning("Scenario ", outName, ", Dependency ", cDepOutName, " error: ", tt[1])
        outResults[[cDepOutName]] <- FALSE
      }
      
      # if Remove
    } else { 
      if ((nrow(dependencySet) == 0) || !is.element(cDep, dependencySet$ScenarioID)) {
        warning(paste0(cDepOutName, " is not a dependency of ", outName))
        outResults[[cDepOutName]] <- FALSE
      } else {
        if (force) {
          answer <- "y"
        } else {
          answer <- readline(prompt = paste0("Do you really want to remove dependency ", cDepOutName, " from ", outName, "? (y/n): "))
        }
        if (answer == "y") {
          args <- list(delete = NULL, dependency = NULL, lib = .filepath(x), sid = cScn, did = cDep, force = NULL)
          tt <- command(args, .session(x))
          if (tt[1] == "saved") {
            message(paste0("Dependency: <", cDepOutName, ">, deleted from scenario: <", outName, ">"))
            outResults[[cDepOutName]] <- TRUE
          } else{
            warning("Scenario ", outName, ", Dependency ", cDepOutName, " error: ", tt[1])
            outResults[[cDepOutName]] <- FALSE
          }
        } else {
          message("Deletion of dependency ", cDepOutName," skipped")
          outResults[[cDepOutName]] <- FALSE
        }
      }
    }
    
  }
  return(invisible(outResults))
})
