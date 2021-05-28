# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Set or remove Scenario dependency(s), or get existing dependencies.
#'
#' @details
#'
#' If dependency==NULL, other arguments are ignored, and set of existing dependencies 
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
#' @param scenario Scenario. The scenario to which a dependency is to be added 
#'     (or has already been added if remove=TRUE).
#' @param dependency \code{\link{Scenario}}, character string, integer, or 
#'     list/vector of these. The scenario(s) that are the source of the dependency, 
#'     in order from lowest to highest precedence. If NULL other arguments are 
#'     ignored and the list of existing dependencies is returned.
#' @param scenario character string, integer, or vector of these. Name or ID of 
#'     scenario(s) to which a dependency is to be added (or has been already 
#'     added if remove=TRUE). If NULL then ssimObject must be a \code{\link{Scenario}}. 
#'     Note that integer ids are slightly faster.
#' @param remove logical. If F (default) dependencies are added. If T, dependencies 
#'     are removed.
#' @param force logical. If F (default) prompt before removing dependencies.
#' 
#' @return 
#' If dependency is NULL, a dataframe of existing dependencies, or list of these 
#' if multiple inputs are provided. If dependency is not NULL, the function 
#' invisibly returns a list bearing the names of the dependencies inputted and 
#' carrying a logical `TRUE` upon success (i.e.successful addition or deletion) 
#' and `FALSE` upon failure.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary)
#' myScenario <- scenario(myProject)
#' 
#' # Get all dependencies info
#' myDatasheets <- dependency(myScenario)
#' 
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
  names(dependencySet)[names(dependencySet) == "iD"] <- "scenarioId"
  
  # if no dependency, dependency info for each scenario
  if (is.null(dependency)) {
    return(dependencySet)
  }
  
  allScns <- .scenario(.ssimLibrary(x), summary = TRUE)
  
  success <- FALSE
  outResults <- list()
  
  if (class(dependency) == "Scenario") {
    dependency <- scenarioId(dependency)
  }
  
  for (j in seq(length.out = length(dependency))) {
    cDepRaw <- dependency[[j]]
    cDep <- NULL
    
    if (class(cDepRaw) == "Scenario") {
      cDep <- .scenarioId(cDepRaw)
    } 
    
    else if (class(cDepRaw) == "character") {
      if (!is.element(cDepRaw, allScns$name)) {
        warning(cDepRaw, ": dependency scenario not found in library, wil be ignored.")
      }
      cDep <- allScns$scenarioId[allScns$name == cDepRaw]
      if (length(cDep) == 0) {
        stop("Could not find dependency scenario ", cDepRaw)
      }
      if (length(cDep) > 1) {
        stop("Found more than one scenario named ", cDepRaw, ". Please specify a dependency scenario id:", paste0(cDep, collapse = ","))
      }
    } 
    
    else if (class(cDepRaw) == "numeric") {
      if (!is.element(cDepRaw, allScns$scenarioId)) {
        warning(cDepRaw, ": dependency scenario not found in library, will be ignored.")
      }
      cDep <- cDepRaw
    }
    
    if (is.null(cDep)) {
      stop("dependency must be a Scenario, character string, integer, or vector/list of these.")
    }
    cDepOutName <- paste0(allScns$name[allScns$scenarioId == cDep], " [", cDep, "]")
    
    # if Add
    if (!remove) {
      if ((nrow(dependencySet) > 0) && is.element(cDep, dependencySet$scenarioId)) {
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
      if ((nrow(dependencySet) == 0) || !is.element(cDep, dependencySet$scenarioId)) {
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
