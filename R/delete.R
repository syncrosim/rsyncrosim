# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Delete SsimLibrary, Project, Scenario, Datasheet
#'
#' Deletes one or more items. Note that this is irreversible.
#'
#' @param ssimObject \code{\link{SsimLibrary}}, \code{\link{Project}},
#'     or \code{\link{Scenario}} object, or character (i.e. path to a SsimLibrary)
#' @param project character string, numeric, or vector of these. One or more 
#'     \code{\link{Project}} names or ids. Note that project argument is ignored 
#'     if SsimObject is a list. Note that integer ids are slightly faster (optional)
#' @param scenario character string, numeric, or vector of these. One or more 
#'     \code{\link{Scenario}} names or ids. Note that Scenario argument is 
#'     ignored if SsimObject is a list. Note that integer ids are slightly faster 
#'     (optional)
#' @param datasheet character string or vector of these. One or more Datasheet 
#' names (optional)
#' @param force logical. If \code{FALSE} (default), user will be prompted to approve 
#'     removal of each item
#' 
#' @return 
#' Invisibly returns a list of boolean values corresponding to each
#' input: \code{TRUE} upon success (i.e.successful deletion) and \code{FALSE} upon failure.
#' 
#' @examples
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "a project")
#' 
#' # Check the Projects associated with this SsimLibrary
#' project(myLibrary)
#' 
#' # Delete Project
#' delete(myLibrary, project = "a project", force = TRUE)
#' 
#' # Check that Project was successfully deleted from SsimLibrary
#' project(myLibrary)
#' }
#' 
#' @export
# Note delete supports character paths because sometimes we want to delete a library without updating it.
# Note delete supports project/scenario arguments because sometimes we want to delete objects without creating them.
setGeneric("delete", function(ssimObject, project = NULL, scenario = NULL, datasheet = NULL, force = FALSE) standardGeneric("delete"))

#' @rdname delete
setMethod("delete", signature(ssimObject = "character"), function(ssimObject, project, scenario, datasheet, force) {
  if (is.null(datasheet) & is.null(project) & is.null(scenario)) {
    return(deleteLibrary(ssimObject, force))
  } else {
    if (ssimObject == SyncroSimNotFound(warn = FALSE)) {
      return(SyncroSimNotFound())
    }
    
    ssimObject <- .ssimLibrary(ssimObject)
    return(delete(ssimObject, project, scenario, datasheet, force))
  }
})

#' @rdname delete
setMethod("delete", signature(ssimObject = "SsimObject"), function(ssimObject, project, scenario, datasheet, force) {
  ScenarioId <- NULL
  
  xProjScn <- .getFromXProjScn(ssimObject, project = project, scenario = scenario, returnIds = TRUE, convertObject = FALSE, complainIfMissing = TRUE)
  
  # expect to have a vector of valid project or scenario ids - checking already done
  x <- xProjScn$ssimObject
  project <- xProjScn$project
  scenario <- xProjScn$scenario
  goal <- xProjScn$goal
  
  if (goal == "library") {
    if (is.null(datasheet)) {
      out <- deleteLibrary(ssimObject, force)
    } else {
      datasheets <- .datasheets(ssimObject)
      out <- deleteDatasheet(datasheet, datasheets, cProj = NULL, cScn = NULL, cProjName = NULL, cScnName = NULL, force = force)
    }
    
    if (out == "saved"){
      message("Library deleted")
      out <- TRUE
    } else {
      message(out)
      out <- FALSE
    }
    
    return(invisible(out))
  }
  
  if (goal == "project") {
    allProjects <- xProjScn$projectSet
    
    if (!is.numeric(project)) {
      stop("Error in delete: project ids are not numeric.")
    }
    
    if (!is.null(datasheet)) {
      if (is.element(class(ssimObject), c("Project", "Scenario"))) {
        datasheets <- .datasheets(ssimObject, refresh = TRUE)
      } else {
        datasheets <- .datasheets(.project(ssimObject, project = project[1]))
      }
    }
    
    out <- list()
    for (i in seq(length.out = length(project))) {
      cProj <- project[i]
      name <- allProjects$name[allProjects$projectId == cProj]
      
      # If datasheets(s) specified delete them. Otherwise delete the projects.
      if (!is.null(datasheet)) {
        outBit <- deleteDatasheet(x, datasheet, datasheets, cProj = cProj, cScn = NULL, cProjName = name, cScnName = NULL, out = out, force = force)
        
        if (outBit == "saved"){ 
          message(paste0("Datasheet " , datasheet, " deleted"))
          outBit <- TRUE
        } else{
          message(outBit)
          outBit <- FALSE
        }
        
      } else {
        if (force) {
          answer <- "y"
        } else {
          answer <- readline(prompt = paste0("Do you really want to delete project ", name, "(", cProj, ")? (y/n): "))
        }
        if (answer == "y") {
          outBit <- command(list(delete = NULL, project = NULL, lib = .filepath(x), pid = cProj, force = NULL), .session(x))
        } else {
          outBit <- paste0("Deletion of project " , cProj, " skipped")
        }
        
        if (outBit == "saved"){ 
          message(paste0("Project " , cProj, " deleted"))
          outBit <- TRUE
        } else{
          message(outBit)
          outBit <- FALSE
        }
        
      }
      
      out[[as.character(cProj)]] <- outBit
    }
    
    if (length(out) == 1) {
      out <- out[[1]]
    }
    
    return(invisible(out))
  }
  
  if (goal == "scenario") {
    allScenarios <- xProjScn$scenarioSet
    
    if (!is.numeric(scenario)) {
      stop("Error in delete: expect to have valid scenario ids.")
    }
    
    if (!is.null(datasheet)) {
      if (is.element(class(ssimObject), c("Scenario"))) {
        datasheets <- .datasheets(ssimObject, refresh = TRUE)
        scenarioSet <- scenario(.ssimLibrary(ssimObject), summary = TRUE)
      } else {
        datasheets <- .datasheets(.scenario(ssimObject, scenario = scenario[1]))
        scenarioSet <- scenario(ssimObject, summary = TRUE)
      }
    }
    
    out <- list()
    for (i in seq(length.out = length(scenario))) {
      cScn <- scenario[i]
      name <- allScenarios$Name[allScenarios$ScenarioId == cScn]
      
      if (!is.null(datasheet)) {
        cProj <- subset(scenarioSet, ScenarioId == cScn)$ProjectId
        outBit <- deleteDatasheet(x, datasheet = datasheet, datasheets = datasheets, 
                                  cProj = cProj, cScn = cScn, cProjName = "", 
                                  cScnName = name, out = out, force = force)
        
        if (outBit == "saved"){ 
          message(paste0("Datasheet " , datasheet, " deleted"))
          outBit <- TRUE
        } else{
          message(outBit)
          outBit <- FALSE
        }
        
      } else {
        if (force) {
          answer <- "y"
        } else {
          answer <- readline(prompt = paste0("Do you really want to remove scenario ", name, "(", cScn, ")? (y/n): "))
        }
        if (answer == "y") {
          outBit <- command(list(delete = NULL, scenario = NULL, lib = .filepath(x), sid = cScn, force = NULL), .session(x))
        } else {
          outBit <- paste0("Deletion of scenario " , cProj, " skipped")
        }
        
        if (outBit == "saved"){ 
          message(paste0("Scenario " ,cScn, " deleted"))
          outBit <- TRUE
        } else{
          message(outBit)
          outBit <- FALSE
        }
        
      }
      
      out[[as.character(cScn)]] <- outBit
    }
    
    if (length(out) == 1) {
      out <- out[[1]]
    }
    return(invisible(out))
  }
  
  stop("Error in delete().")
})
