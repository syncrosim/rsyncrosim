# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

setMethod(
  f = "initialize", signature = "Scenario",
  definition = function(.Object, ssimLibrary = NULL, project = NULL, name = NULL, id = NULL, sourceScenario = NULL, scenarios = NULL, folder = NULL) {
    
    ProjectID <- NULL
    ScenarioID <- NULL
    Name <- NULL
    
    # assume this is being called from scenario fn or getFromXProjScn(). ssimObject and pid are valid, id is valid if not null, and duplicate name problems have been sorted out.
    .Object@breakpoints <- list()
    .Object@parentId <- 0
    
    x <- ssimLibrary
    
    if (is.null(folder)){
      folder <- 0
    }

    # For fast processing - quickly return without system calls if scenario exists
    if (is.null(scenarios)) {
      scenarios <- getScnSet(x)
    }
    allScenarios <- scenarios
    if (!is.null(name)) {
      cName <- name
      scenarios <- subset(scenarios, Name == cName)
    }
    if (!is.null(id)) {
      scenarios <- subset(scenarios, ScenarioID == id)
    }
    if (!is.null(project)) {
      scenarios <- subset(scenarios, ProjectID == project)
    }

    findScn <- subset(scenarios, !is.na(ScenarioID))
    if (nrow(findScn) > 1) {
      stop("Something is wrong.")
    }

    # If found only one, open it.
    if (nrow(findScn) == 1) {
      if (!is.null(sourceScenario)) {
        stop("Scenario ", name, " already exists. Delete the scenario before replacing it.")
      }
      if (findScn$IsResult == "Yes") {
        scnNameList <- strsplit(findScn$Name, "[", fixed=TRUE)[[1]]
        parentBit <- scnNameList[length(scnNameList)]
        parent <- strsplit(parentBit, "]", fixed = TRUE)[[1]][1]
        .Object@parentId <- as.numeric(parent)
      }
      
      pid <- as.numeric(findScn$ProjectID)
      sid <- as.numeric(findScn$ScenarioID)
      folderId <- addScenarioToFolder(x, pid, sid, folder)

      # Go ahead and create the Scenario object without issuing system commands to make sure it is ok
      .Object@session <- .session(x)
      .Object@filepath <- .filepath(x)
      .Object@datasheetNames <- .datasheets(x, scope = "all", refresh = TRUE)
      .Object@scenarioId <- sid
      .Object@projectId <- pid
      .Object@folderId <- folderId
      return(.Object)
    }

    # If given an id for a scenario that does not yet exist, complain
    if (is.null(name)) {
      stop(paste0("The library does not contain scenario id ", id, ". Please provide a name for the new scenario - the id will be assigned automatically by SyncroSim."))
    }

    # Now go ahead to handle odder cases
    # Assume pid is valid, and name is defined, not duplicated, etc
    # x can be either a project or a library - but need a project in order to create a new scenario
    pid <- project

    # Create a new scenario
    if (is.null(sourceScenario)) {
      tt <- command(list(create = NULL, scenario = NULL, lib = .filepath(x), name = name, pid = pid), .session(x))
    } else {
      sid <- sourceScenario
      slib <- .filepath(x)
      if (is(sourceScenario, "numeric")) {
        if (!is.element(sourceScenario, allScenarios$ScenarioID)) {
          stop("Source scenario id ", sourceScenario, " not found in SsimLibrary.")
        }
      }
      if (is(sourceScenario, "character")) {
        sourceOptions <- subset(allScenarios, Name == sourceScenario)
        if (nrow(sourceOptions) == 0) {
          stop(paste0("Source scenario name ", sourceScenario, " not found in SsimLibrary."))
        }
        if (nrow(sourceOptions) > 1) {
          stop(paste0("There is more than one scenario called ", sourceScenario, " in the SsimLibrary. Please provide a sourceScenario id: ", paste(sourceOptions$scenarioId, collapse = ",")))
        }
        sid <- sourceOptions$ScenarioID
      }

      if (is(sourceScenario, "Scenario")) {
        sid <- .scenarioId(sourceScenario)
        slib <- .filepath(sourceScenario)
        sourceScnName <- name(sourceScenario)
      } else {
        sourceScnName <- subset(allScenarios, ScenarioID == sid)$Name
      }

      if (name == "GetSourceCopyCopyCopy") {
        copyName <- paste(sourceScnName, "- Copy")
        if (!is.element(copyName, allScenarios$Name)) {
          name <- copyName
        } else {
          done <- FALSE
          count <- 0
          while (!done) {
            count <- count + 1
            cName <- paste0(copyName, count)
            if (!is.element(cName, allScenarios$Name)) {
              name <- cName
              done <- TRUE
            }
          }
        }
      }

      tt <- command(list(copy = NULL, scenario = NULL, slib = slib, tlib = .filepath(x), name = name, sid = sid, pid = pid), .session(x))
    }
    id <- as.numeric(strsplit(tt, ": ")[[1]][2])
    
    # Move Scenario into folder (if specified)
    folderId <- addScenarioToFolder(x, as.numeric(pid), as.numeric(id), folder)

    .Object@session <- .session(x)
    .Object@filepath <- .filepath(x)
    .Object@datasheetNames <- .datasheets(x, refresh = TRUE, scope = "all")
    .Object@scenarioId <- as.numeric(id)
    .Object@projectId <- as.numeric(pid)
    .Object@folderId <- folderId
    return(.Object)
  }
)

#' Create or open Scenario(s)
#'
#' Create or open one or more \code{\link{Scenario}}s from a 
#' \code{\link{SsimLibrary}}.
#'
#' @param ssimObject \code{\link{SsimLibrary}} or \code{\link{Project}} object,
#'   or character (i.e. a filepath)
#' @param scenario character, integer, or vector of these. Names or ids of one or 
#'     more Scenarios. Note integer ids are slightly faster, but can only be 
#'     used to open existing Scenarios
#' @param sourceScenario character or integer. If not \code{NULL} (Default), new 
#' Scenarios will be copies of the sourceScenario
#' @param folder \code{\link{Folder}} object, character, or integer. The Folder object,
#'     name (must be unique), or Folder ID. If not \code{NULL} (Default), new 
#'     Scenarios will be moved into the specified folder
#' @param summary logical. If \code{TRUE} then loads and returns the Scenario(s) in a 
#'     named vector/dataframe with the scenarioId, name, description, owner, 
#'     dateModified, readOnly, parentID. Default is \code{TRUE} if \code{scenario=NULL}, 
#'     \code{FALSE} otherwise
#' @param results logical. If \code{TRUE} only return result Scenarios. Default is 
#' \code{FALSE}
#' @param forceElements logical. If \code{TRUE} then returns a single Scenario as a named 
#'     list; if \code{FALSE} (default), returns a single Scenario as a 
#'     \code{\link{Scenario}} object. Applies only when \code{summary=FALSE}
#' @param overwrite logical. If \code{TRUE} an existing Scenario will be overwritten.
#' Default is \code{FALSE}
#' 
#' @return 
#' A \code{Scenario} object representing a SyncroSim scenario, a list of Scenario 
#' objects, or a data frame of Scenario names and descriptions. If \code{summary = FALSE}, 
#' returns one or more \code{\link{Scenario}} objects representing SyncroSim Scenarios.
#' If \code{summary = TRUE}, returns Scenario summary info.
#' 
#' @details
#'
#' For each element of Scenario:
#' \itemize{
#'   \item {If element/Project/SsimObject uniquely identifies an existing 
#'          Scenario: }{Returns the existing Scenario}.
#'   \item {If element/Project/SsimObject uniquely identifies more than one existing 
#'          Scenario: }{Error}.
#'   \item {If element/Project/SsimObject do not identify an existing Scenario or 
#'          Project: }{Error}.
#'   \item {If element/Project/SsimObject do not identify an existing Scenario and 
#'          element is numeric: }{Error - a name is required for new Scenarios. 
#'          SyncroSim will automatically assign an id when a Scenario is created.}
#'   \item {If element/Project/SsimObject do not identify an existing Scenario and 
#'          do identify a Project, and element is a character string: }{Creates a 
#'          new Scenario named element in the Project. SyncroSim automatically 
#'          assigns an id. If sourceScenario is not \code{NULL} the new Scenario will be 
#'          a copy of sourceScenario.}
#' }
#' 
#' @examples
#' \dontrun{
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set the SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession) 
#' myProject <- project(myLibrary, project = "My Project")
#' 
#' # Create a new Scenario
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' 
#' # Create a new Scenario from an existing Scenario
#' myScenarioCopy <- scenario(myProject, scenario = "My Scenario Copy",
#'                            sourceScenario = myScenario)
#'                           
#' # Find all the Scenarios in a SsimLibrary
#' scenario(myLibrary)
#' 
#' # Only return the results Scenarios for a SsimLibrary
#' scenario(myLibrary, results = TRUE)
#' 
#' # Overwrite an existing Scenario
#' myNewScenario <- scenario(myProject, scenario = "My New Scenario", 
#'                          overwrite = TRUE)
#' }
#' 
#' @name scenario
#' @export
scenario <- function(ssimObject = NULL, scenario = NULL, sourceScenario = NULL, folder = NULL, summary = NULL, results = FALSE, forceElements = FALSE, overwrite = FALSE) {
  if (is.character(ssimObject) && (ssimObject == SyncroSimNotFound(warn = FALSE))) {
    return(SyncroSimNotFound())
  }

  if (is.null(ssimObject)) {
    e <- ssimEnvironment()
    ssimObject <- ssimLibrary(e$LibraryFilePath)
    scenario <- as.integer(e$ScenarioId)
  }
  IsResult <- NULL
  # if ssimObject is a scenario return the scenario
  if (is.element(class(ssimObject), c("Scenario")) & is.null(scenario)) {
    if (is.null(summary)) {
      summary <- FALSE
    }
    if (!summary) {
      convertObject <- TRUE
      returnIds <- FALSE
    } else {
      convertObject <- FALSE
      returnIds <- TRUE
    }
  } else {
    # set summary default
    if (is.null(summary)) {
      if (is.null(scenario)) {
        if (is.null(sourceScenario)) {
          summary <- TRUE
        } else {
          summary <- FALSE
          scenario <- "GetSourceCopyCopyCopy"
        }
      } else {
        summary <- FALSE
      }
    }
    convertObject <- FALSE
    returnIds <- TRUE
  }

  xProjScn <- .getFromXProjScn(ssimObject, project = NULL, scenario = scenario, convertObject = convertObject, returnIds = returnIds, goal = "scenario", complainIfMissing = FALSE)

  if (is(xProjScn, "Scenario")) {
    if (!overwrite) {
      return(xProjScn)
    }
  }

  if (!is(xProjScn, "list")) {
    stop("something is wrong")
  }
  ssimObject <- xProjScn$ssimObject
  project <- xProjScn$project
  scenario <- xProjScn$scenario
  allScenarios <- xProjScn$scenarioSet
  if (is.element("order", names(allScenarios))) {
    scnSet <- subset(allScenarios, !is.na(order))
  } else {
    if (nrow(allScenarios) > 0) {
      allScenarios$order <- seq(1, nrow(allScenarios))
    }
    scnSet <- allScenarios
  }

  if (results) {
    scnSet <- subset(scnSet, !is.element(IsResult, c(NA, FALSE, "No")))
  }

  if (nrow(scnSet) == 0) {
    if (summary) {
      scnSet$exists <- NULL
      scnSet$order <- NULL
      return(scnSet)
    } else {
      stop("Error in scenario(): No scenarios to get or make.")
    }
  }
  # if all projects exist and summary, simply return summary
  if ((sum(is.na(scnSet$exists)) == 0) & summary) {
    scnSet <- subset(scnSet, !is.na(order))
    scnSet <- scnSet[order(scnSet$order), ]
    scnSet[scnSet$IsReadOnly == "FALSE", "IsReadOnly"] <- "No"
    scnSet[scnSet$IsReadOnly == "TRUE", "IsReadOnly"] <- "Yes"
    scnSet$exists <- NULL
    scnSet$order <- NULL
    return(scnSet)
  }

  # Now assume scenario is defined
  # distinguish existing scenarios from those that need to be made
  areIds <- is.numeric(scenario)

  # if scenarios need to be made, pass all scenarios in library
  makeSum <- sum(!is.na(scnSet$order) & is.na(scnSet$exists))
  libScns <- subset(allScenarios, !is.na(exists))
  if (makeSum > 0) {
    if (!is.null(sourceScenario) && (is(sourceScenario, "Scenario"))) {
      libScns <- getScnSet(ssimObject) # get all scenarios for library, not just those from ssimObject
      # check validity in new("Scenario",...)
    }
  } else {
    if (!is.null(sourceScenario)) {
      warning("sourceScenario was ignored because scenario already exists.")
    }
  }

  # make scnenarios/scenario objects
  scnsToMake <- subset(scnSet, !is.na(order))
  if (overwrite) {
    for (i in seq(length.out = nrow(scnsToMake))) {
      if (is.na(scnsToMake$exists[i])) {
        next
      }
      ret <- delete(ssimObject, scenario = scnsToMake$ScenarioID[i], force = TRUE)
      cRow <- scnsToMake[i, ]
      scnsToMake[i, ] <- NA
      scnsToMake$Name[i] <- cRow$Name
      scnsToMake$ProjectID[i] <- cRow$ProjectID
      scnsToMake$order[i] <- cRow$order
    }
    libScns <- getScnSet(ssimObject)
  }
  if (summary | results) {
    scnsToMake <- subset(scnsToMake, is.na(exists))
  }
  if (nrow(scnsToMake) == 0) {
    fullScnSet <- fullScnSet[order(fullScnSet$order), ]
    fullScnSet$exists <- NULL
    fullScnSet$order <- NULL
    return(fullScnSet)
  }
  if (results & (nrow(scnsToMake) > 0)) {
    stop(paste0("Could not find these scenarios in the ssimObject. To create them, set results=F: ", paste(scnsToMake$Name, collapse = ",")))
  }
  scnsToMake <- scnsToMake[order(scnsToMake$order), ]
  scnList <- list()
  for (i in seq(length.out = nrow(scnsToMake))) {
    cRow <- scnsToMake[i, ]
    if (!is.na(cRow$exists)) {
      scnList[[as.character(scnsToMake$ScenarioID[i])]] <- new("Scenario", ssimObject, project = cRow$ProjectID, id = cRow$ScenarioID, scenarios = cRow)
    } else {
      obj <- new("Scenario", ssimObject, project = cRow$ProjectID, name = cRow$Name, sourceScenario = sourceScenario, scenarios = libScns, folder = folder)
      scnList[[as.character(.scenarioId(obj))]] <- obj
    }
  }

  if (!summary) {
    if ((length(scnList) == 1) & !forceElements) {
      scnList <- scnList[[1]]
    }
    return(scnList)
  }

  scnSetOut <- getScnSet(ssimObject)
  scnSetOut$exists <- NULL
  idList <- data.frame(ScenarioID = as.numeric(names(scnList)), order = seq(1:length(scnList)))
  scnSetOut <- merge(idList, scnSetOut, all.x = TRUE)
  if (sum(is.na(scnSetOut$Name)) > 0) {
    stop("Something is wrong with scenario()")
  }

  scnSetOut <- scnSetOut[order(scnSetOut$order), ]
  scnSetOut$order <- NULL
  return(scnSetOut)
}
