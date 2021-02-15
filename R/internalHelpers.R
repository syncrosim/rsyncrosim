# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

SyncroSimNotFound <- function(inMessage = NULL, warn = TRUE) {
  outMessage <- "SyncroSim not found."
  if (!is.null(inMessage)) {
    if (inMessage != outMessage) {
      stop(inMessage)
    }
  }
  if (warn) {
    warning(outMessage)
  }
  return(outMessage)
}

backupEnabled <- function(path) {
  drv <- DBI::dbDriver("SQLite")
  con <- DBI::dbConnect(drv, path)

  tableExists <- DBI::dbExistsTable(con, "core_Backup")
  
  # Check for existence of table, if it does not exist assume we want to 
  # go ahead with the backup
  if(tableExists){
    ret <- DBI::dbGetQuery(con, "SELECT * FROM core_Backup")
    DBI::dbDisconnect(con)
  } else{
    DBI::dbDisconnect(con)
    return(TRUE)
  }

  if (is.na(ret$BeforeUpdate)) {
    return(FALSE)
  }

  if (ret$BeforeUpdate == 0) {
    return(FALSE)
  }

  return(TRUE)
}

deleteDatasheet <- function(x, datasheet, datasheets, cProj = NULL, cScn = NULL, cProjName = NULL, cScnName = NULL, out = list(), force) {
  out <- list()
  lib = ssimLibrary(.filepath(x), summary=T)
  pkg = lib$value[lib$property == "Package Name:"]
  
  for (j in seq(length.out = length(datasheet))) {
    cName <- datasheet[j]
    
    if (!grepl("_", cName, fixed = TRUE)) {
      cName <- paste0(pkg, "_", cName)
    }
    
    cSheet <- subset(datasheets, name == cName)
    if (nrow(cSheet) == 0) {
      stop("datasheet ", cName, " not found in object identified by ssimObject/project/scenario arguments.")
    }
    targs <- list(delete = NULL, data = NULL, lib = .filepath(x), sheet = cName, force = NULL)
    outName <- cName
    if (cSheet$scope == "project") {
      targs[["pid"]] <- cProj
      outName <- paste0(outName, " pid", cProj)
      addPrompt <- paste0(" from project ", cProjName, "(", cProj, ")")
    }
    if (cSheet$scope == "scenario") {
      targs[["sid"]] <- cScn
      outName <- paste0(outName, " sid", cScn)
      addPrompt <- paste0(" from scenario ", cScnName, "(", cScn, ")")
    }

    if (force) {
      answer <- "y"
    } else {
      promptString <- paste0("Do you really want to delete datasheet ", cName, addPrompt, "? (y/n): ")
      answer <- readline(prompt = promptString)
    }
    if (!is.element(outName, names(out))) { # don't try something again that has already been tried
      if (answer == "y") {
        outBit <- command(targs, .session(x))
      } else {
        outBit <- "skipped"
      }
      out[[outName]] <- outBit
    }
  }
  if (length(out) == 1) {
    out <- out[[1]]
  }
  return(out)
}

getIdsFromListOfObjects <- function(ssimObject, expecting = NULL, scenario = NULL, project = NULL) {
  if (is.null(expecting)) {
    expecting <- class(ssimObject[[1]])
  }
  if (class(ssimObject[[1]]) != expecting) {
    if (expecting == "character") {
      stop("Expecting a list of library paths.")
    } else {
      stop("Expecting a list of ", expecting, "s.")
    }
  }
  cLib <- .ssimLibrary(ssimObject[[1]])
  if (!is.null(scenario)) {
    warning("scenario argument is ignored when ssimObject is a list.")
  }
  if (!is.null(project)) {
    warning("project argument is ignored when ssimObject is a list.")
  }
  objs <- c()
  for (i in seq(length.out = length(ssimObject))) {
    cObj <- ssimObject[[i]]
    if (expecting == "character") {
      cObj <- .ssimLibrary(cObj)
    }

    if (class(cObj) != expecting) {
      stop("All elements of ssimObject should be of the same type.")
    }
    if ((expecting != "character") && (.filepath(cObj) != .filepath(cLib))) {
      stop("All elements of ssimObject must belong to the same library.")
    }
    if (expecting == "Scenario") {
      objs <- c(objs, .scenarioId(cObj))
    }
    if (expecting == "Project") {
      objs <- c(objs, .projectId(cObj))
    }
    if (is.element(expecting, c("character", "SsimLibrary"))) {
      objs <- c(objs, cObj)
    }
  }
  ssimObject <- cLib
  if (expecting == "character") {
    expecting <- "SsimLibrary"
  }
  return(list(ssimObject = ssimObject, objs = objs, expecting = expecting))
}

# get scnSet
getScnSet <- function(ssimObject) {
  # get current scenario info
  tt <- command(list(list = NULL, scenarios = NULL, csv = NULL, lib = .filepath(ssimObject)), .session(ssimObject))
  scnSet <- .dataframeFromSSim(tt, localNames = TRUE, convertToLogical = c("readOnly"))
  names(scnSet)[names(scnSet) == "scenarioID"] <- "scenarioId"
  names(scnSet)[names(scnSet) == "projectID"] <- "projectId"
  if (nrow(scnSet) == 0) {
    scnSet <- merge(scnSet, data.frame(scenarioId = NA, exists = NA), all = TRUE)
    scnSet <- subset(scnSet, !is.na(scenarioId))
  } else {
    scnSet$exists <- TRUE
  }
  return(scnSet)
}

# get projectSet
getProjectSet <- function(ssimObject) {
  tt <- command(list(list = NULL, projects = NULL, csv = NULL, lib = .filepath(ssimObject)), .session(ssimObject))
  projectSet <- .dataframeFromSSim(tt, localNames = TRUE, convertToLogical = c("readOnly"))
  if (nrow(projectSet) == 0) {
    projectSet[1, "iD"] <- NA
  }
  names(projectSet)[names(projectSet) == "iD"] <- "projectId"
  projectSet$exists <- TRUE
  projectSet <- subset(projectSet, !is.na(projectId))
  return(projectSet)
}

# make first character of string lower case
camel <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}

# https://stackoverflow.com/questions/26083625/how-do-you-include-data-frame-output-inside-warnings-and-errors
printAndCapture <- function(x) {
  paste(capture.output(print(x)), collapse = "\n")
}

# Get name of parent scenario from result scenario name.
.getParentName <- function(x) {
  out <- strsplit(x, " ([", fixed = TRUE)[[1]][1]
  return(out)
}

# Dataframe from SyncroSim output
#
# Converts output from SyncroSim to a dataframe.
#
# @param x Output from \code{\link{command()}}
# @param colNames A vector of column names.
# @param csv If T assume comma separation. Otherwise, assume undefined white space separation.
# @param localNames If T, remove spaces from column names and make camelCase.
# @return A data frame of output from the SyncroSim console.
# @examples
# # Use a default session to create a new library
# myArgs = list(list=NULL,columns=NULL,lib=".",sheet="stsim_Stratum",pid=1)
# myOutput = command(args=myArgs,mySsim)
# myDataframe = dataframeFromSSim(myOutput)
# myDataframe
#
# Note: this function is now internal. Should now only be called from datasheet.

.dataframeFromSSim <- function(x, colNames = NULL, csv = TRUE, localNames = TRUE, convertToLogical = NULL) {
  if (is.null(colNames)) {
    header <- TRUE
  } else {
    header <- FALSE
  }
  if (csv) {
    con <- textConnection(x)
    out <- read.csv(con, stringsAsFactors = FALSE, header = header)
    close(con)
  } else {
    if (1) {
      # Do the old wierd thing if not csv
      while (max(grepl("   ", x, fixed = TRUE))) {
        x <- gsub("   ", "  ", x, fixed = TRUE)
      }
      x <- gsub("  ", ",", x, fixed = TRUE)
      con <- textConnection(x)
      out <- read.csv(con, stringsAsFactors = FALSE, header = header, sep = ",", encoding = "UTF-8")
      if (!is.null(colNames)) {
        lastName <- names(out)[length(names(out))]
        if ((ncol(out) > length(colNames)) & (sum(!is.na(out[[lastName]])) == 0)) {
          out[[lastName]] <- NULL
        }
        names(out) <- colNames
      }
    }
    close(con)
  }
  if (localNames) {
    names(out) <- gsub(" ", "", names(out))
    names(out) <- gsub(".", "", names(out), fixed = TRUE)
    names(out) <- sapply(names(out), camel)
  }
  if (!is.null(convertToLogical)) {
    for (i in seq(length.out = length(convertToLogical))) {
      cName <- convertToLogical[[i]]
      if (is.element(cName, names(out))) {
        out[[cName]][out[[cName]] == "No"] <- FALSE
        out[[cName]][out[[cName]] == "Yes"] <- TRUE
        out[[cName]] <- as.logical(out[[cName]])
      }
    }
  }
  return(out)
}

# Gets datasheet summary info from an SsimLibrary, Project or Scenario.
#
# @details
# See \code{\link{datasheet}} for discussion of optional/empty/sheetName/lookupsAsFactors arguments.
# \itemize{
#   \item {If x/project/scenario identify a scenario: }{Returns library, project, and scenario scope datasheets.}
#   \item {If x/project/scenario identify a project (but not a scenario): }{Returns library and project scope datasheets.}
#   \item {If x/project/scenario identify a library (but not a project or scenario): }{Returns library scope datasheets.}
# }
#
# @param x An SsimLibrary, Project or Scenario object. Or a path to a SyncroSim library on disk.
# @param project Project name or id. Ignored if x is a Project.
# @param scenario Scenario name or id. Ignored if x is a Scenario.
# @param scope "scenario","project", "library", "all", or NULL.
# @param refresh If FALSE (default) names are retrieved from x@datasheetNames. If TRUE names are retrieved using a console call (slower).
# @return A dataframe of datasheet names.
# @examples
#
# Note: this function is now internal. Should now only be called from datasheet.

datasheets <- function(x, project = NULL, scenario = NULL, scope = NULL, refresh = FALSE) {
  if (!is(x, "SsimObject")) {
    stop("expecting SsimObject.")
  }
  
  x <- .getFromXProjScn(x, project, scenario)
  
  # Get datasheet dataframe
  if (!refresh) {
    datasheets <- x@datasheetNames
  } else {
    tt <- command(c("list", "datasheets", "csv", paste0("lib=", .filepath(x))), .session(x))
    datasheets <- .dataframeFromSSim(tt, convertToLogical = c("isOutput", "isSingle"))
    datasheets$scope <- sapply(datasheets$scope, camel)
    # TO DO - export this info from SyncroSim
  }
  datasheets$order <- seq(1, nrow(datasheets))
  if (!is.null(scope) && (scope == "all")) {
    datasheets$order <- NULL
    return(datasheets)
  }
  if (is.element(class(x), c("Project", "SsimLibrary"))) {
    datasheets <- subset(datasheets, scope != "scenario")
  }
  if (is.element(class(x), c("SsimLibrary"))) {
    datasheets <- subset(datasheets, scope != "project")
  }
  if (!is.null(scope)) {
    if (!is.element(scope, c("scenario", "project", "library"))) {
      stop("Invalid scope ", scope, ". Valid scopes are 'scenario', 'project', 'library' and NULL.")
    }
    cScope <- scope
    datasheets <- subset(datasheets, scope == cScope)
  }
  datasheets <- datasheets[order(datasheets$order), ]
  datasheets$order <- NULL
  return(datasheets)
}

# Internal helper - return uniquely identified and valid SyncroSim object

.getFromXProjScn <- function(ssimObject, project = NULL, scenario = NULL, convertObject = FALSE, returnIds = NULL, goal = NULL, complainIfMissing = TRUE) {
  # If x is scenario, ignore project and scenario arguments
  Freq <- NULL
  if (!is.element(class(ssimObject), c("character", "SsimLibrary", "Project", "Scenario"))) {
    stop("ssimObject should be a filepath, or an SsimLibrary/Scenario object.")
  }
  
  if (class(ssimObject) == "character") {
    ssimObject <- .ssimLibrary(ssimObject)
  }
  
  # Check for conflicts between ssimObject and project/scenario.
  if (is.element(class(ssimObject), c("Project", "Scenario")) & (!is.null(project))) {
    warning("project argument is ignored when ssimObject is a Project/Scenario or list of these.")
    project <- NULL
  }
  
  if (is.element(class(ssimObject), c("Scenario")) & (!is.null(scenario))) {
    warning("scenario argument is ignored when ssimObject is a Scenario or list of these.")
    scenario <- NULL
  }
  
  if (is.null(goal) & (!is.null(project) | (class(ssimObject) == "Project")) & is.null(scenario)) {
    goal <- "project"
    if (is.null(returnIds)) {
      if (length(project) > 1) {
        returnIds <- TRUE
      } else {
        returnIds <- FALSE
      }
    }
  }
  
  if (is.null(goal) & (!is.null(scenario) | (class(ssimObject) == "Scenario"))) {
    goal <- "scenario"
    if (is.null(returnIds)) {
      if (length(scenario) > 1) {
        returnIds <- TRUE
      } else {
        returnIds <- FALSE
      }
    }
  }
  
  if (is.null(goal)) {
    if (is.null(project) & is.null(scenario)) {
      if (!is.null(returnIds) && returnIds) {
        return(list(ssimObject = ssimObject, project = NULL, scenario = NULL, goal = "library"))
      } else {
        return(ssimObject)
      }
    }
    stop("Error in getFromXProjScn()")
  }
  
  # If the goal is a project, return one or more, or complain
  if (!is.null(goal) && (goal == "project")) {
    # if ssimObject is a scenario, return the parent project
    if ((class(ssimObject) == "Scenario")) {
      if (convertObject | !returnIds) {
        ssimObject <- new("Project", ssimObject, id = .projectId(ssimObject))
      }
    }
    if (is.element(class(ssimObject), c("Project", "Scenario"))) {
      if (returnIds) {
        project <- .projectId(ssimObject)
        if (convertObject) {
          ssimObject <- .ssimLibrary(ssimObject)
        }
      } else {
        return(ssimObject)
      }
    }
    scenario <- NULL
    # if not returned, need to get project
    
    # get current project info
    
    projectSet <- getProjectSet(ssimObject)
    
    if (is.null(project)) {
      if (nrow(projectSet) == 0) {
        if (returnIds) {
          projectSet$exists <- NULL
          return(list(ssimObject = ssimObject, project = NULL, scenario = NULL, projectSet = projectSet, goal = goal))
        } else {
          stop("No projects found in library.")
        }
      }
      project <- projectSet$projectId
    }
    
    # Now assume project is defined
    # distinguish existing projects from those that need to be made
    areIds <- is.numeric(project)
    
    if (areIds) {
      mergeBit <- data.frame(projectId = as.numeric(as.character(project)))
    } else {
      mergeBit <- data.frame(name = project, stringsAsFactors = FALSE)
    }
    mergeBit$order <- seq(1:length(project))
    fullProjectSet <- merge(projectSet, mergeBit, all = TRUE)
    missingProjects <- subset(fullProjectSet, is.na(fullProjectSet$exists) & (!is.na(fullProjectSet$order)))
    if (complainIfMissing & (nrow(missingProjects) > 0)) {
      if (areIds) {
        stop("Project ids (", paste(missingProjects$projectId, collapse = ","), ") not found in ssimObject. ")
      } else {
        stop("Projects (", paste(missingProjects$name, collapse = ","), ") not found in ssimObject. ")
      }
    }
    
    missingNames <- subset(missingProjects, is.na(missingProjects$name))
    if (areIds & (nrow(missingNames) > 0)) {
      stop("Project ids (", paste(missingNames$projectId, collapse = ","), ") not found in ssimObject. To make new projects, please provide names (as one or more character strings) to the project argument of the project() function. SyncroSim will automatically assign project ids.")
    }
    
    # Stop if an element of project corresponds to more than one existing row of the project list
    if (!areIds) {
      checkDups <- subset(fullProjectSet, !is.na(order))
      dupNames <- subset(as.data.frame(table(checkDups$name)), Freq > 1)
      if (nrow(dupNames) > 0) {
        # report the first error only
        cName <- dupNames$Var1[1]
        cIds <- checkDups$projectId[checkDups$name == cName]
        stop(paste0("The library contains more than one project called ", cName, ". Specify a project id: ", paste(cIds, collapse = ",")))
      }
    }
    
    smallProjectSet <- subset(fullProjectSet, !is.na(order))
    if (!returnIds) {
      if (nrow(smallProjectSet) > 1) {
        stop("Cannot uniquely identify a project from ssimObject/project arguments.")
      }
      if (!smallProjectSet$exists) {
        stop("Project ", project, " not found in the ssimObject.")
      }
      return(new("Project", ssimObject, id = smallProjectSet$projectId, projects = fullProjectSet))
    }
    if (sum(is.na(smallProjectSet$exists)) == 0) {
      project <- smallProjectSet$projectId
    }
    
    return(list(ssimObject = ssimObject, project = project, scenario = scenario, projectSet = fullProjectSet, goal = goal))
  }
  
  # if goal is scenario, and we have one, return immediately
  if (!is.null(goal) && (goal == "scenario")) {
    if (is.element(class(ssimObject), c("Scenario"))) {
      if (returnIds) {
        project <- .projectId(ssimObject)
        scenario <- .scenarioId(ssimObject)
      } else {
        return(ssimObject)
      }
    }
    
    if (class(ssimObject) == "Project") {
      project <- .projectId(ssimObject)
    }
    
    if (convertObject & returnIds & is.element(class(ssimObject), c("Scenario", "Project"))) {
      ssimObject <- .ssimLibrary(ssimObject)
    }
    
    scnSet <- getScnSet(ssimObject)
    if (!is.null(project)) {
      scnSet <- subset(scnSet, is.element(projectId, project))
    }
    if (!is.null(scenario) && is.numeric(scenario)) {
      scnSet <- subset(scnSet, is.element(scenarioId, scenario))
    }
    if (is.null(scenario)) {
      if (nrow(scnSet) == 0) {
        if (returnIds) {
          scnSet$exists <- NULL
          return(list(ssimObject = ssimObject, project = NULL, scenario = NULL, scenarioSet = scnSet, goal = goal))
        } else {
          stop("No scenarios found in ssimObject.")
        }
      }
      scenario <- scnSet$scenarioId
    }
    
    # Now assume scenario is defined
    # distinguish existing scenarios from those that need to be made
    areIds <- is.numeric(scenario)
    if (areIds) {
      mergeBit <- data.frame(scenarioId = scenario)
    } else {
      mergeBit <- data.frame(name = scenario, stringsAsFactors = FALSE)
    }
    if (!is.null(project)) {
      mergeBit$projectId <- project
    }
    mergeBit$order <- seq(1:length(scenario))
    fullScnSet <- merge(scnSet, mergeBit, all = TRUE)
    missingScns <- subset(fullScnSet, is.na(fullScnSet$exists) & (!is.na(fullScnSet$order)))
    if (complainIfMissing & (nrow(missingScns) > 0)) {
      if (areIds) {
        stop("Scenario ids (", paste(missingScns$scenarioId, collapse = ","), ") not found in ssimObject. ")
      } else {
        stop("Scenarios (", paste(missingScns$name, collapse = ","), ") not found in ssimObject. ")
      }
    }
    
    missingNames <- subset(missingScns, is.na(missingScns$name))
    if (areIds & (nrow(missingNames) > 0)) {
      stop("Scenario ids (", paste(missingNames$scenarioId, collapse = ","), ") not found in ssimObject. To make new scenarios, please provide names (as one or more character strings) to the scenario argument of the scenario() function. SyncroSim will automatically assign scenario ids.")
    }
    
    # For scenarios that need to be made, assign project or fail
    makeSum <- sum(!is.na(fullScnSet$order) & is.na(fullScnSet$exists))
    if (makeSum > 0) {
      if (is.null(project)) {
        allProjects <- project(ssimObject, summary = TRUE)
        if (nrow(allProjects) > 1) {
          stop("Can't create new scenarios because there is more than one project in the ssimObject. Please specify the Project ssimObject to which new scenarios should belong.")
        }
        if (nrow(allProjects) == 0) {
          obj <- project(ssimObject, project = "project1")
          project <- .projectId(obj)
        } else {
          project <- allProjects$projectId
        }
      }
      if (is.null(project) || is.na(project)) {
        stop("Something is wrong")
      }
      fullScnSet$projectId[!is.na(fullScnSet$order) & is.na(fullScnSet$exists)] <- project
    }
    
    # Stop if an element of scenarios corresponds to more than one existing row of the scenario list
    if (!areIds) {
      checkDups <- subset(fullScnSet, !is.na(order))
      dupNames <- subset(as.data.frame(table(checkDups$name)), Freq > 1)
      if (nrow(dupNames) > 0) {
        # report the first error only
        cName <- dupNames$Var1[1]
        cIds <- checkDups$scenarioId[checkDups$name == cName]
        stop(paste0("The ssimObject contains more than one scenario called ", cName, ". Specify a scenario id: ", paste(cIds, collapse = ",")))
      }
    }
    
    smallScenarioSet <- subset(fullScnSet, !is.na(order))
    if (!returnIds) {
      if (nrow(smallScenarioSet) > 1) {
        stop("Cannot uniquely identify a scenario from ssimObject/scenario arguments.")
      }
      if (!smallScenarioSet$exists) {
        stop("Scenario ", scenario, " not found in the ssimObject.")
      }
      return(new("Scenario", ssimObject, id = scenario, scenarios = fullScnSet))
    }
    if (sum(is.na(smallScenarioSet$exists)) == 0) {
      scenario <- smallScenarioSet$scenarioId
    }
    
    return(list(ssimObject = ssimObject, project = project, scenario = scenario, scenarioSet = fullScnSet, goal = goal))
  }
  stop(paste0("Could not identify a SsimLibrary, Project or Scenario from ssimObject, project, and scenario arguments."))
}

# Delete Library - internal helper function
#
# Deletes a SyncroSim library. Note this is irreversable.
#
# @param ssimLibrary SsimLibrary or path to a library
# @param force Logical. If FALSE (default) prompt to confirm that the library should be deleted. This is irreversable.
# @return "saved" or failure message.
# @export

setGeneric("deleteLibrary", function(ssimLibrary, force = FALSE) standardGeneric("deleteLibrary"))

setMethod("deleteLibrary", signature(ssimLibrary = "SsimLibrary"), function(ssimLibrary, force) {
  return(deleteLibrary(.filepath(ssimLibrary), force))
})

setMethod("deleteLibrary", signature(ssimLibrary = "character"), function(ssimLibrary, force) {
  if (!file.exists(ssimLibrary)) {
    stop(paste0("Library not found: ", ssimLibrary))
  }
  if (force) {
    answer <- "y"
  } else {
    answer <- readline(prompt = paste0("Do you really want to delete library ", ssimLibrary, "? (y/n): "))
  }
  if (answer == "y") {
    unlink(ssimLibrary)
    if (file.exists(ssimLibrary)) {
      return(paste0("Failed to delete ", ssimLibrary))
    }
    
    unlink(paste0(ssimLibrary, ".backup"), recursive = TRUE, force = TRUE)
    unlink(paste0(ssimLibrary, ".input"), recursive = TRUE, force = TRUE)
    unlink(paste0(ssimLibrary, ".output"), recursive = TRUE, force = TRUE)
    unlink(paste0(ssimLibrary, ".temp"), recursive = TRUE, force = TRUE)
    
    return("saved")
  } else {
    return("skipped")
  }
})