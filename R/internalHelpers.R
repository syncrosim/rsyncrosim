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

  ret <- DBI::dbGetQuery(con, "SELECT * FROM core_Backup")
  DBI::dbDisconnect(con)

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
  for (j in seq(length.out = length(datasheet))) {
    cName <- datasheet[j]
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

# http://stackoverflow.com/questions/26083625/how-do-you-include-data-frame-output-inside-warnings-and-errors
# @export
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
# myArgs = list(list=NULL,columns=NULL,lib="C:/Temp/NewLibrary.ssim",sheet="stsim_Stratum",pid=1)
# myOutput = command(args=myArgs,mySsim)
# myDataframe = dataframeFromSSim(myOutput)
# myDataframe
#
# Note: this function is now internal. Should now only be called from datasheet.

.dataframeFromSSim <- function(x, colNames = NULL, csv = T, localNames = T, convertToLogical = NULL) {
  if (is.null(colNames)) {
    header <- T
  } else {
    header <- F
  }
  if (csv) {
    con <- textConnection(x)
    out <- read.csv(con, stringsAsFactors = F, header = header)
    close(con)
  } else {
    if (1) {
      # Do the old wierd thing if not csv
      while (max(grepl("   ", x, fixed = T))) {
        x <- gsub("   ", "  ", x, fixed = T)
      }
      x <- gsub("  ", ",", x, fixed = T)
      con <- textConnection(x)
      out <- read.csv(con, stringsAsFactors = F, header = header, sep = ",", encoding = "UTF-8")
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
    names(out) <- gsub(".", "", names(out), fixed = T)
    names(out) <- sapply(names(out), camel)
  }
  if (!is.null(convertToLogical)) {
    for (i in seq(length.out = length(convertToLogical))) {
      cName <- convertToLogical[[i]]
      if (is.element(cName, names(out))) {
        out[[cName]][out[[cName]] == "No"] <- F
        out[[cName]][out[[cName]] == "Yes"] <- T
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

datasheets <- function(x, project = NULL, scenario = NULL, scope = NULL, refresh = F) {
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