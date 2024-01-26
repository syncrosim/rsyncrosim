# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Run scenarios
#'
#' Run one or more SyncroSim \code{\link{Scenario}}(s).
#'
#' @param ssimObject \code{\link{SsimLibrary}}, \code{\link{Project}}, or
#'     \code{\link{Scenario}} object, or a list of Scenarios, or character (i.e.
#'     path to a SsimLibrary on disk)
#' @param scenario character, integer, or vector of these. Scenario names or ids. 
#'     If \code{NULL} (default), then runs all Scenarios associated with the SsimObject. Note 
#'     that integer ids are slightly faster
#' @param summary logical. If \code{FALSE} (default) result Scenario objects are returned. 
#'     If \code{TRUE} (faster) result Scenario ids are returned
#' @param copyExternalInputs logical. If \code{FALSE} (default) then a copy of external
#'     input files (e.g. GeoTIFF files) is not created for each multiprocessing job. Otherwise, a 
#'     copy of external inputs is created for each multiprocessing job. Applies only when 
#'     the number of jobs is set to >1 in the core_Multiprocessing datasheet.
#' @param transformerName character.  The name of the transformer to run (optional)
#' @param forceElements logical. If \code{TRUE} then returns a single result Scenario 
#'     as a named list; if \code{FALSE} (default) returns a single result Scenario as 
#'     a Scenario object. Applies only when \code{summary=FALSE}
#'     
#' @details
#' Note that breakpoints are ignored unless the SsimObject is a single Scenario.
#' 
#' @return 
#' If \code{summary = FALSE}, returns a result Scenario object or a named list 
#' of result Scenarios. The name is the parent Scenario for each result. If 
#' \code{summary = TRUE}, returns summary info for result Scenarios.
#' 
#' @examples 
#' \dontrun{
#' # Install helloworldSpatial package
#' installPackage("helloworldSpatial")
#' 
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set the SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session(printCmd=T)
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession, 
#'                          package = "helloworldSpatial",
#'                          template = "example-library",
#'                          forceUpdate = TRUE)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Run with default parameters
#' resultScenario <- run(myScenario)
#' 
#' # Only return summary information
#' resultScenario <- run(myScenario, summary = TRUE)
#' 
#' # Return results as a named list
#' resultScenario <- run(myScenario, forceElements = TRUE)
#' }
#' 
#' @export
setGeneric("run", function(ssimObject, scenario = NULL, summary = FALSE, copyExternalInputs = FALSE, transformerName = NULL, forceElements = FALSE) standardGeneric("run"))

#' @rdname run
setMethod("run", signature(ssimObject = "character"), function(ssimObject, scenario, summary, copyExternalInputs, transformerName, forceElements) {
  if (ssimObject == SyncroSimNotFound(warn = FALSE)) {
    return(SyncroSimNotFound())
  }
  ssimObject <- .ssimLibrary(ssimObject)
  out <- run(ssimObject, scenario, summary, copyExternalInputs, transformerName, forceElements)
  return(out)
})

#' @rdname run
setMethod("run", signature(ssimObject = "list"), function(ssimObject, scenario, summary, copyExternalInputs, transformerName, forceElements) {
  x <- getIdsFromListOfObjects(ssimObject, expecting = "Scenario", scenario = scenario)
  ssimObject <- x$ssimObject
  scenario <- x$objs
  out <- run(ssimObject, scenario, summary, copyExternalInputs, transformerName, forceElements)
  return(out)
})

#' @rdname run
setMethod("run", signature(ssimObject = "SsimObject"), function(ssimObject, scenario, summary, copyExternalInputs, transformerName, forceElements) {
  xProjScn <- .getFromXProjScn(ssimObject, scenario = scenario, convertObject = TRUE, returnIds = TRUE, goal = "scenario", complainIfMissing = TRUE)
  # Now assume scenario is x is valid object and scenario is valid vector of scenario ids
  x <- xProjScn$ssimObject
  scenario <- xProjScn$scenario
  scenarioSet <- xProjScn$scenarioSet

  if (!is.numeric(scenario)) {
    stop("Error in run(): expecting valid scenario ids.")
  }

  out <- list()
  addBits <- seq(1, length(scenario))
  for (i in seq(length.out = length(scenario))) {
    tt <- NULL
    cScn <- scenario[i]
    name <- scenarioSet$Name[scenarioSet$ScenarioId == cScn][1]
    resultId <- NA

    print(paste0("Running scenario [", cScn, "] ", name))

    if (is(ssimObject, "Scenario")) {
      breakpoints <- ssimObject@breakpoints
      xsim <- ssimObject
      xsim@breakpoints <- breakpoints
    } else {
      breakpoints <- NULL
    }

    if ((!is(breakpoints, "list")) | (length(breakpoints) == 0)) {
      args <- list(run = NULL, lib = .filepath(x), sid = cScn, copyextfiles = "no")

      if (!is.null(transformerName)) {
        args[["trx"]] <- transformerName
      }
      
      if (copyExternalInputs == TRUE) {
        args[["copyextfiles"]] <- "yes"
      }

      tt <- command(args, .session(x))

      for (i in tt) {
        if (startsWith(i, "Result scenario ID is:")) {
          resultId <- strsplit(i, ": ", fixed = TRUE)[[1]][2]
        } else {
          print(i)
        }
      }

      if (is.na(resultId)) {
        stop()
      }
    } else {

      # create a session
      xsim@breakpoints <- breakpoints
      cBreakpointSession <- breakpointSession(xsim)
      # TO DO: multiple tries in connection

      # load a library
      msg <- paste0('load-library --lib=\"', filepath(x), '\"')
      ret <- remoteCall(cBreakpointSession, msg)
      if (ret != "NONE") {
        stop("Something is wrong: ", ret)
      }

      # set breakpoints
      ret <- setBreakpoints(cBreakpointSession)
      if (ret != "NONE") {
        stop("Something is wrong: ", ret)
      }

      resultId <- run(cBreakpointSession)
      resp <- writeLines("shutdown", connection(cBreakpointSession), sep = "")
      close(connection(cBreakpointSession)) # Close the connection.
    }
    inScn <- paste0(name, " (", cScn, ")")

    if (is.element(inScn, names(out))) {
      inScn <- paste(inScn, addBits[i])
    }
    if (!identical(resultId, suppressWarnings(as.character(as.numeric(resultId))))) {
      out[[inScn]] <- tt
      print(tt)
    } else {
      if (summary) {
        out[[inScn]] <- as.numeric(resultId)
        scn <- .scenario(x, scenario = as.numeric(resultId))
      } else {
        out[[inScn]] <- .scenario(x, scenario = as.numeric(resultId))
      }
    }
  }

  if (summary && (is(out, "list"))) {
    # summary info for ids
    scnSelect <- unlist(out)
    out <- .scenario(x, scenario = scnSelect, summary = TRUE)
  }

  if (!forceElements && (is(out, "list")) && (length(out) == 1)) {
    out <- out[[1]]
  }
  return(out)
})
