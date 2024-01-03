# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

Breakpoint <- setClass("Breakpoint", representation(arguments = "character", breakpointName = "character", name = "character", transformerName = "character", callback = "function"))
setMethod(f = "initialize", signature = "Breakpoint", definition = function(.Object, breakpointName, transformerName, arguments, callback, name = "Main") {
  .Object@breakpointName <- breakpointName
  .Object@transformerName <- transformerName
  .Object@arguments <- paste(arguments, collapse = ",")
  .Object@callback <- callback
  .Object@name <- name
  return(.Object)
})

new_breakpoint <- function(breakpointName, transformerName, arguments, callback, name = "Main") {
  return(new("Breakpoint", breakpointName, transformerName, arguments, callback, name))
}

setOldClass("sockconn")
BreakpointSession <- setClass("BreakpointSession", representation(scenario = "Scenario", connection = "sockconn", name = "character", isMPJob = "logical"))
setMethod(f = "initialize", signature = "BreakpointSession", definition = function(.Object, scenario, ipAddress = "127.0.0.1", port = 13000, quiet = TRUE, name = "Main", startServer = TRUE, isMPJob = FALSE) {
  location <- filepath(session(scenario))

  if (startServer) {
    args <- list(ipaddress = ipAddress, port = port, quiet = quiet)
    if (isMPJob) {
      args <- c(args, "child-process" = TRUE)
    }
    tt <- command(args, .session(scenario), program = "SyncroSim.Server.exe", wait = FALSE)
  }
  .Object@connection <- connection(ipAddress, port)
  .Object@scenario <- scenario
  .Object@name <- name

  return(.Object)
})

breakpointSession <- function(scenario, ipAddress = "127.0.0.1", port = 13000, quiet = TRUE, name = "Main", startServer = TRUE, isMPJob = FALSE) {
  return(new("BreakpointSession", scenario, ipAddress, port, quiet, name, startServer, isMPJob))
}

setGeneric("connection", function(x, ...) standardGeneric("connection"))
setMethod("connection", signature(x = "missingOrNULLOrChar"), function(x = "127.0.0.1", port = 13000) {
  ipAddress <- x
  con <- socketConnection(host = ipAddress, port = port, open = "r+", encoding = "UTF-8", blocking = TRUE, server = FALSE, timeout = 4)
  if (!isOpen(con)) {
    stop(paste0("Problem connecting to the SyncroSim server. IP:", ipAddress, " Port:", port))
  }
  return(con)
})

setGeneric("connection<-", function(x, value) standardGeneric("connection<-"))
setMethod("connection", signature(x = "BreakpointSession"), function(x) {
  return(x@connection)
})
setReplaceMethod(
  f = "connection",
  signature = "BreakpointSession",
  definition = function(x, value) {
    if (!is.element("sockconn", class(value))) {
      stop("Must assign a socket connection object.")
    }
    x@connection <- value
    return(x)
  }
)

setGeneric("remoteCall", function(x, message, getResponse = TRUE) standardGeneric("remoteCall"))
setMethod("remoteCall", signature(x = "BreakpointSession"), function(x, message, getResponse) {
  if (!getResponse) {
    stop("handle this case")
  }

  ret <- ""
  tt <- writeLines(message, connection(x), sep = "") # Gives an error

  while (getResponse) {
    cRes <- suppressWarnings(readChar(connection(x), 1, useBytes = FALSE))
    if (length(cRes) == 0) {
      Sys.sleep(1) # try again in a while
    } else {
      # read the rest of the message
      res <- ""
      while (cRes != "\n") {
        res <- paste0(res, cRes)
        cRes <- suppressWarnings(readChar(connection(x), 1, useBytes = FALSE))
      }

      cmd <- strsplit(res, "|", fixed = TRUE)[[1]][1]
      if (cmd == "breakpoint-hit") {
        split <- strsplit(res, "|", fixed = TRUE)[[1]]
        tt <- onBreakpointHit(x, split)
        tt <- writeLines("breakpoint-continue", connection(x), sep = "")
      } else if (cmd == "call-complete") {
        split <- strsplit(res, "|", fixed = TRUE)[[1]]
        if (split[2] == "FAILURE") {
          stop("Server returned a failure: ", split[3])
        } else {
          ret <- split[3]
          break
        }
      } else {
        stop("server response invalid: ", cmd)
      }
    }
  }
  return(ret)
})

setGeneric("onBreakpointHit", function(x, split) standardGeneric("onBreakpointHit"))
setMethod("onBreakpointHit", signature(x = "BreakpointSession"), function(x, split) {
  bpkey <- paste0(split[[2]], ":", split[[3]])
  cBreak <- x@scenario@breakpoints[[bpkey]]
  cResult <- scenario(.project(x@scenario), scenario = as.numeric(split[4]))
  cBreak@callback(cResult, iteration = as.numeric(split[5]), timestep = as.numeric(split[6]))

  # Tell the breaking transformer to load any new data
  dataDir <- paste0(.filepath(cResult), ".temp/Data")
  if (file.exists(dataDir)) {
    msg <- "execute-command --name=data-ready"
    tt <- remoteCall(x, msg)
    if (tt != "NONE") {
      stop("Something is wrong: ", tt)
    }
    unlink(dataDir, recursive = TRUE)
  }
  NULL
})

setGeneric("setBreakpoints", function(x) standardGeneric("setBreakpoints"))
setMethod("setBreakpoints", signature(x = "BreakpointSession"), function(x) {
  cBreaks <- x@scenario@breakpoints
  if (length(cBreaks) == 0) {
    stop("Expecting breakpoints")
  }
  ret <- list()
  for (i in 1:length(cBreaks)) {
    cBreak <- cBreaks[[i]]
    msg <- paste0("set-breakpoint --name=", cBreak@breakpointName, " --trx=", cBreak@transformerName, " --granularity=", cBreak@arguments)
    ret[[names(cBreaks)[i]]] <- remoteCall(x, msg)
  }
  return(ret)
})

runJobParallel <- function(cPars) {
  ret <- tryCatch(
    {
      cScn <- scenario(.ssimLibrary(cPars$x, session = cPars$session), scenario = 1)
      if (!exists("cScn")) {
        stop("Problem with split-scenario: Can't find the library ", cPars$x, ".")
      }
      cScn@breakpoints <- cPars$breaks
      sess <- breakpointSession(cScn, port = cPars$port, name = paste0("Child=", cPars$port), startServer = TRUE, isMPJob = TRUE)
      if (!exists("sess")) {
        stop("Problem creating breakpoint session.")
      }
      msg <- paste0('load-library --lib=\"', filepath(cScn), '\"')
      ret <- remoteCall(sess, msg)
      if (ret != "NONE") {
        stop("Problem with ", msg, " :", ret)
      }
      ret <- setBreakpoints(sess)

      msg <- paste0("run-scenario --sid=", 1, " --jobs=1")
      ret <- remoteCall(sess, msg)
      if (as.character(as.numeric(ret)) != ret) {
        stop("Problem with ", msg, " :", ret)
      }
      "saved"
    },
    error = function(e) {
      print(e)
      return(e)
    },
    finally = {
      resp <- writeLines("shutdown", connection(sess), sep = "")
      close(connection(sess)) # Close the connection.
    }
  )
}

getBPNameLongForm <- function(breakpointType) {
  types <- list(
    bi = "stime:break-before-iteration",
    ai = "stime:break-after-iteration",
    bt = "stime:break-before-timestep",
    at = "stime:break-after-timestep"
  )

  if (!is.element(breakpointType, names(types))) {
    return(NULL)
  }

  return(types[[breakpointType]])
}

#' Add a Scenario breakpoint
#'
#' This function allows the user to add breakpoints to a SyncroSim model, for a 
#' given \code{\link{Scenario}}. When the Scenario is \code{\link{run}} the 
#' function specified by the callback argument will be called for the specified 
#' iterations or timesteps.
#'
#' @param x \code{\link{Scenario}} object
#' @param transformerName character. A Stochastic Time Transformer 
#' e.g. "stsim_Runtime" (optional)
#' @param breakpointType character. Options include "bi" (before iteration),
#' "ai" (after iteration), "bt" (before timestep), or "at" (after timestep) 
#' (optional)
#' @param arguments vector of timesteps or iterations e.g. c(1,2) (optional)
#' @param callback function to be called when the breakpoint is hit (optional)
#' 
#' @return 
#' A SyncroSim Scenario with an updated list of breakpoints.
#' 
#' @details Breakpoints are only supported for Stochastic Time Transformers.
#' 
#' @examples 
#' \dontrun{
#' # Create callback function
#' callbackFunction <- function(x, iteration, timestep) {
#'   print(paste0("Breakpoint hit: ", scenarioId(x)))
#' }
#' 
#' # Install helloworldSpatial package
#' addPackage("helloworldSpatial")
#' 
#' # Set SsimLibrary name
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set Session and SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession,
#'                          package = "helloworldSpatial")
#' myScenario <- scenario(myLibrary, "My Scenario")
#' 
#' # Add breakpoints before the 1st and 2nd iterations
#' myScenario <- addBreakpoint(x= myScenario,
#'                             transformerName= "helloworldSpatial_Primary",
#'                             breakpointType = "bi", 
#'                             arguments = c(1,2),
#'                             callback = callbackFunction)
#'                             
#' # Check that the breakpoints were added
#' breakpoint(myScenario)
#' }
#' @export
setGeneric("addBreakpoint", function(x, transformerName, breakpointType, arguments, callback) standardGeneric("addBreakpoint"))

#' @rdname addBreakpoint
setMethod("addBreakpoint", signature(x = "Scenario"), function(x, transformerName, breakpointType, arguments, callback) {
  breakpointName <- getBPNameLongForm(breakpointType)

  if (is.null(breakpointName)) {
    stop("breakpointType not recognized: ", breakpointType)
  }

  breakpointKey <- paste0(breakpointName, ":", transformerName)

  if (is.element(breakpointKey, names(x@breakpoints))) {
    warning("Resetting breakpoint for: ", breakpointName, " -> ", transformerName)
  }

  x@breakpoints[[breakpointKey]] <- new_breakpoint(breakpointName, transformerName, arguments, callback)
  return(x)
})

#' Delete a Scenario breakpoint
#'
#' This function will delete a \code{\link{Scenario}} breakpoint.
#'
#' @param x \code{\link{Scenario}} object
#' @param transformerName character. A Stochastic Time Transformer 
#' e.g. "stsim_Runtime" (optional)
#' @param breakpointType character. Options include "bi" (before iteration),
#' "ai" (after iteration), "bt" (before timestep), or "at" (after timestep) 
#' (optional)
#' 
#' @return A SyncroSim Scenario with an updated list of breakpoints.
#' 
#' @seealso \code{\link{addBreakpoint}}.
#' 
#' @examples
#' \dontrun{
#' # Create callback function
#' callbackFunction <- function(x, iteration, timestep) {
#'   print(paste0("Breakpoint hit: ", scenarioId(x)))
#' }
#' 
#' # Install helloworldSpatial package
#' addPackage("helloworldSpatial")
#' 
#' # Set SsimLibrary name
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set Session and SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession,
#'                          package = "helloworldSpatial")
#' myScenario <- scenario(myLibrary, "My Scenario")
#' 
#' # Add breakpoints before the 1st and 2nd iterations
#' myScenario <- addBreakpoint(x= myScenario,
#'                             transformerName= "helloworldSpatial_Primary",
#'                             breakpointType = "bi", 
#'                             arguments = c(1,2),
#'                             callback = callbackFunction)
#'                             
#' # Check that the breakpoints were added
#' breakpoint(myScenario)
#' 
#' # Delete breakpoints
#' myScenario <- deleteBreakpoint(myScenario)
#' 
#' # Check that breakpoints were deleted
#' breakpoint(myScenario)
#' }
#' @export
setGeneric("deleteBreakpoint", function(x, transformerName = NULL, breakpointType = NULL) standardGeneric("deleteBreakpoint"))

#' @rdname deleteBreakpoint
setMethod("deleteBreakpoint", signature(x = "Scenario"), function(x, transformerName, breakpointType) {
  if (length(x@breakpoints) == 0) {
    return(x)
  }

  none <- (is.null(transformerName) && is.null(breakpointType))
  both <- (!is.null(transformerName) && !is.null(breakpointType))
  tname <- transformerName
  bname <- NULL

  if (!is.null(breakpointType)) {
    bname <- getBPNameLongForm(breakpointType)
    if (is.null(bname)) {
      stop("breakpointType not recognized: ", breakpointType)
    }
  }

  if (none) {
    x@breakpoints <- list()
  } else if (both) {
    x@breakpoints <- Filter(function(i) all(i@transformerName != tname || i@breakpointName != bname), x@breakpoints)
  } else if (!is.null(bname)) {
    x@breakpoints <- Filter(function(i) all(i@breakpointName != bname), x@breakpoints)
  } else {
    x@breakpoints <- Filter(function(i) all(i@transformerName != tname), x@breakpoints)
  }

  return(x)
})

#' Breakpoints for a Scenario
#'
#' Lists the breakpoints for a Scenario.
#'
#' @param x \code{\link{Scenario}} object
#' 
#' @return 
#' None
#' 
#' @examples 
#' \dontrun{
#' # Create callback function
#' callbackFunction <- function(x, iteration, timestep) {
#'   print(paste0("Breakpoint hit: ", scenarioId(x)))
#' }
#' 
#' # Install helloworldSpatial package
#' addPackage("helloworldSpatial")
#' 
#' # Set SsimLibrary name
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set Session and SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession,
#'                          package = "helloworldSpatial")
#' myScenario <- scenario(myLibrary, "My Scenario")
#' 
#' # Add breakpoints before the 1st and 2nd iterations
#' myScenario <- addBreakpoint(x= myScenario,
#'                             transformerName= "helloworldSpatial_Primary",
#'                             breakpointType = "bi", 
#'                             arguments = c(1,2),
#'                             callback = callbackFunction)
#'                             
#' # Check that the breakpoints were added
#' breakpoint(myScenario)
#' 
#' # Delete breakpoints
#' myScenario <- deleteBreakpoint(myScenario)
#' 
#' # Check that breakpoints were deleted
#' breakpoint(myScenario)
#' }
#' 
#' @export
setGeneric("breakpoint", function(x) standardGeneric("breakpoint"))

#' @rdname breakpoint
setMethod("breakpoint", signature(x = "Scenario"), function(x) {
  len <- length(x@breakpoints)

  if (len > 0) {
    for (i in 1:len) {
      bp <- x@breakpoints[[i]]

      cat(paste0("Transfomer name: ", bp@transformerName, "\n"))
      cat(paste0("Breakpoint name: ", bp@breakpointName, "\n"))
      cat(paste0("Arguments:       ", bp@arguments, "\n"))

      if (i < len) {
        cat("\n")
      }
    }
  }
})
