# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' SyncroSim console command
#'
#' This function issues a command to the SyncroSim console, and is mostly
#' used internally by other functions. 
#'
#' @details
#' Example args, and the resulting character string passed to the SyncroSim console:
#' \itemize{
#'    \item Character string e.g. "--create --help": "--create --help"
#'    \item Named list or named vector e.g. list(name1=NULL,name2=value2): "--name1 --name2=value2"
#'    \item Unnamed list or unnamed vector e.g. c("create","help"): "--create --help"
#' }
#' 
#' @param args character string, named list, named vector, unnamed list, or unnamed 
#'     vector. Arguments for the SyncroSim console. See 'details' for more 
#'     information about this argument
#' @param session \code{\link{Session}} object. If \code{NULL}(default), the default
#'  session will be used
#' @param program character. The name of the target SyncroSim executable. 
#'     Options include "SyncroSim.Console.exe" (default), "SyncroSim.Server.exe", 
#'     "SyncroSim.PackageManager.exe" and "SyncroSim.Multiband.exe"
#' @param wait logical. If \code{TRUE}(default) R will wait for the command to finish 
#'     before proceeding. Note that silent(session) is ignored if \code{wait=FALSE}
#' @param progName character. Internal argument for setting path to SyncroSim 
#'     installation folder.
#' 
#' @return 
#' Character string: output from the SyncroSim program.
#' 
#' @examples
#' \dontrun{
#' # Install "stsim" if not already installed
#' installPackage("stsim")
#' 
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib.ssim")
#' 
#' # Specify the command line arguments for creating a new stsim SsimLibrary
#' args <- list(create = NULL, library = NULL, 
#'         name = myLibraryName, 
#'         package = "stsim")
#'         
#' # Use a default session to create a new SsimLibrary in the current working directory
#' output <- command(args, session = session(printCmd = TRUE))
#' output
#'
#' # Provide arguments to the command line using an unnamed vector
#' command(c("create", "help"))
#' 
#' # Provide arguments to the command line using a character string
#' command("--create --help")
#' 
#' # Provide arguments to the command line using a named list
#' command(list(create = NULL, help = NULL))
#' 
#' # Call on a different program to find all installed packages
#' command(list(installed = NULL), program = "SyncroSim.PackageManager.exe")
#' }
#' @export
command <- function(args, session = NULL, program = "SyncroSim.Console.exe", wait = TRUE, progName = NULL) {

  # if a SyncroSim session is not provided, make one
  if (is.null(session) && is.null(progName)) {
    session <- .session()
  }
  if (is(session, "character") && (is(session, SyncroSimNotFound(warn = FALSE)))) {
    return(SyncroSimNotFound())
  }

  if (is(args,"list") & is.null(names(args))) {
    args <- as.character(args)
  }

  if (is(args, "list")) {
    # catch invalid library paths - note this only works for args with names
    if (is.element("lib", names(args))) {
      if (!file.exists(args$lib)) {
        stop(paste0("Library does not exist: ", args$lib))
      }
    }
    sysArgs <- c()
    for (i in seq(length.out = length(args))) {
      cArg <- paste0("--", names(args)[i])
      sysArgs <- c(sysArgs, cArg)
      if (is.null(args[[i]])) {
        next
      }
      if (is.na(args[[i]])) {
        next
      }
      if (args[[i]] == "") {
        next
      }
      a <- args[[i]]
      if (is.logical(a)) {
        if (a == TRUE) {
          a <- "True"
        } else {
          a <- "False"
        }
      }
      sysArgs[i] <- paste0(sysArgs[i], '="', a, '"')
    }
  } else {
    args <- gsub(" --", "---", args, fixed = TRUE)
    fixPaths <- grepl(" ", args)
    args[fixPaths] <- gsub("=", '="', args[fixPaths], fixed = TRUE)
    args[fixPaths] <- paste0(args[fixPaths], '"')
    args <- gsub("---", " --", args, fixed = TRUE)

    if (sum(grepl("--", args, fixed = TRUE)) == 0) {
      args <- paste0("--", args)
    }
    sysArgs <- args
  }
  
  if (!is.null(session)){
    if (printCmd(session)) {
      outCmd <- gsub("\"", "", paste(sysArgs, collapse = " "), fixed = TRUE)
      print(outCmd)
    }
    progName <- paste0('\"', .filepath(session), "/", program, '\"')
  } else {
    progName <- paste0('\"', progName, "/", program, '\"')
  }

  tempCmd <- NULL
  
  if (.Platform$OS.type == "windows") {
    tempCmd <- paste(c(progName, sysArgs), collapse = " ")
  } else {
    tempCmd <- paste(c("mono", progName, sysArgs), collapse = " ")
  }

  if (wait) {
    out <- suppressWarnings(system(tempCmd, intern = TRUE))
  } else {
    out <- suppressWarnings(system(tempCmd, wait = FALSE))
    Sys.sleep(5)
  }

  if (identical(out, character(0))) {
    out <- "saved"
  }
  return(out)
}
