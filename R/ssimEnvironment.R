# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License

#' SyncroSim Environment
#'
#' This function is part of a set of functions designed to facilitate the
#' development of R-based Syncrosim Packages. \code{ssimEnvironment} retrieves
#' specific environment variables.
#'
#' @return 
#' Returns a single-row data.frame of SyncroSim specific environment variables.
#' 
#' @examples 
#' \dontrun{
#' # Get the whole set of variables
#' e <- ssimEnvironment()
#' 
#' # Get the path to transfer directory, for instance
#' transferdir <- e$TransferDirectory
#' }
#' 
#' @export
ssimEnvironment <- function() {
  return(data.frame(
    PackageDirectory = Sys.getenv(tolower("SSIM_PACKAGE_DIRECTORY"), unset = NA),
    ProgramDirectory = Sys.getenv(tolower("SSIM_PROGRAM_DIRECTORY"), unset = NA),
    LibraryFilePath = Sys.getenv(tolower("SSIM_LIBRARY_FILEPATH"), unset = NA),
    ProjectId = as.integer(Sys.getenv(tolower("SSIM_PROJECT_ID"), unset = -1)),
    ScenarioId = as.integer(Sys.getenv(tolower("SSIM_SCENARIO_ID"), unset = -1)),
    DataDirectory = Sys.getenv(tolower("SSIM_DATA_DIRECTORY"), unset = NA),
    TempDirectory = Sys.getenv(tolower("SSIM_TEMP_DIRECTORY"), unset = NA),
    TransferDirectory = Sys.getenv(tolower("SSIM_TRANSFER_DIRECTORY"), unset = NA),
    BeforeIteration = as.integer(Sys.getenv(tolower("SSIM_STOCHASTIC_TIME_BEFORE_ITERATION"), unset = -1)),
    AfterIteration = as.integer(Sys.getenv(tolower("SSIM_STOCHASTIC_TIME_AFTER_ITERATION"), unset = -1)),
    BeforeTimestep = as.integer(Sys.getenv(tolower("SSIM_STOCHASTIC_TIME_BEFORE_TIMESTEP"), unset = -1)),
    AfterTimestep = as.integer(Sys.getenv(tolower("SSIM_STOCHASTIC_TIME_AFTER_TIMESTEP"), unset = -1)),
    IsChildProcess = Sys.getenv(tolower("SSIM_IS_CHILD_PROCESS"), unset = "false"), stringsAsFactors = FALSE
  ))
}

envValidateEnvironment <- function() {
  e <- ssimEnvironment()

  if (is.na(e$ProgramDirectory)) {
    stop("This function requires a SyncroSim environment.")
  }
}

envCreateScenarioFolder <- function(scenario, parentFolder, datasheetName) {
  sidpart <- paste0("Scenario-", scenario@scenarioId)

  p <- gsub("\\", "/", parentFolder, fixed = TRUE)
  f <- file.path(p, sidpart, datasheetName, fsep = .Platform$file.sep)

  if (!dir.exists(f)) {
    dir.create(f, recursive = TRUE)
  }

  return(f)
}

envCreateTempFolder <- function(folderName) {
  t <- ssimEnvironment()$TempDirectory
  p <- gsub("\\", "/", t, fixed = TRUE)

  f <- file.path(p, folderName, fsep = .Platform$file.sep)

  if (!dir.exists(f)) {
    dir.create(f, recursive = TRUE)
  }

  return(f)
}

#' SyncroSim Data Folder
#'
#' This function is part of a set of functions designed to facilitate the
#' development of R-based Syncrosim Packages. This function creates and returns 
#' a SyncroSim Data Folder.
#'
#' @param scenario \code{\link{Scenario}} object. A SyncroSim result Scenario
#' @param datasheetName character. The datasheet name
#' 
#' @return 
#' Returns a data folder name for the specified datasheet.
#' 
#' @examples 
#' \dontrun{
#' dataFolder <- runtimeDataFolder()
#' }
#' 
#' @export
runtimeDataFolder <- function(scenario, datasheetName) {
  envValidateEnvironment()
  return(envCreateScenarioFolder(scenario, ssimEnvironment()$DataDirectory, datasheetName))
}

#' SyncroSim Temporary Folder
#'
#' This function is part of a set of functions designed to facilitate the
#' development of R-based Syncrosim Packages. This function creates and returns 
#' a SyncroSim Temporary Folder.
#'
#' @param folderName character. The folder name
#' 
#' @return 
#' Returns a temporary folder name.
#' 
#' @examples 
#' \dontrun{
#' tempFolder <- runtimeTempFolder()
#' }
#' 
#' @export
runtimeTempFolder <- function(folderName) {
  envValidateEnvironment()
  return(envCreateTempFolder(folderName))
}

#' Sets the progress bar in the SyncroSim User Interface
#' 
#' This function is designed to facilitate the development of R-based Syncrosim 
#' Packages, such as beginning, stepping, ending, and reporting the progress 
#' for a SyncroSim simulation.
#' 
#' @param type character. Update to apply to progress bar. Options include
#' "begin", "end", "step", "report", and "message" (Default is "step")
#' @param iteration integer. The current iteration. Only used if 
#' \code{type = "report"}
#' @param timestep integer. The current timestep. Only used if 
#' \code{type = "report"}
#' @param totalSteps integer. The total number of steps in the simulation. Only
#' used if \code{type = "begin"}
#' @param message character. An arbitrary messsage to be printed to the status
#' bar. Only used if \code{type = "message"}.
#' 
#' @return 
#' No returned value, used for side effects
#' 
#' @examples 
#' \dontrun{
#' # Begin the progress bar for a simulation
#' progressBar(type = "begin", totalSteps = numIterations * numTimesteps)
#' 
#' # Increase the progress bar by one step for a simulation
#' progressBar(type = "step")
#' 
#' # Report progress for a simulation
#' progressBar(type = "report", iteration = iter, timestep = ts)
#' 
#' # Report arbitrary progress message 
#' progressBar(type = "message", message = msg)
#' 
#' # End the progress bar for a simulation
#' progressBar(type = "end")
#' }
#' 
#' @export
progressBar <- function(type = "step", iteration = NULL, timestep = NULL, totalSteps = NULL, message) {
  
  # Check Program Directory
  envValidateEnvironment()
  
  # Begin progress bar tracking
  if (type == "begin") {
    if (is.numeric(totalSteps)) {
      cat(sprintf("ssim-task-start=%d\r\n", totalSteps))
      flush.console()
    } else {
      stop("totalSteps argument must be an integer")
    }
    
  # End progress bar tracking
  } else if (type == "end") {
    cat("ssim-task-end=True\r\n")
    flush.console()
    
  # Step progress bar
  } else if (type == "step") {
    cat("ssim-task-step=1\r\n")
    flush.console()
  
  # Report iteration and timestep in UI  
  } else if (type == "report") {
    if (is.numeric(iteration) & is.numeric(timestep)) {
      cat(sprintf("ssim-task-status=Simulating -> Iteration is %d - Timestep is %d\r\n", iteration, timestep))
      flush.console()
    } else {
      stop("iteration and timestep arguments must be integers")
    }
    
  # Print arbitrary message to UI
  } else if (type == "message") {
    if (!missing(message)) {
      cat(paste0("ssim-task-status=", as.character(message), "\r\n"))
      flush.console()
    } else {
      stop("please provide a message")
    }
  
  # Throw error if type not specified correctly  
  } else {
    stop("Invalid type argument")
  }
  
}

#' Function to write to the SyncroSim run log
#' 
#' This function is designed to facilitate the development of R-based Syncrosim 
#' Packages by allowing developers to send messages to the run log.
#' 
#' @param ... One or more objects which can be coerced to character
#' which are pasted together using `sep`.
#' @param sep character. Used to separate terms. Not NA_character_
#' @param type character. Type of message to add to run log. One of "status",
#' "info", or "warning".
#' 
#' @return 
#' No returned value, used for side effects
#' 
#' @examples 
#' \dontrun{
#' # Write a message to run log
#' updateRunLog(msg)
#' 
#' # Construct and write a message to run log
#' updateRunLog(msg, additionalMsg, sep = " ")
#' }
#'
#' @export
updateRunLog <- function(..., sep = "", type = "status") {
  if(length(list(...)) == 0)
    stop("Please provide a message to write to the run log.")
  
  # Concatenate objects to form the message
  fullMessage <- paste(..., sep = sep, collapse = "")
  
  # Split the message at line breaks
  splitMessage <- strsplit(fullMessage, "\n")[[1]]
  
  # Standardize surrounding empty lines
  if(splitMessage[1] == "") splitMessage <- splitMessage[-1]
  if(splitMessage[length(splitMessage)] != "") splitMessage <- c(splitMessage, "")
  
  # Annotate messages
  annotatedMessage <- paste0("ssim-task-log=", splitMessage, "\r\n")
  
  if(!type %in% c("status", "info", "warning"))
    stop("Please select a valid run log message type.")
  
  if(type == "info")
    annotatedMessage[1] <- sub("ssim-task-log", "ssim-task-info", annotatedMessage[1])
  if(type == "warning")
    annotatedMessage[1] <- sub("ssim-task-log", "ssim-task-warning", annotatedMessage[1])
  
  # Send to SyncroSim
  for(m in annotatedMessage) {
    cat(m)
    flush.console()
  }
}
