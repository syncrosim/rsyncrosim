# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License

#' SyncroSim Environment.
#'
#' Retrieves SyncroSim specific environment variables.
#'
#' @return 
#' A data.frame of SyncroSim specific environment variables.
#' 
#' @export
#' @rdname ssimEnvironment
ssimEnvironment <- function() {
  return(data.frame(
    PackageDirectory = Sys.getenv(tolower("SSIM_PACKAGE_DIRECTORY"), unset = NA),
    ProgramDirectory = Sys.getenv(tolower("SSIM_PROGRAM_DIRECTORY"), unset = NA),
    LibraryFilePath = Sys.getenv(tolower("SSIM_LIBRARY_FILEPATH"), unset = NA),
    ProjectId = as.integer(Sys.getenv(tolower("SSIM_PROJECT_ID"), unset = -1)),
    ScenarioId = as.integer(Sys.getenv(tolower("SSIM_SCENARIO_ID"), unset = -1)),
    InputDirectory = Sys.getenv(tolower("SSIM_INPUT_DIRECTORY"), unset = NA),
    OutputDirectory = Sys.getenv(tolower("SSIM_OUTPUT_DIRECTORY"), unset = NA),
    TempDirectory = Sys.getenv(tolower("SSIM_TEMP_DIRECTORY"), unset = NA),
    TransferDirectory = Sys.getenv(tolower("SSIM_TRANSFER_DIRECTORY"), unset = NA),
    BeforeIteration = as.integer(Sys.getenv(tolower("SSIM_STOCHASTIC_TIME_BEFORE_ITERATION"), unset = -1)),
    AfterIteration = as.integer(Sys.getenv(tolower("SSIM_STOCHASTIC_TIME_AFTER_ITERATION"), unset = -1)),
    BeforeTimestep = as.integer(Sys.getenv(tolower("SSIM_STOCHASTIC_TIME_BEFORE_TIMESTEP"), unset = -1)),
    AfterTimestep = as.integer(Sys.getenv(tolower("SSIM_STOCHASTIC_TIME_AFTER_TIMESTEP"), unset = -1)), stringsAsFactors = FALSE
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

#' SyncroSim DataSheet Input Folder
#'
#' Creates and returns a SyncroSim DataSheet Input Folder.
#'
#' @param scenario Scenario.  A SyncroSim result scenario.
#' @param datasheetName character.  The input datasheet name.
#' 
#' @return 
#' a folder name for the specified data sheet
#' 
#' @export
#' @rdname ssimEnvironment-input
envInputFolder <- function(scenario, datasheetName) {
  envValidateEnvironment()
  return(envCreateScenarioFolder(scenario, ssimEnvironment()$InputDirectory, datasheetName))
}

#' SyncroSim DataSheet Output Folder
#'
#' Creates and returns a SyncroSim DataSheet Output Folder.
#'
#' @param scenario Scenario.  A SyncroSim result scenario.
#' @param datasheetName character.  The output datasheet name.
#' 
#' @return 
#' a folder name for the specified data sheet
#' 
#' @export
#' @rdname ssimEnvironment-output
envOutputFolder <- function(scenario, datasheetName) {
  envValidateEnvironment()
  return(envCreateScenarioFolder(scenario, ssimEnvironment()$OutputDirectory, datasheetName))
}

#' SyncroSim Temporary Folder
#'
#' Creates and returns a SyncroSim Temporary Folder.
#'
#' @param folderName character.  The folder name
#' 
#' @return 
#' A temporary folder name
#' 
#' @export
#' @rdname ssimEnvironment-temp
envTempFolder <- function(folderName) {
  envValidateEnvironment()
  return(envCreateTempFolder(folderName))
}

#' Reports progress for a SyncroSim simulation
#'
#' Reports progress for a SyncroSim simulation.
#'
#' @param iteration integer.  The current iteration.
#' @param timestep integer.  The current timestep.
#' 
#' @export
#' @rdname ssimEnvironment-progress
envReportProgress <- function(iteration, timestep) {
  envValidateEnvironment()
  cat(sprintf("ssim-task-status=Simulating -> Iteration is %d - Timestep is %d\r\n", iteration, timestep))
  flush.console()
}

#' Begins a SyncroSim simulation
#'
#' Begins a SyncroSim simulation.
#'
#' @param totalSteps integer.  The total number of steps in the simulation.
#' 
#' @export
#' @rdname ssimEnvironment-progress
envBeginSimulation <- function(totalSteps) {
  envValidateEnvironment()
  cat(sprintf("ssim-task-start=%d\r\n", totalSteps))
  flush.console()
}

#' Steps a SyncroSim simulation
#'
#' Steps a SyncroSim simulation
#' 
#' @return 
#' No returned value, used for side effects.
#'
#' @export
#' @rdname ssimEnvironment-progress
envStepSimulation <- function() {
  envValidateEnvironment()
  cat("ssim-task-step=1\r\n")
  flush.console()
}

#' Ends a SyncroSim simulation
#'
#' Ends a SyncroSim simulation.
#' 
#' @return
#' No returned value, used for side effects.
#'
#' @export
#' @rdname ssimEnvironment-progress
envEndSimulation <- function() {
  envValidateEnvironment()
  cat("ssim-task-end=True\r\n")
  flush.console()
}
