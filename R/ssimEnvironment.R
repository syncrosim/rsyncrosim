# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License

#' SyncroSim Environment.
#'
#' Retrieves SyncroSim specific environment variables. 
#'
#' @return a data.frame of SyncroSim specific environment variables.
#' @export
#' @rdname ssimEnvironment
ssimEnvironment <- function() {

    return(data.frame(
      PackageDirectory = Sys.getenv("SSIM_PACKAGE_DIRECTORY", unset = NA),
      ProgramDirectory = Sys.getenv("SSIM_PROGRAM_DIRECTORY", unset = NA),
      LibraryFilePath = Sys.getenv("SSIM_LIBRARY_FILEPATH", unset = NA),
      ProjectId = Sys.getenv("SSIM_PROJECT_ID", unset = NA),
      ScenarioId = Sys.getenv("SSIM_SCENARIO_ID", unset = NA),
      InputDirectory = Sys.getenv("SSIM_INPUT_DIRECTORY", unset = NA),
      OutputDirectory = Sys.getenv("SSIM_OUTPUT_DIRECTORY", unset = NA),
      TempDirectory = Sys.getenv("SSIM_TEMP_DIRECTORY", unset = NA),
      TransferDirectory = Sys.getenv("SSIM_TRANSFER_DIRECTORY", unset = NA),
      BeforeIteration = Sys.getenv("SSIM_STOCHASTIC_TIME_BEFORE_ITERATION", unset = NA),
      AfterIteration = Sys.getenv("SSIM_STOCHASTIC_TIME_AFTER_ITERATION", unset = NA),
      BeforeTimestep = Sys.getenv("SSIM_STOCHASTIC_TIME_BEFORE_TIMESTEP", unset = NA),
      AfterTimestep = Sys.getenv("SSIM_STOCHASTIC_TIME_AFTER_TIMESTEP", unset = NA), stringsAsFactors = FALSE))
}

envValidateEnvironment <- function() {

    e = ssimEnvironment()

    if (is.na(e$ProgramDirectory)) {
        stop("This function requires a SyncroSim environment.")
    }
}

envCreateScenarioFolder <- function(scenario, parentFolder, datasheetName) {

    sidpart = paste0("Scenario-", scenario@scenarioId)

    p = gsub("\\", "/", parentFolder, fixed = T)
    f = file.path(p, sidpart, datasheetName, fsep = .Platform$file.sep)

    if (!dir.exists(f)) {
        dir.create(f, recursive = T)
    }

    return(f)
}

envCreateTempFolder <- function(folderName) {

    t = ssimEnvironment()$TempDirectory
    p = gsub("\\", "/", t, fixed = T)
    
    f = file.path(p, folderName, fsep = .Platform$file.sep)

    if (!dir.exists(f)) {
        dir.create(f, recursive = T)
    }

    return(f)
}

#' SyncroSim DataSheet Input Folder
#'
#' Creates and returns a SyncroSim DataSheet Input Folder
#'
#' @param scenario Scenario.  A SyncroSim result scenario.
#' @param datasheetName character.  The input datasheet name.
#' @return a folder name for the specified data sheet
#' @export
#' @rdname ssimEnvironment-input
envInputFolder <- function(scenario, datasheetName) {
    envValidateEnvironment()
    return(envCreateScenarioFolder(scenario, ssimEnvironment()$InputDirectory, datasheetName))
}

#' SyncroSim DataSheet Output Folder
#'
#' Creates and returns a SyncroSim DataSheet Output Folder
#'
#' @param scenario Scenario.  A SyncroSim result scenario.
#' @param datasheetName character.  The output datasheet name.
#' @return a folder name for the specified data sheet
#' @export
#' @rdname ssimEnvironment-output
envOutputFolder <- function(scenario, datasheetName) {
    envValidateEnvironment()
    return(envCreateScenarioFolder(scenario, ssimEnvironment()$OutputDirectory, datasheetName))
}

#' SyncroSim Temporary Folder
#'
#' Creates and returns a SyncroSim Temporary Folder
#'
#' @param folderName character.  The folder name
#' @return a temporary folder name
#' @export
#' @rdname ssimEnvironment-temp
envTempFolder <- function(folderName) {
    envValidateEnvironment()
    return(envCreateTempFolder(folderName))
}

#' Reports progress for a SyncroSim simulation
#'
#' Reports progress for a SyncroSim simulation
#'
#' @param iteration integer.  The current iteration.
#' @param timestep integer.  The current timestep.
#' @export
#' @rdname ssimEnvironment-progress
envReportProgress <- function(iteration, timestep) {

    envValidateEnvironment()
    cat(sprintf("ssim-task-status=Simulating -> Iteration is %d - Timestep is %d\r\n", iteration, timestep))
    flush.console()
}

#' Begins a SyncroSim simulation
#'
#' Begins a SyncroSim simulation
#'
#' @param totalSteps integer.  The total number of steps in the simulation.
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
#' @export
#' @rdname ssimEnvironment-progress
envStepSimulation <- function() {
  
  envValidateEnvironment()
  cat("ssim-task-step=1\r\n")
  flush.console()
}

#' Ends a SyncroSim simulation
#'
#' Ends a SyncroSim simulation
#'
#' @export
#' @rdname ssimEnvironment-progress
envEndSimulation <- function() {
  
  envValidateEnvironment()
  cat("ssim-task-end=True\r\n")
  flush.console()
}

