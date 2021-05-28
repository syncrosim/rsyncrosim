# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License

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
#' \donttest{
#' # Get the whole set of variables
#' e <- ssimEnvironment()
#' 
#' # Get the path to transfer direcyory, for instance
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
#' This function is part of a set of functions designed to facilitate the
#' development of R-based Syncrosim Packages. This function creates and returns 
#' a SyncroSim Datasheet Input Folder.
#'
#' @param scenario Scenario.  A SyncroSim result \code{\link{Scenario}}.
#' @param datasheetName Character.  The input datasheet name.
#' 
#' @return 
#' Returns a folder name for the specified datasheet.
#' 
#' @examples 
#' \donttest{
#' inputFolder <- envInputFolder
#' }
#' 
#' @export
envInputFolder <- function(scenario, datasheetName) {
  envValidateEnvironment()
  return(envCreateScenarioFolder(scenario, ssimEnvironment()$InputDirectory, datasheetName))
}

#' SyncroSim DataSheet Output Folder
#'
#' This function is part of a set of functions designed to facilitate the
#' development of R-based Syncrosim Packages. Thus function creates and returns 
#' a SyncroSim DataSheet Output Folder.
#'
#' @param scenario Scenario. A SyncroSim result \code{\link{Scenario}}.
#' @param datasheetName Character. The output datasheet name.
#' 
#' @return 
#' Returns a folder name for the specified datasheet.
#' 
#' @examples 
#' \donttest{
#' outputFolder <- envOutputFolder()
#' }
#' 
#' @export
envOutputFolder <- function(scenario, datasheetName) {
  envValidateEnvironment()
  return(envCreateScenarioFolder(scenario, ssimEnvironment()$OutputDirectory, datasheetName))
}

#' SyncroSim Temporary Folder
#'
#' This function is part of a set of functions designed to facilitate the
#' development of R-based Syncrosim Packages. This function creates and returns 
#' a SyncroSim Temporary Folder.
#'
#' @param folderName Character. The folder name.
#' 
#' @return 
#' Returns a temporary folder name.
#' 
#' @examples 
#' \donttest{
#' tempFolder <- envTempFolder()
#' }
#' 
#' @export
envTempFolder <- function(folderName) {
  envValidateEnvironment()
  return(envCreateTempFolder(folderName))
}

#' Reports progress for a SyncroSim simulation
#'
#' This function is part of a set of functions designed to facilitate the
#' development of R-based Syncrosim Packages. This function reports progress 
#' for a SyncroSim simulation.
#'
#' @param iteration integer. The current iteration.
#' @param timestep integer. The current timestep.
#' 
#' @return
#' No returned value, used for side effects.
#' 
#' @examples 
#' \donttest{
#' envReportProgress()
#' }
#' 
#' @export
envReportProgress <- function(iteration, timestep) {
  envValidateEnvironment()
  cat(sprintf("ssim-task-status=Simulating -> Iteration is %d - Timestep is %d\r\n", iteration, timestep))
  flush.console()
}

#' Begins a SyncroSim simulation
#'
#' This function is part of a set of functions designed to facilitate the
#' development of R-based SyncroSim Packages. Specifically, this function begins
#' the reporting of a SyncroSim simulation.
#'
#' @param totalSteps integer.  The total number of steps in the simulation.
#' 
#' @return
#' No returned value, used for side effects.
#' 
#' @examples 
#' \donttest{
#' n_iter <- 50
#' n_timesteps <- 10
#' n_steps <- n_iter * n_timesteps
#' envBeginSimulation(n_steps)
#' }
#' 
#' @export
envBeginSimulation <- function(totalSteps) {
  envValidateEnvironment()
  cat(sprintf("ssim-task-start=%d\r\n", totalSteps))
  flush.console()
}

#' Steps a SyncroSim simulation
#'
#' This function is part of a set of functions designed to facilitate the
#' development of R-based Syncrosim Packages. This function steps a SyncroSim 
#' simulation, and is to be called between \link{\code{envBeginSimulation}} and 
#' \link{\code{envEndSimulation}}.
#' 
#' @return 
#' No returned value, used for side effects.
#' 
#' @examples
#' \donttest{
#' envStepSimulation()
#' }
#'
#' @export
envStepSimulation <- function() {
  envValidateEnvironment()
  cat("ssim-task-step=1\r\n")
  flush.console()
}

#' Ends a SyncroSim simulation
#'
#' This function is part of a set of functions designed to facilitate the
#' development of R-based Syncrosim Packages. This function ends a SyncroSim 
#' simulation.
#' 
#' @return
#' No returned value, used for side effects.
#' 
#' @examples 
#' \donttest{
#' envEndSimulation()
#' }
#' 
#' @export
envEndSimulation <- function() {
  envValidateEnvironment()
  cat("ssim-task-end=True\r\n")
  flush.console()
}
