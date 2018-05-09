# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
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
      ModuleDirectory = Sys.getenv("SSIM_MODULE_DIRECTORY", unset = NA),      
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
      AfterTimestep = Sys.getenv("SSIM_STOCHASTIC_TIME_AFTER_TIMESTEP", unset = NA), stringsAsFactors=FALSE))
}

SSIM_CreateFolder <- function(scenario, parentFolder, datasheetName){
  
  if (is.na(parentFolder)){
    stop("This function requires the SyncroSim environment.")
  }
  
  sidpart = paste0("Scenario-", scenario@scenarioId)
  
  p = gsub("\\", "/", parentFolder, fixed=T)
  f = file.path(p, sidpart, datasheetName, fsep = .Platform$file.sep)
  
  if (!dir.exists(f)){
    dir.create(f, recursive=T)    
  }

  return(f)
}

#' SyncroSim DataSheet Input Folder
#'
#' Retrieves a SyncroSim DataSheet Input Folder
#'
#' @return a folder name for the specified data sheet
#' @export
#' @rdname ssimInputFolder
ssimInputFolder <- function(scenario, datasheetName) {
  return(SSIM_CreateFolder(scenario, ssimEnvironment()$InputDirectory, datasheetName))
}

#' SyncroSim DataSheet Output Folder
#'
#' Retrieves a SyncroSim DataSheet Output Folder
#'
#' @return a folder name for the specified data sheet
#' @export
#' @rdname ssimOutputFolder
ssimOutputFolder <- function(scenario, datasheetName) {
  return(SSIM_CreateFolder(scenario, ssimEnvironment()$OutputDirectory, datasheetName))
}



