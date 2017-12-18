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