# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Save datasheet
#'
#' Saves a datasheet to a \code{\linkS4class{SsimLibrary}}, \code{\linkS4class{Project}}, or 
#' \code{\linkS4class{Scenario}}.
#' 
#' @param ssimObject \code{\linkS4class{SsimLibrary}}, \code{\linkS4class{Project}}, or 
#'     \code{\linkS4class{Scenario}} object
#' @param data data.frame. The datasheet to load
#' @param name character. The name of the datasheet to be saved
#' @param append logical. If \code{TRUE}, the incoming data will be appended to the 
#'     datasheet if possible.  Default is \code{TRUE} for Project/SsimLibrary-scope datasheets, 
#'     and \code{FALSE} for Scenario-scope Datasheets. See 'details' for more information 
#'     about this argument
#' @param force logical. If datasheet scope is Project/SsimLibrary, and \code{append=FALSE}, 
#'     datasheet will be deleted before loading the new data. This can also delete 
#'     other definitions and results, so if \code{force=FALSE} (default) user will be 
#'     prompted for approval 
#' 
#' @details
#' About the 'append' argument:
#' 
#' \itemize{
#'   \item A Datasheet is a VALIDATION SOURCE if its data can be used to validate 
#'   column values in a different Datasheet.
#'   \item The \code{append} argument will be ignored if the Datasheet is a validation 
#'   source and has a Project scope.  In this case the data will be MERGED.
#' }
#' 
#' @return 
#' Invisibly returns a vector or list of logical values for each 
#' input: \code{TRUE} upon success (i.e.successful save) and \code{FALSE} upon failure.
#' 
#' @examples 
#' \dontrun{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set the SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession,
#'                          packages = "helloworldSpatial")
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Get all Datasheet info
#' myDatasheets <- datasheet(myScenario)
#' 
#' # Get a specific Datasheet
#' myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl")
#' 
#' # Modify Datasheet
#' myDatasheet$MaximumTimestep <- 10
#' 
#' # Save Datasheet
#' saveDatasheet(ssimObject = myScenario, 
#'               data = myDatasheet, 
#'               name = "helloworldSpatial_RunControl")
#' }
#' 
#' @export
setGeneric("saveDatasheet", 
           function(ssimObject, data, name = NULL, append = NULL, 
                    force = FALSE) standardGeneric("saveDatasheet"))

#' @rdname saveDatasheet
setMethod("saveDatasheet", 
          signature(ssimObject = "character"), 
          function(ssimObject, data, name, append, force) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname saveDatasheet
setMethod("saveDatasheet", signature(ssimObject = "SsimObject"), 
          function(ssimObject, data, name, append, force) {
            
  # Check if data is in correct format
  if (!is.data.frame(data)) {
    stop("data must be in R data.frame format.")
  }          
  
  # Check if we are currently running in a SyncroSim environment
  e <- ssimEnvironment()
  if (!is.na(e$TransferDirectory)) {
    import <- FALSE
    path <- e$TransferDirectory
  } else {
    import <- TRUE
    path <- .tempfilepath(ssimObject)
  }
  
  # Set the append argument default
  if (is.null(append)) {
    if (is(ssimObject, "Scenario")) {
      append <- FALSE
    } else {
      append <- TRUE
    }
  }
  
  # If force not set to TRUE, then prompt user for project/library ds removal
  if (!force & !append & (is(ssimObject, "Project")) | is(ssimObject, "Library")) {
    message("Overwriting", name, 
            " may result in data in linked scenario-scoped datasheets being removed as well.",
            "\nAre you sure you want to overwrite ", name, "?")
    answer <- readline(prompt = "(y/n):")
    
    if (answer == "n") {
      append <- FALSE
    }
  }

  if (!grepl("_", name, fixed = T)) {
    stop("The datasheet name requires a package prefix (e.g., 'stsim_RunControl')")
  }

  # convert factors to strings
  for (kk in seq(length.out = ncol(data))) {
    if (is.factor(data[[kk]])) {
      data[[kk]] <- as.character(data[[kk]])
    }
  }

  # Check whether datasheet provided actually exists.
  sheetNames <- .datasheets(ssimObject, core = TRUE)
  scope <- sheetNames$scope[sheetNames$name == name]
  if (length(scope) == 0) {
    stop(paste0(name, " not found in available datasheets"))
  }
  
  # Subset data by available columns
  tt <- command(c("list", "columns", "csv", "allprops", 
                  paste0("lib=", .filepath(ssimObject)), paste0("sheet=", name)), 
                .session(ssimObject))
  sheetInfo <- .dataframeFromSSim(tt)
  
  # Remove the Library/Project/Scenario ID from the datasheet
  if (scope == "library"){
    colsToKeep <- sheetInfo$name[!sheetInfo$name %in% c("LibraryId")]
  } else if (scope == "project"){
    colsToKeep <- sheetInfo$name[!sheetInfo$name %in% c("ProjectId")]
  } else if (scope == "scenario"){
    colsToKeep <- sheetInfo$name[!sheetInfo$name %in% c("ScenarioId")]
  }
  
  # Remove the datasheet ID from the datasheet if it exists
  dsInfo <- sheetNames[sheetNames$name == name,]
  dsName <- gsub(paste0(dsInfo$package, "_"), '', dsInfo$name)
  dsNameID <- paste0(dsName, "Id")
  colsToKeep <- colsToKeep[!colsToKeep %in% c(dsNameID)]
  
  # Determine if any columns in incoming data do not exist in datasheet
  unknownCols <- !colnames(data) %in% colsToKeep
  if (any(unknownCols)) {
    unknownCols <- colnames(data)[unknownCols]
    stop(paste0("The following column does not exist in the datasheet: ", 
                unknownCols))
  }
  
  # Add invisible columns to data to ensure they are not overwritten
  # if datasheet is possibly a validation source
  # Remove rows that already exist in the dataframe
  if ((scope == "project") & (append != FALSE)) {
    originalData <- .datasheet(ssimObject, name, returnInvisible = T)
    data <- merge(data, originalData, all.x = T)
  }
  
  # Subset data by the valid columns
  colsToKeep <- colnames(data)[colnames(data) %in% colsToKeep]
  data <- data[colsToKeep]

  # Convert logicals to "Yes" or "No"
  for (j in seq(length.out = ncol(data))) {
    if (is.logical(data[[j]])) {
      inCol <- data[[j]]
      data[[j]][inCol] <- "Yes"
      data[[j]][!inCol] <- "No"
    }
  }

  # Convert dataframe to characters and NAs to empty strings
  data[] <- lapply(data, as.character)
  data[is.na(data)] <- ""
  
  # Save temporary datasheet CSV file
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  
  if (append) {
    tempFile <- paste0(path, "/", "SSIM_APPEND-", name, ".csv")
  } else {
    tempFile <- paste0(path, "/", "SSIM_OVERWRITE-", name, ".csv")
  }
  
  if (nchar(tempFile) >= 260){
    msg <- paste("path to temporary files generated at runtime is longer", 
                 " than 260 characters. This may result in a connection ",
                 "error if long paths are not enabled on Windows machines.")
    updateRunLog(msg, type = "warning")
  }
  
  write.csv(data, file = tempFile, row.names = FALSE, quote = TRUE)

  # If not running in SyncroSim env then import changes to ssim database
  out <- TRUE
  
  if (import) {
    
    args <- list(import = NULL, lib = .filepath(ssimObject), 
                 sheet = name, file = tempFile)
    tt <- "saved"

    if (nrow(data) > 0) {
      
      if (scope == "project") {
        args[["pid"]] <- .projectId(ssimObject)
        if (append) args <- c(args, list(append = NULL))
      }
      
      if (scope == "scenario") {
        args[["sid"]] <- .scenarioId(ssimObject)
        if (append) args <- c(args, list(append = NULL))
      }
      
      tt <- command(args, .session(ssimObject))
    } else {
      if (append == FALSE) {
        .delete(ssimObject, datasheet = name, force = force)
      }
    }
    
    if (tt[[1]] == "saved") {
      unlink(tempFile)
      message(paste0("Datasheet <",name, "> saved"))
    } else {
      out <- FALSE
      message(tt[[1]])
    }
  }
  
  # Clean up temporary files
  unlink(.tempfilepath(ssimObject), recursive = TRUE)
  
  return(invisible(out))
})
