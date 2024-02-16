# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Save Datasheet(s)
#'
#' Saves Datasheets to a \code{\link{SsimLibrary}}, \code{\link{Project}}, or 
#' \code{\link{Scenario}}.
#' 
#' @param ssimObject \code{\link{SsimLibrary}}, \code{\link{Project}}, or 
#'     \code{\link{Scenario}} object
#' @param data data.frame, named vector, or list of these. One or more 
#'     Datasheets to load
#' @param name character or vector of these. The name(s) of the Datasheet(s) to 
#'     be saved. If a vector of names is provided, then a list must be provided 
#'     for the \code{data} argument. Names provided here will override those provided 
#'     with \code{data} argument's list
#' @param fileData named list or SpatRaster object. Names are file names (without paths), 
#'     corresponding to entries in \code{data} The elements are objects containing the 
#'     data associated with each name. Currently supports terra SpatRaster objects as elements, 
#'     (support for Raster objects is deprecated)
#' @param append logical. If \code{TRUE}, the incoming data will be appended to the 
#'     Datasheet if possible.  Default is \code{TRUE} for Project/SsimLibrary-scope Datasheets, 
#'     and \code{FALSE} for Scenario-scope Datasheets. See 'details' for more information 
#'     about this argument
#' @param forceElements logical. If \code{FALSE} (default) a single return message will 
#'     be returned as a character string. Otherwise it will be returned in a list
#' @param force logical. If Datasheet scope is Project/SsimLibrary, and \code{append=FALSE}, 
#'     Datasheet will be deleted before loading the new data. This can also delete 
#'     other definitions and results, so if \code{force=FALSE} (default) user will be 
#'     prompted for approval 
#' @param breakpoint logical. Set to \code{TRUE} when modifying Datasheets in a 
#'     breakpoint function. Default is \code{FALSE}
#' @param import logical. Set to \code{TRUE} to import the data after saving. 
#'     Default is \code{FALSE}
#' @param path character.  output path (optional)
#' 
#' @details
#' SsimObject/Project/Scenario should identify a single SsimObject.
#'
#' If \code{fileData != NULL}, each element of \code{names(fileData)} should correspond uniquely 
#' to at most one entry in data. If a name is not found in data the element will 
#' be ignored with a warning. If \code{names(fileData)} are full filepaths, rsyncrosim 
#' will write each object to the corresponding path for subsequent loading by SyncroSim. 
#' Note this is generally more time-consuming because the files must be written twice.
#' If \code{names(fileData)} are not filepaths (faster, recommended), rsyncrosim will 
#' write each element directly to the appropriate SyncroSim input/output folders.
#' rsyncrosim will write each element of fileData directly to the appropriate 
#' SyncroSim input/output folders. If \code{fileData != NULL}, data should be a data.frame, 
#' vector, or list of length 1, not a list of length >1.
#' 
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
#' # Install helloworldSpatial package
#' addPackage("helloworldSpatial")
#' 
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib_saveDatasheet")
#' 
#' # Set the SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession, 
#'                          package = "helloworldSpatial",
#'                          template = "example-library",
#'                          forceUpdate = TRUE)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Get all Datasheet info
#' myDatasheets <- datasheet(myScenario)
#' 
#' # Get a specific Datasheet
#' myDatasheet <- datasheet(myScenario, name = "RunControl")
#' 
#' # Modify Datasheet
#' myDatasheet$MaximumTimestep <- 10
#' 
#' # Save Datasheet
#' saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl")
#'           
#' # Import data after saving
#' saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl",
#'               import = TRUE)
#'         
#' # Save the new Datasheet to a specified output path
#' saveDatasheet(ssimObject = myScenario, data = myDatasheet, name = "RunControl",
#'               path = tempdir())
#'               
#' # Save a raster stack using fileData
#' # Create a raster stack - add as many raster files as you want here
#' map1 <- datasheetRaster(myScenario, datasheet = "InputDatasheet",
#'                         column = "InterceptRasterFile")
#' inRasters <- terra::rast(map1)
#' 
#' # Change the name of the rasters in the input Datasheets to match the stack
#' inSheet <- datasheet(myScenario, name="InputDatasheet")
#' inSheet[1,"InterceptRasterFile"] <- names(inRasters)[1]
#' 
#' # Save the raster stack to the input Datasheet
#' saveDatasheet(myScenario, data=inSheet, name="InputDatasheet", 
#'               fileData=inRasters)
#' }
#' 
#' @export
setGeneric("saveDatasheet", function(ssimObject, data, name = NULL, fileData = NULL, append = NULL, forceElements = FALSE, force = FALSE, breakpoint = FALSE, import = TRUE, path = NULL) standardGeneric("saveDatasheet"))

#' @rdname saveDatasheet
setMethod("saveDatasheet", signature(ssimObject = "character"), function(ssimObject, data, name, fileData, append, forceElements, force, breakpoint, import, path) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname saveDatasheet
setMethod("saveDatasheet", signature(ssimObject = "SsimObject"), function(ssimObject, data, name, fileData, append, forceElements, force, breakpoint, import, path) {

  isFile <- NULL
  x <- ssimObject
  if (is.null(append)) {
    if (is(x, "Scenario")) {
      append <- FALSE
    } else {
      append <- TRUE
    }
  }

  args <- list()
  sheetNames <- .datasheets(x)

  if (is.null(path)) {
    e <- ssimEnvironment()
    if (!is.na(e$TransferDirectory)) {
      import <- FALSE
      path <- e$TransferDirectory
    }
  }

  # Note - cannot handle a list of named vectors, only a list of dataframes.
  if ((!is(data, "list")) | (!is(data[[1]], "data.frame"))) {
    if (is.null(name)) {
      stop("Need a datasheet name.")
    }
    if (length(name) > 1) {
      stop("If a vector of names is provided, then data must be a list.")
    }

    if (!grepl("_", name, fixed = )) {
      l = ssimLibrary(.filepath(ssimObject), summary=T)
      p = l$value[l$property == "Package Name:"]
      name <- paste0(p, "_", name)
    }

    if (grepl("STSim_", name, fixed = TRUE)) {
      warning("An STSim_ prefix for a datasheet name is no longer required.")
      name <- paste0("stsim_", gsub("STSim_", "", name, fixed = TRUE))
    }

    hdat <- data
    data <- list()
    data[[name]] <- hdat
  } else {
    if (!is.null(name)) {
      if (length(name) != length(data)) {
        stop("Please provide a name for each element of data.")
      }
      warning("Name argument will override names(data).")
      names(data) <- name
    } else {
      name <- names(data)
    }
  }

  if (!is.null(fileData) && (length(data) > 1)) {
    stop("If fileData != NULL, data should be a dataframe, vector, or list of length 1.")
  }
  out <- list()
  for (i in seq(length.out = length(data))) {
    cName <- names(data)[i]
    cDat <- data[[cName]]

    # handle cases when cDat is not a data.frame
    if (!is(cDat, "data.frame")) {
      cIn <- cDat
      if (length(cIn) == 0) {
        stop("No data found for ", cName)
      }
      if (!is.null(names(cDat))) {
        cDat <- data.frame(a = cIn[[1]])
        names(cDat) <- names(cIn)[1]
        for (j in seq(length.out = (length(cIn) - 1))) {
          cDat[[names(cIn)[j + 1]]] <- cIn[[j + 1]]
        }
      } else {
        stop() #handle this case
      }
    }

    # convert factors to strings
    for (kk in seq(length.out = ncol(cDat))) {
      if (is.factor(cDat[[kk]])) {
        cDat[[kk]] <- as.character(cDat[[kk]])
      }
    }

    # note deletions must happen before files are written.
    scope <- sheetNames$scope[sheetNames$name == cName]
    if (length(scope) == 0) {
      sheetNames <- datasheets(x, core = TRUE)
      scope <- sheetNames$scope[sheetNames$name == cName]
      if (length(scope) == 0) {
        stop("Name not found in datasheetNames")
      }
    }
    
    # Subset data by available columns
    tt <- command(c("list", "columns", "csv", "allprops", 
                    paste0("lib=", .filepath(x)), paste0("sheet=", name)), 
                  .session(x))
    sheetInfo <- .dataframeFromSSim(tt)
    
    # Remove the Library/Project/Scenario ID from the datasheet
    if (scope == "library"){
      colsToKeep <- sheetInfo$name[!sheetInfo$name %in% c("LibraryID")]
    } else if (scope == "project"){
      colsToKeep <- sheetInfo$name[!sheetInfo$name %in% c("ProjectID")]
    } else if (scope == "scenario"){
      colsToKeep <- sheetInfo$name[!sheetInfo$name %in% c("ScenarioID")]
    }
    
    # Remove the datasheet ID from the datasheet if it exists
    dsInfo <- sheetNames[sheetNames$name == cName,]
    dsName <- gsub(paste0(dsInfo$package, "_"), '', dsInfo$name)
    dsNameID <- paste0(dsName, "ID")
    colsToKeep <- colsToKeep[!colsToKeep %in% c(dsNameID)]
    
    # Subset data by the valid columns
    cDat <- cDat[colsToKeep]

    # if no fileData found and datasheet contains files, find the files
    if (is.null(fileData)) {

      if (sum(grepl("isExternalFile^True", sheetInfo$properties, fixed = TRUE)) > 0) {
        sheetInfo$isFile <- grepl("isRaster^True", sheetInfo$properties, fixed = TRUE)
      } else {
        sheetInfo$isFile <- grepl("isExternalFile^Yes", sheetInfo$properties, fixed = TRUE)
        # NOTE: this should be isExternalFile - but the flag is set to true even for non-files
      }
      
      sheetInfo <- subset(sheetInfo, isFile)

      sheetInfo <- subset(sheetInfo, is.element(name, names(cDat)))
    }

    # Write items to appropriate locations
    if (!is.null(fileData)) {
      itemNames <- names(fileData)
      if (is.null(itemNames) || is.na(itemNames) || (length(itemNames) == 0)) {
        stop("names(fileData) must be defined, and each element must correspond uniquely to an entry in data")
      }
    
      sheetInfo <- subset(datasheet(x, summary = TRUE, optional = TRUE), name == cName)
      fileDir <- .filepath(x)
      if (sheetInfo$isOutput) {
        fileDir <- paste0(fileDir, ".output")
      } else {
        fileDir <- paste0(fileDir, ".input")
      }
      fileDir <- paste0(fileDir, "/Scenario-", .scenarioId(x), "/", cName)

      dir.create(fileDir, showWarnings = FALSE, recursive = TRUE)

      for (j in seq(length.out = length(itemNames))) {
        cFName <- itemNames[j]
        cItem <- fileData[[cFName]]
        if (!is(cItem, "RasterLayer") & !is(cItem, "SpatRaster")){
          stop("rsyncrosim currently only supports terra SpatRasters and Raster layers as elements of fileData.")
        }
        if (is(cItem, "RasterLayer")) {
          warning("Raster Layer support in rsyncrosim is now deprecated and will be removed in a future version.")
        }
        # check for cName in datasheet

        findName <- cDat == cFName
        findName[is.na(findName)] <- FALSE
        sumFind <- sum(findName == TRUE, na.rm = TRUE)

        if (sumFind > 1) {
          stop("Each element of names(fileData) must correspond to at most one entry in data. ", sumFind, " entries of ", cName, " were found in data.")
        }
        if (sumFind == 0) {
          warning(cName, " not found in data. This element will be ignored.")
          next
        }

        if (identical(basename(cFName), cFName)) {
          cOutName <- paste0(fileDir, "/", cFName)
        } else {
          cOutName <- cFName
        }
        if (!grepl(".tif", cOutName, fixed = TRUE)) {
          cDat[findName] <- paste0(cDat[findName], ".tif")

          cOutName <- paste0(cOutName, ".tif")
        }

        if (is(cItem, "SpatRaster")){
          terra::writeRaster(cItem, cOutName, overwrite = TRUE)
        }
        if (is(cItem, "RasterLayer")) {
          raster::writeRaster(cItem, cOutName, format = "GTiff", overwrite = TRUE)
        }
      }
    }
    for (j in seq(length.out = ncol(cDat))) {
      if (is.factor(cDat[[j]])) {
        cDat[[j]] <- as.character(cDat[[j]])
      }
      if (is.logical(cDat[[j]])) {
        inCol <- cDat[[j]]
        cDat[[j]][inCol] <- "Yes"
        cDat[[j]][!inCol] <- "No"
      }
    }

    cDat[is.na(cDat)] <- ""
    pathBit <- NULL

    if (is.null(path)) {
      if (breakpoint) {
        pathBit <- paste0(.filepath(x), ".temp/Data")
      } else {
        pathBit <- .tempfilepath(x)
      }
    } else {
      pathBit <- path
    }

    dir.create(pathBit, showWarnings = FALSE, recursive = TRUE)

    if (append) {
      tempFile <- paste0(pathBit, "/", "SSIM_APPEND-", cName, ".csv")
    } else {
      tempFile <- paste0(pathBit, "/", "SSIM_OVERWRITE-", cName, ".csv")
    }

    write.csv(cDat, file = tempFile, row.names = FALSE, quote = TRUE)
    if (breakpoint) {
      out[[cName]] <- "Saved"
      next
    }

    if (import) {
      args <- list(import = NULL, lib = .filepath(x), sheet = cName, file = tempFile)
      tt <- "saved"
      if (nrow(cDat) > 0) {
        if (scope == "project") {
          args[["pid"]] <- .projectId(x)
          if (append) args <- c(args, list(append = NULL))
        }
        if (scope == "scenario") {
          args[["sid"]] <- .scenarioId(x)
          if (append) args <- c(args, list(append = NULL))
        }
        tt <- command(args, .session(x))
      }
      if (tt[[1]] == "saved") {
        unlink(tempFile)
      }
      out[[cName]] <- tt
    } else {
      out[[cName]] <- "Saved"
    }
    
    if (out[[cName]][1] == "saved"){
      message(paste0("Datasheet <",cName, "> saved"))
      out[[cName]] <- TRUE
    } else {
      message(out[[cName]])
      out[[cName]] <- FALSE
    }
    
  }
  
  if (!forceElements && (length(out) == 1)) {
    out <- out[[1]]
  }
  unlink(.tempfilepath(x), recursive = TRUE)
  return(invisible(out))
})
