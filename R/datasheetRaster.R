# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieve spatial data from a SyncroSim Datasheet
#'
#' This function retrieves spatial columns from one or more SyncroSim 
#' \code{\link{Scenario}} Datasheets.
#'
#' @param ssimObject SsimLibrary/Project/Scenario object or list of Scenario objects. If 
#'     SsimLibrary/Project, then \code{scenario} argument is required
#' @param datasheet character string. The name of the Datasheet containing the 
#'     raster data
#' @param column character string. The name of the column in the datasheet containing 
#'     the file names for raster data. If \code{NULL} (default) then use the first 
#'     column that contains raster file names
#' @param scenario character string, integer, or vector of these. The Scenarios to 
#'     include. Required if SsimObject is an SsimLibrary/Project, ignored if 
#'     SsimObject is a list of Scenarios (optional)
#' @param iteration integer, character string, or vector of integer/character strings. 
#'     Iteration(s) to include. If \code{NULL} (default) then all iterations are 
#'     included. If no Iteration column is in the Datasheet, then ignored
#' @param timestep integer, character string, or vector of integer/character string. 
#'     Timestep(s) to include. If \code{NULL} (default) then all timesteps are 
#'     included.  If no Timestep column is in the Datasheet, then ignored
#' @param filterColumn character string. The column to filter a Datasheet by. 
#'     (e.g. "TransitionGroupID"). Note that to use the filterColumn argument, 
#'     you must also specify a filterValue. Default is \code{NULL}
#' @param filterValue character string or integer. The value of the filterColumn
#'     to filter the Datasheet by. To use the filterValue argument, you must 
#'     also specify a filterColumn. Default is \code{NULL}
#' @param subset logical expression indicating Datasheet rows to return. 
#'     e.g. expression(grepl("Ts0001", Filename, fixed=T)). See subset() for 
#'     details (optional)
#' @param forceElements logical. If \code{TRUE} then returns a single raster as a RasterStack; 
#'     otherwise returns a single raster as a RasterLayer directly. Default is 
#'     \code{FALSE}
#' @param pathOnly logical. If \code{TRUE} then returns a list of filepaths to the raster
#'     files on disk. Default is \code{FALSE}
#' 
#' @return 
#' A SpatRast object or List. See terra package documentation for details.
#' 
#' @details 
#' The names of the returned raster stack contain metadata.
#' For Datasheets without Filename this is: 
#' 
#' \code{paste0(<datasheet name>,".Scn",<scenario id>,".",<tif name>)}.
#' 
#' For Datasheets containing Filename this is: 
#' 
#' \code{paste0(<datasheet name>,".Scn",<scenario id>,".It",<iteration>,".Ts",<timestep>)}.
#' 
#' @examples
#' \dontrun{
#' # Install the helloworldSpatial package from the server
#' addPackage("helloworldSpatial")
#' 
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib_datasheetRaster")
#' 
#' # Set up a SyncroSim Session
#' mySession <- session()
#' 
#' # Use the example template library from helloworldSpatial
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession,
#'                          package = "helloworldSpatial",
#'                          template = "example-library",
#'                          overwrite=TRUE)
#' 
#' # Set up Project and Scenario
#' myProject <- rsyncrosim::project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Run Scenario to generate results
#' resultScenario <- run(myScenario)
#' 
#' # Extract specific Datasheet rasters by iteration and timestep
#' resultRaster <- datasheetRaster(ssimObject = resultScenario,
#'                   datasheet = "IntermediateDatasheet",
#'                   column = "OutputRasterFile",
#'                   iteration = 3,
#'                   timestep = 2
#' )
#' 
#' # Extract specific Datasheet rasters using pattern matching
#' resultDatasheet <- datasheet(resultScenario, name = "IntermediateDatasheet")
#' colnames(resultDatasheet)
#' outputRasterPaths <- resultDatasheet$OutputRasterFile
#' resultRaster <- datasheetRaster(ssimObject = resultScenario, 
#'                   datasheet = "IntermediateDatasheet",
#'                   column = "OutputRasterFile",
#'                   subset = expression(grepl("ts20", 
#'                                              outputRasterPaths,
#'                                              fixed = TRUE))
#' )
#' 
#' # Return the raster Datasheets as a raster stack
#' resultRaster <- datasheetRaster(ssimObject = resultScenario, 
#'                  datasheet = "IntermediateDatasheet",
#'                  column = "OutputRasterFile",
#'                  forceElements = TRUE
#'                  )
#'                  
#' # Filter for only rasters that fit specific criteria
#' # Load the ST-Sim spatial example library
#' addPackage("stsim")
#' 
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib_stsim_datasheet")
#' 
#' # Set the SyncroSim Session
#' mySession <- session()
#' 
#' # Create a new SsimLibrary with the example template from ST-Sim
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession, 
#'                          package = "stsim",
#'                          template = "spatial-example")
#'                          
#' myScenario <- scenario(myLibrary, scenario = 16)
#' 
#' # Run Scenario to generate results
#' resultScenario <- run(myScenario)
#' 
#' 
#' resultRaster <- datasheetRaster(resultScenario,
#'                  datasheet = "stsim_OutputSpatialState",
#'                  timestep = 5,
#'                  iteration = 5,
#'                  filterColumn = "TransitionTypeID",
#'                  filterValue = "Fire")
#' }
#' 
#' @export
setGeneric("datasheetRaster", function(ssimObject, datasheet, column = NULL, scenario = NULL, iteration = NULL, timestep = NULL, filterColumn = NULL, filterValue = NULL, subset = NULL, forceElements = FALSE, pathOnly = FALSE) standardGeneric("datasheetRaster"))

#' @rdname datasheetRaster
setMethod("datasheetRaster", signature(ssimObject = "character"), function(ssimObject, datasheet, column, scenario, iteration, timestep, filterColumn, filterValue, subset, forceElements, pathOnly) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname datasheetRaster
setMethod("datasheetRaster", signature(ssimObject = "list"), function(ssimObject, datasheet, column, scenario, iteration, timestep, filterColumn, filterValue, subset, forceElements, pathOnly) {
  if (class(ssimObject[[1]]) != "Scenario") {
    stop("Expecting an SsimLibrary/Project/Scenario or list of Scenario objects.")
  }
  if (!is.null(scenario)) {
    warning("scenario argument is ignored when ssimObject is a list of Scenarios")
    scenario <- NULL
  }
  started <- FALSE
  for (i in 1:length(ssimObject)) {
    cScn <- ssimObject[[i]]
    cOut <- datasheetRaster(cScn, datasheet = datasheet, column = column, scenario = scenario, iteration = iteration, timestep = timestep, filterColumn = filterColumn, filterValue = filterValue, subset = subset, forceElements = forceElements, pathOnly = pathOnly)
    if (!((class(cOut) == "list") && (length(cOut) == 0))) {
      names(cOut) <- paste0("scn", .scenarioId(cScn), ".", names(cOut))
      if (!started) {
        out <- cOut
      } else {
        out <- raster::stack(out, cOut)
      }
      started <- TRUE
    }
  }
  
  if ((length(names(out)) == 1) & !forceElements) {
    out <- out[[1]]
  }
  return(out)
})

#' @rdname datasheetRaster
setMethod("datasheetRaster", signature(ssimObject = "SsimObject"), function(ssimObject, datasheet, column, scenario, iteration, timestep, filterColumn, filterValue, subset, forceElements, pathOnly) {
  if (is.null(scenario)) {
    stop("If ssimObject is an SimLibrary or Project, one or more scenarios must be specified using the scenario argument.")
  }
  scnSet <- .scenario(ssimObject)
  missingScns <- scenario
  if (is.character(scenario)) {
    missingScns <- setdiff(scenario, scnSet$Name)
  }
  if (is.numeric(scenario)) {
    missingScns <- setdiff(scenario, scnSet$ScenarioID)
  }
  if (length(missingScns) > 0) {
    stop("Scenarios not found in ssimObject: ", paste(missingScns, collapse = ","))
  }
  
  scnList <- .scenario(ssimObject, scenario = scenario)
  scenario <- NULL
  
  return(datasheetRaster(scnList, datasheet, column, scenario, iteration, timestep, filterColumn, filterValue, subset, forceElements))
})

#' @rdname datasheetRaster
setMethod("datasheetRaster", signature(ssimObject = "Scenario"), function(ssimObject, datasheet, column, scenario, iteration, timestep, filterColumn, filterValue, subset, forceElements, pathOnly) {
  rat <- NULL
  if (is.null(subset)) {
    getFactors <- FALSE
  } else {
    getFactors <- TRUE
  }
  Timestep <- NULL
  Iteration <- NULL
  layerName <- NULL
  freq <- NULL
  if (!is.null(scenario)) {
    warning("scenario argument is ignored when ssimObject is a scenario.")
  }
  
  if (!grepl("_", datasheet, fixed = )) {
    l = ssimLibrary(.filepath(ssimObject), summary=T)
    p = l$value[l$property == "Package Name:"]
    datasheet <- paste0(p, "_", datasheet)
  }
  
  if (grepl("STSim_", datasheet, fixed = TRUE)) {
    warning("An STSim_ prefix for a datasheet name is no longer required.")
    datasheet <- paste0("stsim_", gsub("STSim_", "", datasheet, fixed = TRUE))
  }
  
  x <- ssimObject
  cSheets <- .datasheets(x)
  if (!is.element(datasheet, cSheets$name)) {
    cSheets <- .datasheets(x, core = TRUE)
  }
  
  # TO DO: make sure datasheet is spatial after opening
  cMeta <- .datasheet(x, name = datasheet, optional = TRUE, filterColumn = filterColumn, filterValue = filterValue, lookupsAsFactors = getFactors)
  
  if (nrow(cMeta) == 0) {
    cMeta <- .datasheet(x, name = datasheet, optional = TRUE, filterColumn = filterColumn, filterValue = filterValue, lookupsAsFactors = getFactors)
  }
  args <- list(list = NULL, columns = NULL, allprops = NULL, sheet = datasheet, csv = NULL, lib = .filepath(x))
  
  tt <- command(args, session = .session(x))
  cPropsAll <- .dataframeFromSSim(tt)
  cPropsAll$isRaster <- grepl("isRaster^True", cPropsAll$properties, fixed = TRUE)
  # get a valid raster column
  if (is.null(column)) {
    column <- cPropsAll$name[cPropsAll$isRaster][1]
    if (is.na(column)) {
      stop("No raster columns found in datasheet ", datasheet)
    }
  } else {
    if (!is.element(column, cPropsAll$name)) {
      stop("Column ", column, " not found in datasheet ", datasheet)
    }
  }
  cProps <- subset(cPropsAll, name == column)
  if (!cProps$isRaster) {
    stop(column, " is not a raster column.")
  }
  
  tryCount <- 0
  while (tryCount <= 1) {
    warningMsg <- ""
    if (!is.null(timestep) & is.element("Timestep", names(cMeta))) {
      timestep <- as.numeric(timestep)
      missSteps <- setdiff(timestep, cMeta$Timestep)
      if (length(missSteps) > 0) {
        warningMsg <- paste0("Selected timesteps not available: ", paste(missSteps, collapse = ","))
      }
      cMeta <- subset(cMeta, is.element(Timestep, timestep))
    }
    
    if (!is.null(iteration) & is.element("Iteration", names(cMeta))) {
      iteration <- as.numeric(iteration)
      missSteps <- setdiff(iteration, cMeta$Iteration)
      if (length(missSteps) > 0) {
        warningMsg <- paste0(warningMsg, " Selected iterations not available: ", paste(missSteps, collapse = ","))
      }
      cMeta <- subset(cMeta, is.element(Iteration, iteration))
    }
    
    if ((nchar(warningMsg) > 0) | (nrow(cMeta) == 0)) {
      if (tryCount == 1) {
        if (nrow(cMeta) == 0) {
          stop("No data available.")
        } else {
          warning(warningMsg)
        }
      } else {
        cMeta <- .datasheet(x, name = datasheet, optional = TRUE, filterColumn = filterColumn, lookupsAsFactors = getFactors)
      }
    }
    tryCount <- tryCount + 1
  }
  
  if (grepl("bandColumn", cProps$properties, fixed = TRUE)) {
    propSplit <- strsplit(cProps$properties, "!", fixed = TRUE)[[1]]
    bandBit <- propSplit[grepl("bandColumn", propSplit)]
    bandColumn <- strsplit(bandBit, "^", fixed = TRUE)[[1]][2]
    cMeta$bandColumn <- cMeta[[bandColumn]]
  } else {
    cMeta$bandColumn <- NA
  }
  cMeta$rasterColumn <- cMeta[[column]]
  # subset rows using subset argument
  if (!is.null(subset)) {
    cMeta <- .subset(cMeta, eval(subset))
  }
  
  # Now cMeta contains bandColumn, rasterColumn, and only rows to be exported
  cMeta$outName <- gsub(".tif", "", basename(cMeta$rasterColumn), fixed = TRUE)

  if ((length(setdiff(NA, unique(cMeta$Band))) > 0) & length(intersect(names(cMeta), c("Timestep", "Iteration"))) == 0) {
    cMeta$outName <- paste0(cMeta$outName, ".b", cMeta$bandColumn)
  }

  cMeta$outName <- gsub("ts.ts", "ts", cMeta$outName, fixed = TRUE)

  if (nrow(cMeta) == 0) {
    warning("No raster data to return.")
    return(list())
  }
  
  nFiles <- unique(cMeta$rasterColumn)
  
  # Case of unique file for many iterations/timestep
  if ((length(nFiles) == 1) & (nrow(cMeta) > 1) & !is.null(cMeta$Band[1])) {
    if (!file.exists(nFiles)) {
      addPath <- paste0(.filepath(x), ".output/Scenario-", .scenarioId(x), "/", datasheet, "/", nFiles)
      if (!file.exists(addPath)) {
        stop("Output not found: ", nFiles)
      }
      cMeta$rasterColumn <- addPath
    }
    cStack <- terra::rast(cMeta$rasterColumn[1])

    cMeta$layerName <- paste0(strsplit(nFiles, ".", fixed = TRUE)[[1]][1], ".", cMeta$Band)

    keepLayers <- intersect(names(cStack), cMeta$layerName)
    cStack <- terra::subset(x = cStack, subset = keepLayers)
    missing <- setdiff(cMeta$layerName, names(cStack))
    if (length(missing) > 0) {
      warning("Some layers not found: ", paste(cMeta$outName[is.element(cMeta$layerName, missing)]))
    }
    
    cMeta <- subset(cMeta, is.element(layerName, names(cStack)))
    
    for (i in 1:nrow(cMeta)) {
      cRow <- cMeta[i, ]
      cName <- cRow$layerName
      cStack[[cName]]@title <- cRow$outName
      names(cStack)[names(cStack) == cRow$layerName] <- cRow$outName
    }
  } else {
    # Every other case
    for (i in seq(length.out = nrow(cMeta))) {
      cRow <- cMeta[i, ]
      if (is.na(cRow$rasterColumn)) {
        next
      }
      if (!file.exists(cRow$rasterColumn)) {
        # TO DO: path should already be there...
        addPath <- paste0(.filepath(x), ".output/Scenario-", .scenarioId(x), "/", datasheet, "/", cRow$rasterColumn)
        if (!file.exists(addPath)) {
          stop("Output not found: ", cRow$rasterColumn)
        }
        cRow$rasterColumn <- addPath
      }
      if (is.na(cRow$bandColumn)) {
        cRaster <- terra::rast(cRow$rasterColumn)
      } else {
        cRaster <- terra::rast(cRow$rasterColumn, lyrs = cRow$bandColumn)
      }
      
      cRaster@title <- cRow$outName
      if (i == 1) {
        cStack <- terra::rast(cRaster)
        names(cStack) <- c(cRow$outName)
      } else {
        oldNames <- names(cStack)
        cStack <- terra::add(x = cStack, value = cRaster)
        names(cStack) <- c(oldNames, cRow$outName)
      }
    }
  }
  
  # If return pathOnly = TRUE, just return filepaths to rasters
  if (pathOnly) {
    return(cMeta$rasterColumn)
  }
  
  # ensure layers are sorted by name
  sortNames <- sort(names(cStack))
  cStack <- terra::subset(x = cStack, subset = sortNames)
  
  if ((length(names(cStack)) == 1) & !forceElements) {
    cStack <- cStack[[1]]
  }
  return(cStack)
})
