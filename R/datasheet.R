# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Retrieve a SyncroSim Datasheet
#'
#' This function retrieves a SyncroSim Datasheet, either by calling the SyncroSim
#' console, or by directly querying the \code{\link{SsimLibrary}} database.
#'
#' @details
#' If \code{summary=TRUE} or \code{summary=NULL} and \code{name=NULL} a data.frame describing the 
#' Datasheets is returned. If \code{optional=TRUE}, columns include: \code{scope}, \code{packages}, 
#' \code{name}, \code{displayName}, \code{isSingle}, \code{data}. data only displayed for 
#' a SyncroSim \code{\link{Scenario}}. \code{dataInherited} and \code{dataSource} columns 
#' added if a Scenario has dependencies. If \code{optional=FALSE}, columns include: 
#' \code{scope}, \code{name}, \code{displayName}. All other arguments are ignored.
#'
#' Otherwise, for each element in name a Datasheet is returned as follows:
#' \itemize{
#'   \item If \code{lookupsAsFactors=TRUE} (default): Each column is given the correct 
#'          data type, and dependencies returned as factors with allowed values (levels). 
#'          A warning is issued if the lookup has not yet been set.
#'   \item If \code{empty=TRUE}: Each column is given the correct data type. Fast (1 less 
#'          console command).
#'   \item If \code{empty=FALSE} and \code{lookupsAsFactors=FALSE}: Column types are not checked, 
#'          and the optional argument is ignored. Fast (1 less console command).
#'   \item If SsimObject is a list of \code{\link{Scenario}} or \code{\link{Project}} 
#'          objects (output from \code{\link{run}}, \code{\link{Scenario}} or 
#'          \code{\link{Project}}): Adds ScenarioId/ProjectId column if appropriate.
#'   \item If Scenario/Project is a vector: Adds ScenarioId/ProjectId column 
#'          as necessary.
#'   \item If requested Datasheet has Scenario scope and contains info from more 
#'          than one Scenario: ScenarioId/ScenarioName/ScenarioParent columns 
#'          identify the Scenario by \code{name}, \code{id}, and \code{parent} (if a result Scenario).
#'   \item If requested Datasheet has Project scope and contains info from more 
#'          than one Project: ProjectId/ProjectName columns identify the Project 
#'          by \code{name} and \code{id}
#' }
#'
#' @param ssimObject \code{\link{SsimLibrary}}, \code{\link{Project}},
#'     or \code{\link{Scenario}} object or list of objects. 
#'     Note that all objects in a list must be of the same type, and belong to 
#'     the same SsimLibrary
#' @param name character or character vector. Sheet name(s). If \code{NULL} (default), 
#'     all datasheets in the ssimObject will be returned. Note that setting 
#'     \code{summary=FALSE} and \code{name=NULL} pulls all Datasheets, which is time 
#'     consuming and not generally recommended
#' @param project numeric or numeric vector. One or more 
#'     \code{\link{Project}} ids
#' @param scenario numeric or numeric vector. One or more 
#'     \code{\link{Scenario}} ids
#' @param summary logical or character. If \code{TRUE} (default) returns a data.frame of sheet names 
#'     and other info including built-in core SyncroSim Datasheets. If \code{FALSE} returns 
#'     data.frame or list of data.frames.
#' @param optional logical. If \code{summary=TRUE} and \code{optional=TRUE} returns 
#'     only \code{scope}, \code{name} and \code{displayName}. If \code{summary=FALSE} and \code{optional=TRUE} returns 
#'     all of the Datasheet's columns, including the optional columns. If 
#'     \code{summary=TRUE}, \code{optional=FALSE} (default), returns only those columns that are mandatory 
#'     and contain data (if \code{empty=FALSE}). Ignored if \code{summary=FALSE}, \code{empty=FALSE}
#'     and \code{lookupsAsFactors=FALSE}
#' @param empty logical. If \code{TRUE} returns empty data.frames for each
#'     Datasheet. Ignored if \code{summary=TRUE} Default is \code{FALSE}
#' @param filterColumn character string. The column to filter a Datasheet by.
#'     (e.g. "TransitionGroupId"). Note that to use the filterColumn argument,
#'     you must also specify the filterValue argument. Default is \code{NULL}
#' @param filterValue character string or integer (scalar or vector). The value
#'     to filter the filterColumn by. To use the filterValue argument, you must
#'     also specify the filterColumn argument. Default is \code{NULL}
#' @param lookupsAsFactors logical. If \code{TRUE} (default) dependencies
#'     returned as factors with allowed values (levels). Set \code{FALSE} to speed 
#'     calculations. Ignored if \code{summary=TRUE}
#' @param sqlStatement list returned by \code{\link{sqlStatement}}. \code{SELECT} and 
#'     \code{GROUP BY} SQL statements passed to SQLite database. Ignored if 
#'     \code{summary=TRUE} (optional)
#' @param includeKey logical. If \code{TRUE} include primary key in table. Default is 
#' \code{FALSE}
#' @param forceElements logical. If \code{FALSE} (default) and name has a single element 
#'     returns a data.frame; otherwise returns a list of data.frames. Ignored if 
#'     \code{summary=TRUE}
#' @param fastQuery logical.  If \code{TRUE}, the request is optimized for 
#'     performance.  Ignored if combined with summary, empty, or 
#'     \code{\link{sqlStatement}} flags. Default is \code{FALSE}
#' @param returnScenarioInfo logical. If \code{TRUE}, returns the Scenario Id,
#'     Scenario Name, Parent Id, and Parent Name columns with the 
#'     Scenario-scoped Datasheet. Does nothing if the Datasheet exists at the
#'     Library or Project level. Default is \code{FALSE}
#' @param returnInvisible logical. If \code{TRUE}, returns columns that are 
#'     invisible in the User Interface (i.e., are only used and populated
#'     internally by SyncroSim or the SyncroSim Package). Default is \code{FALSE}
#' @param rawValues logical. If \code{TRUE}, returns the raw ID values rather
#'     than automatically translating the values to strings. Default is 
#'     \code{FALSE}.
#' @param verbose logical. If set to \code{FALSE}, will not print notes about
#'     datasheet validation. Default is \code{TRUE}.
#' 
#' @return 
#' If \code{summary=TRUE} returns a data.frame of Datasheet names 
#' and other information, otherwise returns a data.frame or list of these.
#' 
#'
#' @examples 
#' \dontrun{
#' # Install helloworldSpatial package from package server
#' installPackage("helloworldSpatial")
#' 
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib_datasheet")
#' 
#' # Set the SyncroSim Session
#' mySession <- session()
#' 
#' # Create a new SsimLibrary with the example template from helloworldSpatial
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession, 
#'                          packages = "helloworldSpatial")
#'                          
#' # Set the Project and Scenario
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Get all Datasheet info for the Scenario
#' myDatasheets <- datasheet(myScenario)
#' 
#' 
#' # Return a list of data.frames (1 for each Datasheet)
#' myDatasheetList <- datasheet(myScenario, summary = FALSE)
#' 
#' # Get a specific Datasheet
#' myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl")
#' 
#' # Include primary key when retrieving a Datasheet
#' myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl", 
#'                          includeKey = TRUE)
#' 
#' # Return all columns, including optional ones
#' myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl", 
#'                          summary = TRUE, optional = TRUE)
#' 
#' # Return Datasheet as an element
#' myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl", 
#'                          forceElements = TRUE)
#' myDatasheet$helloworldSpatial_RunControl
#' 
#' # Get a Datasheet without pre-specified values
#' myDatasheetEmpty <- datasheet(myScenario, 
#'                               name = "helloworldSpatial_RunControl", 
#'                               empty = TRUE)
#' 
#' # If Datasheet is empty, do not return dependencies as factors
#' myDatasheetEmpty <- datasheet(myScenario, 
#'                               name = "helloworldSpatial_RunControl", 
#'                               empty = TRUE,
#'                               lookupsAsFactors = FALSE)
#'                               
#' # Optimize query
#' myDatasheet <- datasheet(myScenario, name = "helloworldSpatial_RunControl", 
#'                          fastQuery = TRUE)
#' 
#' # Get specific SsimLibrary core Datasheet
#' myDatasheet <- datasheet(myLibrary, name = "core_Backup")
#' 
#' # Use an SQL statement to query a Datasheet
#' mySQL <- sqlStatement(
#'   groupBy = c("ScenarioId"),
#'   aggregate = c("MinimumTimestep"),
#'   where = list(MinimumTimestep = c(1))
#' )
#' myAggregatedDatasheet <- datasheet(myScenario, 
#'                                    name = "helloworldSpatial_RunControl",
#'                                    sqlStatement = mySQL)
#' }
#' 
#' @export
#' @import RSQLite
setGeneric("datasheet", function(ssimObject, name = NULL, project = NULL, scenario = NULL, 
                                 summary = NULL, optional = FALSE, empty = FALSE, 
                                 filterColumn = NULL, filterValue = NULL, 
                                 lookupsAsFactors = TRUE, 
                                 sqlStatement = list(select = "SELECT *", groupBy = ""), 
                                 includeKey = FALSE, forceElements = FALSE, 
                                 fastQuery = FALSE, returnScenarioInfo = FALSE,
                                 returnInvisible = FALSE, rawValues = FALSE,
                                 verbose = TRUE) standardGeneric("datasheet"))

# Handles case where ssimObject is list of Scenario or Project objects
#' @rdname datasheet
setMethod("datasheet", 
          signature(ssimObject = "list"), 
          function(ssimObject, name, project, scenario, summary, optional, empty, 
                   filterColumn, filterValue, lookupsAsFactors, sqlStatement, 
                   includeKey, forceElements, fastQuery, returnScenarioInfo,
                   returnInvisible, rawValues, verbose) {
  
  print("=== In datasheet method for list ===")

  cScn <- ssimObject[[1]]
  x <- NULL
  
  if (is(cScn, "Scenario")) {
    x <- getIdsFromListOfObjects(ssimObject, expecting = "Scenario", scenario = scenario, project = project)
    scenario <- x$objs
    project <- NULL
  }
  
  if (is(cScn, "Project")) {
    x <- getIdsFromListOfObjects(ssimObject, expecting = "Project", scenario = scenario, project = project)
    project <- x$objs
    scenario <- NULL
  }
  
  ssimObject <- x$ssimObject
  
  if (is.null(ssimObject)) {
    stop("Expecting ssimObject to be an SsimLibrary/Project/Scenario, or a list of Scenarios/Projects.")
  }
  # Now have scenario/project ids of same type in same library, and ssimObject is library
  
  out <- .datasheet(ssimObject, name = name, project = project, scenario = scenario, 
                    summary = summary, optional = optional, empty = empty, 
                    filterColumn = filterColumn, filterValue = filterValue, 
                    lookupsAsFactors = lookupsAsFactors, sqlStatement = sqlStatement, 
                    includeKey = includeKey, forceElements = forceElements, 
                    fastQuery = fastQuery, returnScenarioInfo = returnScenarioInfo,
                    returnInvisible = returnInvisible, rawValues = rawValues,
                    verbose = verbose)
  
  return(out)
})

#' @rdname datasheet
setMethod("datasheet", 
          signature(ssimObject = "character"), 
          function(ssimObject, name, project, scenario, summary, optional, empty, 
                   filterColumn, filterValue, lookupsAsFactors, sqlStatement, 
                   includeKey, fastQuery, returnScenarioInfo, returnInvisible,
                   rawValues, verbose) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname datasheet
setMethod("datasheet", 
          signature(ssimObject = "SsimObject"), 
          function(ssimObject, name, project, scenario, summary, optional, empty, 
                   filterColumn, filterValue, lookupsAsFactors, sqlStatement, 
                   includeKey, forceElements, fastQuery, returnScenarioInfo,
                   returnInvisible, rawValues, verbose) {
          
  temp <- NULL
  ProjectId <- NULL
  ScenarioId <- NULL
  colOne <- NULL
  ParentId <- NULL
  ParentName <- NULL
  Name <- NULL
  xProjScn <- .getFromXProjScn(ssimObject, project, scenario, returnIds = TRUE, 
                               convertObject = FALSE, complainIfMissing = TRUE)

  print("=== In datasheet method for SsimObject ===")
  print("=== After .getFromXProjScn ===")

  IDColumns <- c("ScenarioId", "ProjectId")
  
  # -------------------------------------------------------------------------
  # NEW: Goal-based scope detection (fixes multi-scenario logic)
  # -------------------------------------------------------------------------

  # xProjScn always contains:
  #   $ssimObject
  #   $project
  #   $scenario
  #   $scenarioSet
  #   $goal  ← THIS is the correct source of truth

  scopeDS <- xProjScn$goal
  x <- NULL
  pid <- NULL
  sid <- NULL

  if (is(xProjScn, "SsimLibrary")) {
      # Library-level call
      x <- xProjScn
      pid <- NULL
      sid <- NULL
  } else {
      # Project/scenario-level call
      x <- xProjScn$ssimObject
      pid <- xProjScn$project
      sid <- xProjScn$scenario

      # If scenarios were provided, ensure project IDs align
      if (!is.null(sid) & is.null(pid)) {
          pid <- subset(xProjScn$scenarioSet, ScenarioId %in% sid)$ProjectId
      }
  }

  cat("DEBUG: Goal-based initialization\n")
  print(list(goal = scopeDS, pid = pid, sid = sid))

  # -------------------------------------------------------------------------
  # Override using scenarioSet (handles lists and multi-scenario inputs)
  # -------------------------------------------------------------------------

  if (!is.null(xProjScn$scenarioSet)) {

      scenarioSet <- xProjScn$scenarioSet

      cat("DEBUG: scenarioSet detected\n")
      print(head(scenarioSet))

      # If user did not explicitly specify scenarios, derive them
      if (is.null(scenario)) {
          sid <- unique(scenarioSet$ScenarioId)
          pid <- unique(scenarioSet$ProjectId)
      }

      # MULTI-SCENARIO → enforce scenario scope
      if (length(unique(scenarioSet$ScenarioId)) > 1) {
          scopeDS <- "scenario"
          returnScenarioInfo <- TRUE
          cat("DEBUG: Multi-scenario override triggered\n")
      }

      # MULTI-PROJECT but not multi-scenario
      if (length(unique(scenarioSet$ProjectId)) > 1 &&
          length(unique(scenarioSet$ScenarioId)) == 1) {
          scopeDS <- "project"
          cat("DEBUG: Multi-project override triggered\n")
      }
  }

  # -------------------------------------------------------------------------
  # Respect explicit scenario / project arguments
  # -------------------------------------------------------------------------

  # Explicit scenario override
  if (!missing(scenario) && !is.null(scenario)) {
      sid <- scenario
      if (length(scenario) > 1) {
          scopeDS <- "scenario"
          returnScenarioInfo <- TRUE
      }
  }

  # Explicit project override
  if (!missing(project) && !is.null(project)) {
      pid <- project
      if (length(project) > 1) {
          scopeDS <- "project"
      }
  }

  cat("DEBUG: Final pid/sid/scope after overrides:\n")
  print(list(scope = scopeDS, pid = pid, sid = sid))
  # -------------------------------------------------------------------------
  
  # now have valid pid/sid vectors and x is library.
  if (!is.null(name)) {
    for (i in seq_along(name)) {
      n <- name[i]
      if (!grepl("_", n, fixed = TRUE)) {
        stop("The datasheet name requires a package prefix (e.g., 'stsim_RunControl')")
      }
    
      name[i] <- n
    }
  }
  
  allNames <- name
  
  if (is.null(summary)) {
    if (is.null(name)) {
      summary <- TRUE
    } else {
      summary <- FALSE
    }
  }
  
  # if summary, don't need to bother with project/scenario ids: sheet info doesn't vary among project/scenarios in a project
  if (summary == TRUE) {
    sumInfo <- .datasheets(x, project[[1]], scenario[[1]], core = TRUE)
    
    if (nrow(sumInfo) == 0) {
      stop("No datasheets available")
    }
    
    sumInfo$order <- seq(1, nrow(sumInfo))
    
    if (is.null(name)) {
      name <- sumInfo$name
      allNames <- name
    }
    
    missingSheets <- setdiff(name, sumInfo$name)
    
    if (length(missingSheets) > 0) {
      sumInfo <- .datasheets(x, project[[1]], scenario[[1]], refresh = TRUE)
      missingSheets <- setdiff(name, sumInfo$name)
      
      if (length(missingSheets) > 0) {
        stop(paste0("Datasheets not found: ", paste(missingSheets, collapse = ",")))
      }
    }
    
    sumInfo <- subset(sumInfo, is.element(name, allNames))
  }
  
  # now assume we have one or more names
  if (is.null(name) & summary == FALSE) {
    sumInfo <- .datasheets(x, project[[1]], scenario[[1]])
    allNames <- sumInfo$name
  } else if (is.null(name) & !summary == FALSE) {
    stop("Something is wrong in datasheet().")
  }
  
  if ((summary == TRUE) & !optional) {
    
    sumInfo <- subset(sumInfo, select = c("scope", "name", "displayName", "order"))
    sumInfo[order(sumInfo$order), ]
    sumInfo$order <- NULL
    sumInfo <- sumInfo[sumInfo$scope == scopeDS,]
    
    return(sumInfo)
  }
  
  # Add data info - only for scenario scope datasheets if sid is defined
  if (summary == TRUE) {
    
    # if no scenario scope sheets, return sumInfo without checking for data
    scnSheetSum <- sum(sumInfo$scope == "scenario")
    
    if (scnSheetSum == 0) {
      
      sumInfo[order(sumInfo$order), ]
      sumInfo$order <- NULL
      sumInfo <- sumInfo[sumInfo$scope == scopeDS,]
      
      return(sumInfo)
    }
    
    for (i in seq(length.out = length(sid))) {
      cSid <- sid[i]
      tt <- command(list(list = NULL, datasources = NULL, lib = .filepath(x), sid = cSid), session = session(x))
      
      hasDataInfo <- .dataframeFromSSim(tt, csv = FALSE, convertToLogical = c("data", "dataInherited"))
      if (!is.element("data", names(hasDataInfo))) {
        hasDataInfo$data <- FALSE
        warning("missing data column. assume FALSE")
      }
      if (length(setdiff(hasDataInfo$name, sumInfo$name)) > 0) {
        sumInfo <- .datasheets(x, project[[1]], scenario[[1]], refresh = TRUE)
        sumInfo$order <- seq(1, nrow(sumInfo))
        if (is.null(name)) {
          name <- sumInfo$name
          allNames <- name
        }
      }
      if (sum(hasDataInfo$dataInherited) > 0) {
        addCols <- c("name", "data", "dataInherited", "dataSource")
      } else {
        addCols <- c("name", "data")
      }
      hasDatBit <- subset(hasDataInfo, select = addCols)
      hasDatBit$scenario <- i
      
      if (i == 1) {
        hasDatAll <- hasDatBit
      } else {
        hasDatAll <- rbind(hasDatAll, hasDatBit)
      }
    }
    
    prevNames <- names(sumInfo)
    sumInfo <- merge(sumInfo, hasDatAll, all.x = TRUE)
    sumInfo <- subset(sumInfo, select = c(prevNames, setdiff(names(sumInfo), prevNames)))
    sumInfo <- sumInfo[order(sumInfo$order, sumInfo$scenario), ]
    sumInfo$order <- NULL
    sumInfo <- sumInfo[sumInfo$scope == scopeDS,]
    
    return(sumInfo)
  }
  
  dir.create(.tempfilepath(x), showWarnings = FALSE, recursive = TRUE)
  outSheetList <- list()
  
  # Loop through all datasheet names
  for (kk in seq(length.out = length(allNames))) {

    cat("\n=== Processing sheet:", name, "===\n")
    print(list(pid = pid, sid = sid, sheetScope = scopeDS))
    
    if (summary == FALSE) {
      
      name <- allNames[kk] # TODO see if name and cName are fullt subsituable
      cName <- name
      datasheetNames <- .datasheets(x, scope = "all")
      sheetNames <- subset(datasheetNames, name == cName)
    
      if (nrow(sheetNames) == 0) {
        datasheetNames <- .datasheets(x, scope = "all", core = TRUE)
        sheetNames <- subset(datasheetNames, name == cName)
        if (nrow(sheetNames) == 0) {
          stop("Datasheet ", name, " not found in library.")
        }
      }
      
      # Also check here if second part of filterColumn is not an int, find corresponding ID int
      if (!is.null(filterColumn)) {
        
        if (is.null(filterValue)) {
          stop("filterColumn specified without a filterValue.")
        }
        
        # Check if column exists in Datasheet
        args <- list(list = NULL, columns = NULL, lib = .filepath(x), sheet = name)
        tt <- command(args, session = session(x))
        datasheetCols <- .dataframeFromSSim(tt, csv = FALSE)
        
        if (!(filterColumn %in% datasheetCols$name)) {
          stop("Column '", filterColumn, "' is not present in the datasheet.")
        }

        # verify filterValues are present in filterColumn
        tempFile <- tempfile(fileext = ".csv")
        args <- list(export = NULL, lib = .filepath(x), sheet = name,
                    file = tempFile, valsheets = NULL, force = NULL)
        args <- assignPidSid(args, sheetNames, pid, sid)
        tt <- command(args, session = session(x))

        if (!identical(tt, "saved")) {
          stop("Unable to export datasheet for filter check: ", tt)
        }

        dsPreview <- read.csv(tempFile, as.is = TRUE, encoding = "UTF-8")
        unlink(tempFile)

        presentVals  <- unique(dsPreview[[filterColumn]])
        missingVals  <- setdiff(filterValue, presentVals)

        if (length(missingVals) > 0) {
          stop(
            paste0(
              "The following filterValue(s) are not present in column '",
              filterColumn, "': ",
              paste(missingVals, collapse = ", ")
            )
          )
        }

        fvInt <- suppressWarnings(as.integer(filterValue))

        if (all(is.na(fvInt))) {
          
          inputDatasheetName <- subset(datasheetCols, 
                                       name == filterColumn)$formula1
          
          if (inputDatasheetName == "N/A") {
            inputDatasheetName <- name
          }
            
          tempFile <- paste0(.tempfilepath(x), "/", name, ".csv")
          unlink(tempFile)
          args <- list(export = NULL, lib = .filepath(x), sheet = inputDatasheetName,
                       file = tempFile, valsheets = NULL, extfilepaths = NULL,
                       includepk = NULL, force = NULL)
          args <- assignPidSid(args, sheetNames, pid, sid)
          tt <- command(args, session = session(x))
          inputDatasheet <- read.csv(tempFile, as.is = TRUE, encoding = "UTF-8")
          matchedRows <- inputDatasheet$Name %in% filterValue
          newColID <- inputDatasheet[matchedRows, ][[filterColumn]]
          
          if (length(newColID) == 0) {
            stop("filterValue not found in filterColumn.")
          }
          
        }
      }
    }
    
    rmCols <- c()
    
    if (!includeKey) {
      args <- list(list = NULL, columns = NULL, allprops = NULL, csv = NULL, lib = .filepath(x), sheet = name)
      tt <- command(args, session = session(x))
      cPropsAll <- .dataframeFromSSim(tt)
      filtered <- cPropsAll[grep("isPrimary^Yes", cPropsAll$properties, fixed = TRUE), ]
      if (nrow(filtered) > 0) {
        rmCols <- filtered[1]
      }
    }
    
    # Use console means using the console either to write out a query to file OR
    # write the datasheet directly
    
    useConsole <- TRUE
    tempFile <- paste0(.tempfilepath(x), "/", name, ".csv")
    
    if (!empty | lookupsAsFactors) {
      # If non empty set, carry on with the retrieving of data
      
      # Use console if filterColumn argument is used
      if (!is.null(filterColumn)){
        useConsole <- TRUE
      }

      # Policy change - always query output directly from database. It is faster.
      useConsole <- useConsole & ((sqlStatement$select == "SELECT *")) # &(!lookupsAsFactors))
      useConsole <- useConsole & !((sheetNames$scope == "project") & (length(pid) > 1))
      # Force DB query for multi-scenario sheets (console export loses ScenarioId!)
      if (sheetNames$scope == "scenario" && length(sid) > 1) {
          useConsole <- FALSE
      } else {
          useConsole <- TRUE
      }
      # => These send you to query building (case for BOTH fastQuery and UseConsole are FALSE) if :
      # sql statement is complex, or more than one proj/sce is provided
      
      if (useConsole | fastQuery) {
        unlink(tempFile)
        
        if (fastQuery) {
          
          # Writes out a file
          if (lookupsAsFactors) {
            
            args <- list(export = NULL, lib = .filepath(x), sheet = name, file = tempFile, valsheetsonly = NULL, force = NULL)
            args <- assignPidSid(args, sheetNames, pid[1], sid[1]) # TODO make sure vector
            
            tt <- command(args, .session(x))
            
            if (!identical(tt, "saved")) {
              stop(tt)
            }
          }
          
          sheetList <- list()
          for (id in seq_along(sid)){
            
            args <- list(export = NULL, lib = .filepath(x), sheet = name, file = tempFile, queryonly = NULL, force = NULL, includepk = NULL, colswithdata = NULL)
            args <- assignPidSid(args, sheetNames, pid[id], sid[id])
            
            tt <- command(args, .session(x))
            
            # If error, catch it
            if (!identical(tt, "saved")) {
              if (!grep("No columns found", tt)) {
                stop(tt)
              } else {
                sheet <- data.frame()
              }
              
            } else {
              # Otherwise, carry on
              sql <- readChar(tempFile, file.info(tempFile)$size)
              
              drv <- DBI::dbDriver("SQLite")
              fqcon <- DBI::dbConnect(drv, .filepath(x))
              sheet <- DBI::dbGetQuery(fqcon, sql)
              
              # Adding pid and sid because the sql query generated by the 
              # console does not include them
              if (nrow(sheet) > 0 & (length(pid)>1 | length(sid)>1)){
                sheet$ScenarioId <- sid[id]
                sheet$ProjectId <- pid[id]
              } else if (length(sid) == 1 && returnScenarioInfo){
                sheet$ScenarioId <- sid
                sheet$ProjectId <- pid
              }
              
              # Rearrange on the spot, making sure this is robust for Project-level
              # datasheets as well
              IDColumnsForThisSheet <- IDColumns[IDColumns %in% names(sheet)]
              sheet <- sheet[, c(IDColumnsForThisSheet, 
                                 names(sheet)[!(names(sheet) %in% IDColumnsForThisSheet)])]
              
              sheetList[[id]] <- sheet
              DBI::dbDisconnect(fqcon)
            }
            
          }
          sheet <- do.call(gtools::smartbind, sheetList)
          
        } else {
          # If fastQuery is false, do this
          # This happens IF fast query is FALSE and if not complex
          # It writes out the csv to temp file
          if (!optional && (sheetNames$scope != "library")) {
            args <- list(export = NULL, lib = .filepath(x), sheet = name, 
                         file = tempFile, valsheets = NULL, extfilepaths = NULL, 
                         includepk = NULL, force = NULL, colswithdata = NULL)
          } else {
            args <- list(export = NULL, lib = .filepath(x), sheet = name, 
                         file = tempFile, valsheets = NULL, extfilepaths = NULL, 
                         includepk = NULL, force = NULL)
          }
          args <- assignPidSid(args, sheetNames, pid, sid)

          filteringDone <- FALSE
          
          if (!is.null(filterColumn)) {
            if (length(filterValue) > 1) {
              allSheets <- list()
              
              for (fv in filterValue) {
                tempFileLoop <- tempfile(fileext = ".csv")
                
                argsLoop <- args
                argsLoop[["file"]] <- tempFileLoop
                argsLoop[["filtercol"]] <- paste0(filterColumn, "=", fv)
                
                ttLoop <- command(argsLoop, .session(x))
                if (!identical(ttLoop, "saved")) {
                  stop("Unable to export datasheet for filterValue '", fv, "': ", ttLoop)
                }

                if (!file.exists(tempFileLoop)) {
                  stop("Expected export file was not created: ", tempFileLoop)
                }

                oneSheet <- read.csv(tempFileLoop, as.is = TRUE, encoding = "UTF-8")
                unlink(tempFileLoop)
                
                if (nrow(oneSheet) > 0) {
                  allSheets[[length(allSheets) + 1]] <- oneSheet
                }
              }
              
              # Merge all data frames together
              if (length(allSheets) == 0) {
                stop(
                  paste0(
                    "None of the filterValue(s) ", paste(filterValue, collapse = ", "),
                    " are present in column '", filterColumn, "'."
                  )
                )
              }
              sheet <- do.call(rbind, allSheets)

              filteringDone <- TRUE 
              
            } else {
              args[["filtercol"]] <- paste0(filterColumn, "=", filterValue)

              filteringDone <- FALSE
            }
          }
          
          if (!filteringDone) {
            if (!is.null(filterColumn) && length(filterValue) == 1) {
              args[["filtercol"]] <- paste0(filterColumn, "=", filterValue)
            }

            if (rawValues) {
              args[["valSheet"]] <- NULL
              args <- append(args, list(rawvalues = NULL))
            }

            tt <- command(args, .session(x))
            if (!identical(tt, "saved")) {
              stop(tt)
            }

            cat("CONSOLE EXPORT path\n")

            sheet <- read.csv(tempFile, as.is = TRUE, encoding = "UTF-8")
            
            cat("Rows/Cols after import:", nrow(sheet), ncol(sheet), "\n")
          }
        }
        
        unlink(tempFile)
        
      } else {
        # Query database directly if necessary
        # This bit construct a query and call directly without using the console
        # This happens if BOTH fastQuery and UseConsole are FALSE
        
        drv <- DBI::dbDriver("SQLite")
        con <- DBI::dbConnect(drv, .filepath(x))
        
        if (is.null(sqlStatement$where)) {
          sqlStatement$where <- ""
        }
        
        sqlStatement$from <- paste("FROM", name)
        
        if (sheetNames$scope == "scenario") {
          if (is.null(sid)) {
            stop("Specify a scenario.")
          } else {
            # following https://faculty.washington.edu/kenrice/sisg-adv/sisg-09.pdf
            # and https://www.sqlitetutorial.net/sqlite-in/
            if (sqlStatement$where == "") {
              sqlStatement$where <- paste0("WHERE ScenarioId IN (", paste(sid, collapse = ","), ")")
            } else {
              sqlStatement$where <- paste0(sqlStatement$where, " AND (ScenarioId IN (", paste(sid, collapse = ","), "))")
            }
          }
        }
        
        if (sheetNames$scope == "project") {
          if (is.null(pid)) {
            stop("Specify a project.")
          } else {
            if (sqlStatement$where == "") {
              sqlStatement$where <- paste0("WHERE ProjectId IN (", paste(pid, collapse = ","), ")")
            } else {
              sqlStatement$where <- paste0(sqlStatement$where, " AND (ProjectId IN (", paste(pid, collapse = ","), "))")
            }
          }
        }
        
        sql <- paste(sqlStatement$select, sqlStatement$from, sqlStatement$where, sqlStatement$groupBy)
        sheet <- DBI::dbGetQuery(con, sql)
        # Normalize DB column names to expected SyncroSim case conventions
        names(sheet) <- sub("^ScenarioID$", "ScenarioId", names(sheet), ignore.case = TRUE)
        names(sheet) <- sub("^ProjectID$",  "ProjectId", names(sheet), ignore.case = TRUE)
        cat("DB query path: columns = ", paste(names(sheet), collapse=", "), "\n")
        DBI::dbDisconnect(con)

        cat("Rows/Cols after import:", nrow(sheet), ncol(sheet), "\n")
        
        # Filter out columns without data (drop NA columns) 
        if (!optional && (nrow(sheet) > 0)) {
          sheet <- sheet[!(colSums(is.na(sheet)) == nrow(sheet))]
        }
      }
      
      if (empty) {
        if (nrow(sheet) > 0){
          sheet <- sheet[0,]
        }
      }
    } else {
      # If empty set
      sheet <- data.frame(temp = NA)
      sheet <- subset(sheet, !is.na(temp))
    }
    
    if (nrow(sheet) > 0) {
      sheet[sheet == ""] <- NA
    }
    
    # TODO review this, this bit assign the correct data types 
    if (empty | lookupsAsFactors | !returnInvisible) {
      
      tt <- command(c("list", "columns", "csv", paste0("lib=", .filepath(x)), paste0("sheet=", name)), .session(x))
      sheetInfo <- .dataframeFromSSim(tt)
      sheetInfo$id <- seq(length.out = nrow(sheetInfo))
      sheetInfo <- subset(sheetInfo, !is.element(name, rmCols))
      
      if (!optional) {
        if (!empty) {
          sheetInfo$optional[is.element(sheetInfo$name, names(sheet)) & (sheetInfo$optional == "Yes")] <- "Present"
        }
        sheetInfo <- subset(sheetInfo, is.element(optional, c("No", "Present")))
      }
      
      if (!returnInvisible) {
        sheetInfo <- sheetInfo[sheetInfo$visible == "Yes",]
      }
      
      sheetInfo <- sheetInfo[order(sheetInfo$id), ]
      
      if (nrow(sheet) == 0) {
        sheet[1, 1] <- NA
      }
      
      outNames <- c()
      
      directQuery <- FALSE
      if (lookupsAsFactors & !useConsole) {
        directQuery <- (length(pid) > 1) | (length(sid) > 1)
        # TO DO: must export IDs in lookup tables.
        if (directQuery) {
          drv <- DBI::dbDriver("SQLite")
          con <- DBI::dbConnect(drv, .filepath(x))
          # console export can't handle multiple scenarios/projects - so query database directly
        } else {
          
          tempFile <- paste0(.tempfilepath(x), "/", name, ".csv")
          args <- list(export = NULL, lib = .filepath(x), sheet = name, 
                       file = tempFile, valsheetsonly = NULL, force = NULL, 
                       includepk = NULL)
          args <- assignPidSid(args, sheetNames, pid, sid)
          
          tt <- command(args, .session(x))
          if (!identical(tt, "saved")) {
            stop(tt, "You might be asking for a datasheet at the project level but that datasheet has a scenario scope")
          }
        }
      }
      
      for (i in seq(length.out = nrow(sheetInfo))) {

        cRow <- sheetInfo[i, ]

        if (cRow$name == "ScenarioId") {
          cat("*** About to assign ScenarioId ***\n")
          print(list(
            sid = sid,
            sheet_nrows = nrow(sheet),
            existing_col = sheet[["ScenarioId"]]
          ))
        }
        
        # ------------------------------
        # ScenarioId Handling (correct)
        # ------------------------------
        if (cRow$name == "ScenarioId") {

            # DB query path (multi or single scenario)
            # DB output already contains ScenarioId (after earlier normalization)
            if ("ScenarioId" %in% names(sheet)) {
                outNames <- c(outNames, "ScenarioId")
                next
            }

            # Console export: multi-scenario
            # Console never returns ScenarioId for >1 scenario
            # Leave NA to be filled later during merge with allScns
            if (length(sid) > 1) {
                sheet$ScenarioId <- NA_integer_
                outNames <- c(outNames, "ScenarioId")
                next
            }

            # Console export: single scenario
            # Console output missing ScenarioId → fill with scalar sid
            if (length(sid) == 1) {
                sheet$ScenarioId <- rep(sid, nrow(sheet))
                outNames <- c(outNames, "ScenarioId")
                next
            }

            # Fallback (should never be hit)
            sheet$ScenarioId <- NA_integer_
            outNames <- c(outNames, "ScenarioId")
            next
        }


        # ------------------------------
        # Non-ScenarioId handling
        # Only fill missing columns when needed
        # ------------------------------
        if (!is.element(cRow$name, colnames(sheet))) {

            # For SELECT * queries, missing columns become NA
            if (sqlStatement$select == "SELECT *") {
                sheet[[cRow$name]] <- NA
                outNames <- c(outNames, cRow$name)
                next
            }

            # Otherwise skip; column to be ignored
            next
        }

        # If we reach here, column already exists; include in outNames
        outNames <- c(outNames, cRow$name)
        
        if ((cRow$type %in% c("Integer", "Double", "Single")) & !(cRow$valType %in% c("DataSheet", "List"))) {
          sheet[[cRow$name]] <- as.numeric(sheet[[cRow$name]])
        }
        if (cRow$type == "String") {
          sheet[[cRow$name]] <- as.character(sheet[[cRow$name]])
        }
        if (cRow$type == "Boolean") {
          if (length(setdiff(unique(sheet[[cRow$name]]), c(NA))) > 0) {
            sheet[[cRow$name]] <- gsub("Yes", "1", sheet[[cRow$name]])
            sheet[[cRow$name]] <- gsub("No", "0", sheet[[cRow$name]])
            sheet[[cRow$name]] <- as.logical(abs(as.numeric(sheet[[cRow$name]])))
            # TO DO: stop("handle this case")
          }
        }
        if ((cRow$valType == "List") & lookupsAsFactors) {
          opts <- cRow$formula1
          opts <- strsplit(opts, "|", fixed = TRUE)[[1]]
          cLevels <- c()
          cIDs <- c()
          for (j in seq(length.out = length(opts))) {
            cLevels <- c(cLevels, strsplit(opts[j], ":", fixed = TRUE)[[1]][2])
            cIDs <- as.numeric(c(cIDs, strsplit(opts[j], ":", fixed = TRUE)[[1]][1]))
          }
          # Sometimes input is factors, and output is  IDs
          if (length(setdiff(sheet[[cRow$name]], cIDs)) == 0) {
            warning(paste0("Converting ", cRow$name, " IDs to factor levels"))
            mergeBit <- data.frame(oLev = cLevels)
            mergeBit[[cRow$name]] <- cIDs
            sheet <- merge(sheet, mergeBit, all.x = TRUE)
            sheet[[cRow$name]] <- sheet$oLev
            sheet$oLev <- NULL
          }
          sheet[[cRow$name]] <- factor(sheet[[cRow$name]], levels = cLevels)
        }
        if (cRow$valType == "DataSheet") {
          if (lookupsAsFactors) {
            
            # Find display member to create factors from
            tt <- command(args = list(lib = .filepath(x), list = NULL, 
                                      datasheets = NULL, includesys = NULL), 
                          session = .session(x))
            tt <- .dataframeFromSSim(tt, csv = FALSE)
            displayMem <- tt[tt$name == cRow$formula1,]$displayMember
            
            # console export can't handle multiple projects/scenarios - so query database directly if necessary.
            if (directQuery) {
              lookupSheet <- DBI::dbReadTable(con, name = cRow$formula1)
            } else {
              lookupPath <- gsub(name, cRow$formula1, tempFile, fixed = TRUE)
              if (!file.exists(lookupPath)) {
                lookupSheet <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), 
                                        c(displayMem))
              } else {
                lookupSheet <- read.csv(lookupPath, as.is = TRUE)
              }
            }
            if (is.element("ProjectId", names(lookupSheet))) {
              if (identical(pid, NULL) & !identical(sid, NULL)) {
                allScns <- scenario(x)
                findPrjs <- allScns$ProjectId[is.element(allScns$ScenarioId, sid)]
              } else {
                findPrjs <- pid
              }
              lookupSheet <- subset(lookupSheet, is.element(ProjectId, pid))
            }
            if (is.element("ScenarioId", names(lookupSheet))) {
              if (!is.null(sid)) {
                lookupSheet <- subset(lookupSheet, is.element(ScenarioId, sid))
              }
            }
            if ((nrow(lookupSheet) == 0) & (cRow$optional == "No")) {
              if (!grepl("Output", name)) {
                warning(paste0(cRow$name, " depends on ", cRow$formula1, 
                               ". You should load ", cRow$formula1, 
                               " before setting ", name, "."))
              }
            }
            if (nrow(lookupSheet) > 0) {
              lookupSheet <- lookupSheet[order(lookupSheet[[names(lookupSheet[1])]]), ]
              # Ensure factor levels are unique to avoid "duplicated levels" error
              lookupLevels <- unique(lookupSheet[[displayMem]])
            } else {
              lookupLevels <- c()
            }
            if (is.numeric(sheet[[cRow$name]])) {
              if (nrow(lookupSheet) > 0) {
                if (length(intersect(displayMem, names(lookupSheet))) == 0) {
                  stop(paste0("Something is wrong. Expecting ", displayMem, 
                              " in lookup table."))
                }
                
                lookupMerge <- subset(lookupSheet, 
                                      select = c(names(lookupSheet)[1], 
                                                 displayMem))
                
                names(lookupMerge) <- c(cRow$name, "lookupName")
                sheet <- merge(sheet, lookupMerge, all.x = TRUE)
                if(!all(is.na(sheet$lookupName)))
                  sheet[[cRow$name]] <- sheet$lookupName
                sheet$lookupName <- NULL
              }
            }
            sheet[[cRow$name]] <- factor(sheet[[cRow$name]], levels = lookupLevels)
            # TO DO: handle formula1/formula2
          } else {
            sheet[[cRow$name]] <- as.character(sheet[[cRow$name]])
          }
        }
        
        if (cRow$valType != "List" && cRow$formula2 != "N/A") {
          if (cRow$valCond == "Between" && verbose) {
            print(paste0("Note: ", cRow$name, " should be between ", cRow$formula1, " and ", cRow$formula2))
          }
        }
      }
      
      if (lookupsAsFactors && !useConsole && directQuery) {
        DBI::dbDisconnect(con)
      }
      
      rmSheets <- unique(sheetInfo$formula1[sheetInfo$valType == "DataSheet"])
      
      for (i in seq(length.out = length(rmSheets))) {
        unlink(gsub(name, rmSheets[i], tempFile, fixed = TRUE))
      }
      
      # Remove rows that are all NA
      sheet <- sheet[rowSums(is.na(sheet)) != ncol(sheet), ]
      
      sheet <- subset(sheet, select = outNames)
    } else {
      if (!is.null(rmCols)) {
        sheetNames <- names(sheet)
        for (rr in 1:length(sheetNames)) {
          cName <- sheetNames[rr]
          if (is.element(cName, rmCols)) {
            sheet[[cName]] <- NULL
          }
        }
      }
    }
    
    if (is.element("ProjectId", names(sheet))) {
      if (length(pid) == 1) {
        sheet$ProjectId <- NULL
      } else {
        if (nrow(sheet) > 0) {
          allProjects <- .project(x)
          names(allProjects) <- c("ProjectId", "ProjectName")
          sheet <- merge(allProjects, sheet, all.y = TRUE)
        }
      }
    }
    if (is.element("ScenarioId", names(sheet))) {

      cat("ScenarioId handling block: before merge\n")
      print(head(sheet))

      if (length(sid) > 1){
        returnScenarioInfo <- TRUE
      }
      if (length(sid) == 1){
        sheet$ScenarioId <- NULL
      }
      if (nrow(sheet) > 0 && returnScenarioInfo) {
        if (is(x, "SsimLibrary")){
          lib <- x
        } else {
          lib <- .ssimLibrary(x)
        }
        allScns <- scenario(lib, summary = TRUE)
        if (!is.element("ParentId", names(allScns))) {
          warning("Missing ParentId info from scenario(summary=TRUE).")
          allScns$ParentId <- NA
        }
        allScns <- subset(allScns, select = c(ScenarioId, ProjectId, Name, ParentId))
        allScns$ParentId <- suppressWarnings(as.numeric(allScns$ParentId))
        parentNames <- subset(allScns, select = c(ScenarioId, Name))
        names(parentNames) <- c("ParentId", "ParentName")
        allScns <- merge(allScns, parentNames, all.x = T)
        
        allScns <- subset(allScns, select = c(ScenarioId, ProjectId, Name, ParentId, ParentName))
        
        names(allScns) <- c("ScenarioId", "ProjectId", "ScenarioName", "ParentId", "ParentName")
        allScns <- allScns[allScns$ScenarioId %in% sid,]
        
        # KEEP ScenarioId in sheet for multi-scenario joins
        # Only drop ScenarioId if single-scenario extraction
        if (length(sid) == 1) {
            sheet$ScenarioId <- NULL
        }
        # FIX: join by ScenarioId only
        sheet <- merge(allScns, sheet, by = "ScenarioId", all.y = TRUE)
      }
    }
    
    sheet <- sheet[rowSums(is.na(sheet)) != ncol(sheet), , drop = FALSE]
    outSheetList[[cName]] <- sheet
    
    # return single row datasheets as named vectors (if not for multiple scenarios)
    # note info about data types and lookups will be lost if we do this. so don't.
    if (FALSE && sheetNames$isSingle && (nrow(sheet) <= 1)) {
      if (nrow(sheet) == 0) {
        vec <- rep(" ", ncol(sheet))
        vec <-
          names(vec) <- names(sheet)
      } else {
        vec <- unlist(sheet[1, ])
      }
    }
  }
  
  if (!forceElements & (length(outSheetList) == 1)) {
    outSheetList <- outSheetList[[1]]
  }
  
  unlink(.tempfilepath(x), recursive = TRUE)
  return(outSheetList)
})

# Helper function 
# Assign PID and SID to the argument list
assignPidSid <- function(args, sheetNames, pid, sid){
  if (sheetNames$scope == "project") {
    args[["pid"]] <- pid
  }
  if (is.element(sheetNames$scope, c("project", "scenario"))) {
    args[["pid"]] <- pid
  }
  if (sheetNames$scope == "scenario") {
    args[["sid"]] <- sid
  }
  return(args)
}
