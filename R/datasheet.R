# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Get a datasheet
#'
#' Retrieves a SyncroSim datasheet.
#'
#' @details
#'
#' If summary=TRUE or summary=NULL and name=NULL a dataframe describing the datasheets is returned:
#'   If optional=TRUE columns include: scope, package, name, displayName, isSingle, isOutput, data.
#'   data only displayed for scenarios. dataInherited and dataSource columns added if a scenario has dependencies.
#'   If optional=FALSE columns include: scope, name, displayName.
#'   All other arguments are ignored.
#'
#' Otherwise, for each element in name a datasheet is returned as follows:
#' \itemize{
#'   \item {If lookupsAsFactors=TRUE (default): }{Each column is given the correct data type, and dependencies returned as factors with allowed values (levels). A warning is issued if the lookup has not yet been set.}
#'   \item {If empty=TRUE: }{Each column is given the correct data type. Fast (1 less console command)}
#'   \item {If empty=FALSE and lookupsAsFactors=FALSE: }{Column types are not checked, and the optional argument is ignored. Fast (1 less console command).}
#'   \item {If ssimObject is a list of Scenario or Project objects (output from run(), scenario() or project()): }{Adds ScenarioID/ProjectID column if appropriate.}
#'   \item {If scenario/project is a vector: }{Adds ScenarioID/ProjectID column as necessary.}
#'   \item {If requested datasheet has scenario scope and contains info from more than one scenario: }{ScenarioID/ScenarioName/ScenarioParent columns identify the scenario by name, id, and parent (if a result scenario)}
#'   \item {If requested datasheet has project scope and contains info from more than one project: }{ProjectID/ProjectName columns identify the project by name and id.}
#' }
#'
#' @param ssimObject SsimLibrary/Project/Scenario, or list of objects. Note that all objects in a list must be of the same type, and belong to the same library.
#' @param name Character or vector of these. Sheet name(s). If NULL, all datasheets in the ssimObject will be returned. Note that setting summary=FALSE and name=NULL pulls all datasheets, which is timeconsuming and not generally recommended.
#' @param project Character, numeric, or vector of these. One or more Project names, ids or objects. Note that integer ids are slightly faster.
#' @param scenario Character, numeric, or vector of these. One or more Scenario names, ids or objects. Note that integer ids are slightly faster.
#' @param summary Logical. If TRUE returns a dataframe of sheet names and other info. If FALSE returns dataframe or list of dataframes.
#' @param optional Logical. If summary=TRUE and optional=TRUE returns only scope, name and displayName. If summary=FALSE and optional=TRUE returns all of the datasheet's columns, including the optional columns. If summary=TRUE, optional=FALSE, returns only those columns that are mandatory and contain data (if empty=FALSE). Ignored if summary=FALSE, empty=FALSE and lookupsAsFactors=FALSE.
#' @param empty Logical. If TRUE returns empty dataframes for each datasheet. Ignored if summary=TRUE.
#' @param lookupsAsFactors Logical. If TRUE (default) dependencies returned as factors with allowed values (levels). Set FALSE to speed calculations. Ignored if summary=TRUE.
#' @param sqlStatement List returned by sqlStatement(). SELECT and GROUP BY SQL statements passed to SQLite database. Ignored if summary=TRUE.
#' @param includeKey Logical. If TRUE include primary key in table.
#' @param forceElements Logical. If FALSE and name has a single element returns a dataframe; otherwise a list of dataframes. Ignored if summary=TRUE.
#' @param fastQuery Logical.  If TRUE, the request is optimized for performance.  Ignored if combined with summary, empty, or sqlStatement flags.
#' 
#' @return 
#' If summary=TRUE returns a dataframe of datasheet names and other info, otherwise returns a dataframe or list of these.
#' 
#' @export
#' @import RSQLite
setGeneric("datasheet", function(ssimObject, name = NULL, project = NULL, scenario = NULL, summary = NULL, optional = FALSE, empty = FALSE, lookupsAsFactors = TRUE, sqlStatement = list(select = "SELECT *", groupBy = ""), includeKey = FALSE, forceElements = FALSE, fastQuery = FALSE) standardGeneric("datasheet"))

# Handles case where ssimObject is list of Scenario or Project objects
#' @rdname datasheet
setMethod("datasheet", signature(ssimObject = "list"), function(ssimObject, name, project, scenario, summary, optional, empty, lookupsAsFactors, sqlStatement, includeKey, forceElements, fastQuery) {
  cScn <- ssimObject[[1]]
  x <- NULL
  if (class(cScn) == "Scenario") {
    x <- getIdsFromListOfObjects(ssimObject, expecting = "Scenario", scenario = scenario, project = project)
    scenario <- x$objs
    project <- NULL
  }
  if (class(cScn) == "Project") {
    x <- getIdsFromListOfObjects(ssimObject, expecting = "Project", scenario = scenario, project = project)
    project <- x$objs
    scenario <- NULL
  }
  ssimObject <- x$ssimObject
  if (is.null(ssimObject)) {
    stop("Expecting ssimObject to be an SsimLibrary/Project/Scenario, or a list of Scenarios/Projects.")
  }
  # Now have scenario/project ids of same type in same library, and ssimObject is library

  out <- .datasheet(ssimObject, name = name, project = project, scenario = scenario, summary = summary, optional = optional, empty = empty, lookupsAsFactors = lookupsAsFactors, sqlStatement = sqlStatement, includeKey = includeKey, forceElements = forceElements, fastQuery = fastQuery) # Off for v0.1

  return(out)
})

#' @rdname datasheet
setMethod("datasheet", signature(ssimObject = "character"), function(ssimObject, name, project, scenario, summary, optional, empty, lookupsAsFactors, sqlStatement, includeKey, fastQuery) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname datasheet
setMethod("datasheet", signature(ssimObject = "SsimObject"), function(ssimObject, name, project, scenario, summary, optional, empty, lookupsAsFactors, sqlStatement, includeKey, forceElements, fastQuery) {
  temp <- NULL
  ProjectID <- NULL
  ScenarioID <- NULL
  colOne <- NULL
  parentID <- NULL
  ParentName <- NULL
  xProjScn <- .getFromXProjScn(ssimObject, project, scenario, returnIds = TRUE, convertObject = FALSE, complainIfMissing = TRUE)
  
  if (class(xProjScn) == "SsimLibrary") {
    x <- xProjScn
    pid <- NULL
    sid <- NULL
  } else {
    x <- xProjScn$ssimObject
    pid <- xProjScn$project
    sid <- xProjScn$scenario
    if (!is.null(sid) & is.null(pid)) {
      pid <- subset(xProjScn$scenarioSet, is.element(scenarioId, sid))$projectId
    }
  }
  # now have valid pid/sid vectors and x is library.

  if (!is.null(name)) {
    for (i in seq_along(name)) {
      n <- name[i]
      if (!grepl("_", n, fixed = TRUE)) {
        l = ssimLibrary(.filepath(ssimObject), summary=T)
        p = l$value[l$property == "Package Name:"]
        n <- paste0(p, "_", n)
      }

      if (grepl("STSim_", n, fixed = TRUE)) {
        warning("An STSim_ prefix for a datasheet name is no longer required.")
        n <- paste0("stsim_", gsub("STSim_", "", n, fixed = TRUE))
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
  if (summary | is.null(name)) {
    sumInfo <- .datasheets(x, project[[1]], scenario[[1]])
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
  if (is.null(name)) {
    stop("Something is wrong in datasheet().")
  }

  if (summary & !optional) {
    sumInfo <- subset(sumInfo, select = c("scope", "name", "displayName", "order"))
    sumInfo[order(sumInfo$order), ]
    sumInfo$order <- NULL
    return(sumInfo)
  }

  # Add data info - only for scenario scope datasheets if sid is defined
  if (summary) {
    # if no scenario scope sheets, return sumInfo without checking for data
    scnSheetSum <- sum(sumInfo$scope == "scenario")

    if (scnSheetSum == 0) {
      sumInfo[order(sumInfo$order), ]
      sumInfo$order <- NULL
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
    return(sumInfo)
  }

  dir.create(.tempfilepath(x), showWarnings = FALSE, recursive = TRUE)
  outSheetList <- list()
  
  for (kk in seq(length.out = length(allNames))) {
    
    name <- allNames[kk] # TODO fix this name thing here
    cName <- name
    datasheetNames <- .datasheets(x, scope = "all")
    sheetNames <- subset(datasheetNames, name == cName)
    
    if (nrow(sheetNames) == 0) {
      datasheetNames <- .datasheets(x, scope = "all", refresh = TRUE)
      sheetNames <- subset(datasheetNames, name == cName)
      if (nrow(sheetNames) == 0) {
        stop("Datasheet ", name, " not found in library.")
      }
    }

    rmCols <- c()

    if (!sheetNames$isOutput) {
      if (!includeKey) {
        args <- list(list = NULL, columns = NULL, allprops = NULL, csv = NULL, lib = .filepath(x), sheet = name)
        tt <- command(args, session = session(x))
        cPropsAll <- .dataframeFromSSim(tt)
        filtered <- cPropsAll[grep("isPrimary^Yes", cPropsAll$properties, fixed = TRUE), ]
        if (nrow(filtered) > 0) {
          rmCols <- filtered[1]
        }
      }
    }

    useConsole <- FALSE
    tempFile <- paste0(.tempfilepath(x), "/", name, ".csv")
    
    if (!empty) {
      # Only query database if output or multiple scenarios/project or complex sql
      useConsole <- (!sheetNames$isOutput)

      # Policy change - always query output directly from database. It is faster.
      useConsole <- useConsole & ((sqlStatement$select == "SELECT *")) # &(!lookupsAsFactors))
      useConsole <- useConsole & !((sheetNames$scope == "project") & (length(pid) > 1))
      useConsole <- useConsole & !((sheetNames$scope == "scenario") & (length(sid) > 1))

      if (useConsole | fastQuery) {
        unlink(tempFile)

        if (fastQuery) {
          if (lookupsAsFactors) {
            
            args <- list(export = NULL, lib = .filepath(x), sheet = name, file = tempFile, valsheetsonly = NULL, force = NULL)
            args <- assignPid(args, sheetNames)
            tt <- command(args, .session(x))

            if (!identical(tt, "saved")) {
              stop(tt)
            }
          }

          args <- list(export = NULL, lib = .filepath(x), sheet = name, file = tempFile, queryonly = NULL, force = NULL, includepk = NULL, colswithdata = NULL)
          args <- assignPid(args, sheetNames)
          tt <- command(args, .session(x))

          if (!identical(tt, "saved")) {
            if (!grep("No columns found", tt)) {
              stop(tt)
            } else {
              sheet <- data.frame()
            }
          } else {
            sql <- readChar(tempFile, file.info(tempFile)$size)

            drv <- DBI::dbDriver("SQLite")
            fqcon <- DBI::dbConnect(drv, .filepath(x))
            sheet <- DBI::dbGetQuery(fqcon, sql)
            DBI::dbDisconnect(fqcon)
          }
        } else {
          if (!optional & (sheetNames$scope != "library")) {
            args <- list(export = NULL, lib = .filepath(x), sheet = name, file = tempFile, valsheets = NULL, extfilepaths = NULL, includepk = NULL, force = NULL, colswithdata = NULL) # filepath=NULL
          } else {
            args <- list(export = NULL, lib = .filepath(x), sheet = name, file = tempFile, valsheets = NULL, extfilepaths = NULL, includepk = NULL, force = NULL) # filepath=NULL
          }
          args <- assignPid(args, sheetNames)
          tt <- command(args, .session(x))

          if (!identical(tt, "saved")) {
            stop(tt)
          }

          sheet <- read.csv(tempFile, as.is = TRUE, encoding = "UTF-8")
        }

        unlink(tempFile)
      } else {
        # query database directly if necessary

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
            # following http://faculty.washington.edu/kenrice/sisg-adv/sisg-09.pdf
            # and http://www.sqlitetutorial.net/sqlite-in/
            if (sqlStatement$where == "") {
              sqlStatement$where <- paste0("WHERE ScenarioID IN (", paste(sid, collapse = ","), ")")
            } else {
              sqlStatement$where <- paste0(sqlStatement$where, " AND (ScenarioID IN (", paste(sid, collapse = ","), "))")
            }
          }
        }
        
        if (sheetNames$scope == "project") {
          if (is.null(pid)) {
            stop("Specify a project.")
          } else {
            if (sqlStatement$where == "") {
              sqlStatement$where <- paste0("WHERE ProjectID IN (", paste(pid, collapse = ","), ")")
            } else {
              sqlStatement$where <- paste0(sqlStatement$where, " AND (ProjectID IN (", paste(pid, collapse = ","), "))")
            }
          }
        }
        
        sql <- paste(sqlStatement$select, sqlStatement$from, sqlStatement$where, sqlStatement$groupBy)
        sheet <- DBI::dbGetQuery(con, sql)
        DBI::dbDisconnect(con)

        # Filter out columns without data
        if (!optional && (nrow(sheet) > 0)) {
          colNames <- names(sheet)
          for (r in seq(length.out = length(colNames))) {
            cCol <- colNames[r]
            if (sum(!is.na(sheet[[cCol]])) == 0) {
              sheet[[cCol]] <- NULL
            }
          }
        }
      }
    } else {
      sheet <- data.frame(temp = NA)
      sheet <- subset(sheet, !is.na(temp))
    }
    
    if (nrow(sheet) > 0) {
      sheet[sheet == ""] <- NA
    }
    
    if (empty | lookupsAsFactors) {
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
          args <- list(export = NULL, lib = .filepath(x), sheet = name, file = tempFile, valsheetsonly = NULL, force = NULL, includepk = NULL)
          if (sheetNames$scope == "project") {
            args[["pid"]] <- pid
          }
          if (is.element(sheetNames$scope, c("project", "scenario"))) {
            args[["pid"]] <- pid
          }
          if (sheetNames$scope == "scenario") {
            args[["sid"]] <- sid
          }
          tt <- command(args, .session(x))
          if (!identical(tt, "saved")) {
            stop(tt)
          }
        }
      }
      
      for (i in seq(length.out = nrow(sheetInfo))) {
        cRow <- sheetInfo[i, ]
        if (!is.element(cRow$name, colnames(sheet))) {
          if (sqlStatement$select == "SELECT *") {
            sheet[[cRow$name]] <- NA
          } else {
            next
          }
        }
        outNames <- c(outNames, cRow$name)
        if ((is.element(cRow$type, c("Integer", "Double", "Single"))) & !is.element(cRow$valType, c("DataSheet", "List"))) {
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
            # console export can't handle multiple projects/scenarios - so query database directly if necessary.
            if (directQuery) {
              lookupSheet <- DBI::dbReadTable(con, name = cRow$formula1)
            } else {
              lookupPath <- gsub(name, cRow$formula1, tempFile, fixed = TRUE)
              if (!file.exists(lookupPath)) {
                lookupSheet <- data.frame(Name = NULL)
              } else {
                lookupSheet <- read.csv(lookupPath, as.is = TRUE)
              }
            }
            if (is.element("ProjectID", names(lookupSheet))) {
              if (identical(pid, NULL) & !identical(sid, NULL)) {
                allScns <- scenario(x)
                findPrjs <- allScns$projectId[is.element(allScns$scenarioId, sid)]
              } else {
                findPrjs <- pid
              }
              lookupSheet <- subset(lookupSheet, is.element(ProjectID, pid))
            }
            if (is.element("ScenarioID", names(lookupSheet))) {
              if (!is.null(sid)) {
                lookupSheet <- subset(lookupSheet, is.element(ScenarioID, sid))
              }
            }
            if ((nrow(lookupSheet) == 0) & (cRow$optional == "No")) {
              if (!grepl("Output", name)) {
                warning(paste0(cRow$name, " depends on ", cRow$formula1, ". You should load ", cRow$formula1, " before setting ", name, "."))
              }
            }
            if (nrow(lookupSheet) > 0) {
              lookupSheet <- lookupSheet[order(lookupSheet[[names(lookupSheet[1])]]), ]
              lookupLevels <- lookupSheet$Name
            } else {
              lookupLevels <- c()
            }
            if (is.numeric(sheet[[cRow$name]])) {
              if (nrow(lookupSheet) > 0) {
                if (length(intersect("Name", names(lookupSheet))) == 0) {
                  stop("Something is wrong. Expecting Name in lookup table.")
                }

                lookupMerge <- subset(lookupSheet, select = c(names(lookupSheet)[1], "Name"))

                names(lookupMerge) <- c(cRow$name, "lookupName")
                sheet <- merge(sheet, lookupMerge, all.x = TRUE)
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
        if (cRow$valType != "List") {
          if (cRow$formula2 != "N/A") {
            if (cRow$valCond == "Between") {
              print(paste0("Note: ", cRow$name, " should be between ", cRow$formula1, " and ", cRow$formula2))
            } else {
              stop("handle this case")
            }
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

      # TO DO: deal with NA values in sheet
      # put columns in correct order
      sheet$colOne <- sheet[, 1]
      sheet$cOrder <- seq(1, nrow(sheet))
      if (empty) {
        sheet <- subset(sheet, is.null(colOne))
      } else {
        sheet <- subset(sheet, !is.na(colOne))
        sheet <- sheet[order(sheet$cOrder), ]
      }
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
    
    if (is.element("ProjectID", names(sheet))) {
      if (length(pid) == 1) {
        sheet$ProjectID <- NULL
      } else {
        if (nrow(sheet) > 0) {
          if (is.null(allProjects)) {
            allProjects <- .project(x)
          }
          names(allProjects) <- c("ProjectID", "ProjectName")
          sheet <- merge(allProjects, sheet, all.y = TRUE)
        }
      }
    }

    if (is.element("ScenarioID", names(sheet))) {
      if (length(sid) == 1) {
        sheet$ScenarioID <- NULL
      } else {
        if (nrow(sheet) > 0) {
          allScns <- scenario(x, summary = TRUE)
          if (!is.element("parentID", names(allScns))) {
            warning("Missing parentID info from scenario(summary=TRUE).")
            allScns$parentID <- NA
          }
          allScns <- subset(allScns, select = c(scenarioId, projectId, name, parentID))
          allScns$parentID <- suppressWarnings(as.numeric(allScns$parentID))
          parentNames <- subset(allScns, select = c(scenarioId, name))
          names(parentNames) <- c("parentID", "ParentName")
          allScns <- merge(allScns, parentNames, all.x = TRUE)

          allScns <- subset(allScns, select = c(scenarioId, projectId, name, parentID, ParentName))

          names(allScns) <- c("ScenarioID", "ProjectID", "ScenarioName", "ParentID", "ParentName")

          sheet <- merge(allScns, sheet, all.y = TRUE)
        }
      }
    }
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

# TODO consider moving helper function to helpers file 

assignPid <- function(args, sheetNames){
  if (sheetNames$scope == "project") {
    args[["pid"]] <- pid
  }
  if (is.element(sheetNames$scope, c("project", "scenario"))) {
    args[["pid"]] <- pid
  }
  if (sheetNames$scope == "scenario") {
    args[["sid"]] <- sid
  }
}
