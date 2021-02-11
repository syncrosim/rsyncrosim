# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Save datasheet(s)
#'
#' Saves datasheets to a SsimLibrary/Project/Scenario.
#'
#' @details
#' ssimObject/project/scenario should identify a single ssimObject.
#'
#' If fileData !=NULL, each element of names(fileData) should correspond uniquely to at most one entry in data. If a name is not found in data the element will be ignored with a warning.
#' If names(fileData) are full filepaths, rsyncrosim will write each object to the corresponding path for subsequent loading by SyncroSim. Note this is generally more time-consuming because the files must be written twice.
#' If names(fileData) are not filepaths (faster, recommended), rsyncrosim will write each element directly to the appropriate SyncroSim input/output folders.
#' rsyncrosim will write each element of fileData directly to the appropriate SyncroSim input/output folders.
#' If fileData != NULL, data should be a dataframe, vector, or list of length 1, not a list of length >1.
#' 
#' About the 'append' argument:
#' 
#' \itemize{
#'   \item A datasheet is a VALIDATION SOURCE if its data can be used to validate column values in a different datasheet.
#'   \item The append argument will be ignored if the datasheet is a validation source and has a project scope.  In this case the data will be MERGED.
#' }
#' 
#' @param ssimObject SsimLibrary/Project/Scenario.
#' @param data A dataframe, named vector, or list of these. One or more datasheets to load.
#' @param name Character or vector of these. The name(s) of the datasheet(s) to be saved. If a vector of names is provided, then a list must be provided for the data argument. Names provided here will override those provided with data argument's list.
# @param project character or integer. Project name or id. Note integer ids are slightly faster.
# @param scenario character or integer. Project name or id. Note integer ids are slightly faster.
#' @param fileData Named list or raster stack. Names are file names (without paths), corresponding to entries in data. The elements are objects containing the data associated with each name. Currently only supports Raster objects as elements.
#' @param append Logical. If TRUE, the incoming data will be appended to the datasheet if possible.  Default TRUE for project/library-scope datasheets, and FALSE for scenario-scope datasheets. See 'details' for more information about this argument.
#' @param forceElements Logical. If FALSE (default) a single return message will be returned as a character string. Otherwise it will be returned in a list.
#' @param force Logical. If datasheet scope is project/library, and append=FALSE, datasheet will be deleted before loading the new data. This can also delete other definitions and results, so user will be prompted for approval unless force=TRUE.
#' @param breakpoint Set to TRUE when modifying datasheets in a breakpoint function.
#' @param import Logical. Set to TRUE to import the data after saving.
#' @param path Character.  An optional output path.
#' 
#' @return 
#' This function invisibly returns a vector or list of logical values for each input: `TRUE` upon success (i.e.successful save)
#' and `FALSE` upon failure.
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
    if (class(x) == "Scenario") {
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
  if ((class(data) != "list") | (class(data[[1]]) != "data.frame")) {
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
    if (class(cDat) != "data.frame") {
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
      if (class(cDat[[kk]]) == "factor") {
        cDat[[kk]] <- as.character(cDat[[kk]])
      }
    }

    # note deletions must happen before files are written.
    scope <- sheetNames$scope[sheetNames$name == cName]
    if (length(scope) == 0) {
      sheetNames <- datasheets(x, refresh = TRUE)
      scope <- sheetNames$scope[sheetNames$name == cName]
      if (length(scope) == 0) {
        stop("Name not found in datasheetNames")
      }
    }

    # if no fileData found and datasheet contains files, find the files
    if (is.null(fileData)) {
      # get info on sheet type
      tt <- command(c("list", "columns", "csv", "allprops", paste0("lib=", .filepath(x)), paste0("sheet=", name)), .session(x))
      sheetInfo <- .dataframeFromSSim(tt)
      if (sum(grepl("isExternalFile^True", sheetInfo$properties, fixed = TRUE)) > 0) {
        sheetInfo$isFile <- grepl("isRaster^True", sheetInfo$properties, fixed = TRUE)
      } else {
        sheetInfo$isFile <- grepl("isExternalFile^Yes", sheetInfo$properties, fixed = TRUE)
        # NOTE: this should be isExternalFile - but the flag is set to true even for non-files
      }

      sheetInfo <- subset(sheetInfo, isFile)

      sheetInfo <- subset(sheetInfo, is.element(name, names(cDat)))
      # if(nrow(sheetInfo)>0){
      #   for(kk in seq(length.out=nrow(sheetInfo))){
      #     cCol = sheetInfo$name[kk]
      #     for(ll in seq(length.out=nrow(cDat))){
      #         if (!is.na(cDat[[cCol]][ll]) && (basename(cDat[[cCol]][ll]) == cDat[[cCol]][ll])) {
      #         cDat[[cCol]][ll] = paste0(tempdir(),"/",cDat[[cCol]][ll]) # changed to tempdir()
      #         if(!file.exists(cDat[[cCol]][ll])){
      #           stop("File ", cDat[[cCol]][ll]," not found.")
      #         }
      #       }
      #     }
      #   }
      # }
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
        if (!class(cItem) == "RasterLayer") {
          stop("rsyncrosim currently only supports Raster layers as elements of fileData.")
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

        raster::writeRaster(cItem, cOutName, format = "GTiff", overwrite = TRUE)
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
    
    if (out[[cName]] == "saved"){
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
