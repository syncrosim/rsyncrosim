# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

setMethod(
  f = "initialize", signature = "SsimLibrary",
  definition = function(.Object, name = NULL, package = NULL, session = NULL, addon = NULL, forceUpdate = F, overwrite = F) {
    enabled <- NULL

    if (is.null(session)) {
      e <- ssimEnvironment()
      if (!is.na(e$ProgramDirectory)) {
        session <- .session(e$ProgramDirectory)
      } else {
        session <- .session()
      }
    }

    if (is.character(session)) {
      session <- .session(session)
    }

    inName <- name
    inPkg <- package

    if (is.null(name)) {
      e <- ssimEnvironment()
      name <- e$LibraryFilePath
    }

    if (is.na(name)) {
      stop("A library name is required.")
    }

    packageOptions <- basePackage(session)

    if (nrow(packageOptions) == 0) {
      stop("No base packages are installed.  Use addPackage() or addPackageFile() to install a package.")
    }

    if (identical(basename(name), name)) {
      path <- file.path(getwd(), name)
    } else {
      path <- name
    }

    if (!grepl(".ssim", path)) {
      path <- paste0(path, ".ssim")
    }

    if (overwrite) {
      if (file.exists(path)) {
        deleteLibrary(path, force = T)
      }
    }

    if (!file.exists(path)) {
      
      if (is.null(package)){
        package <- "stsim"        
      }

      if (!is.element(package, packageOptions$name)) {
        stop(paste(package, "not a base package. Use basePackage() to see options."))
      }
      
      pathBits <- strsplit(path, "/")[[1]]
      dir.create(paste(head(pathBits, -1), collapse = "/"), showWarnings = F)

      if (!exists("packageOptions")) {
        packageOptions <- basePackage(session)
      }
      args <- list(create = NULL, library = NULL, name = path, package = packageOptions$name[packageOptions$name == package])
      cStatus <- command(args, session)
      if (cStatus[1] != "saved") {
        stop("Problem creating library: ", cStatus[1])
      }
    }

    # ensure the base package specified matches the base package on disk
    args <- c("list", "datasheets", "csv", paste0("lib=", path))
    tt <- command(args, session)

    if (grepl("Could not find package", tt[[1]])) {
      stop(paste(tt[[1]], "Use addPackage() or addPackageFile() to install this package."))
    }

    if (grepl("The library has unapplied updates", tt[[1]])) {
      if (is.null(inName) | forceUpdate) {
        answer <- "y"
      } else {
        answer <- readline(prompt = paste0("The library has unapplied updates. Do you want to update ", path, "? (y/n): "))
      }
      if (answer == "y") {
        UpdateArgs <- list(update = NULL, lib = path)

        if (backupEnabled(path)) {
          UpdateArgs <- c(UpdateArgs, list(backup = NULL))
        }

        updateMessage <- command(UpdateArgs, session)
        updateMessage <- paste(updateMessage, collapse = " ")

        if (grepl("Update complete", updateMessage, fixed = T)) {
          updateMessage <- "saved"
        }

        if (!identical(updateMessage, "saved")) {
          stop(updateMessage)
        }
      } else {
        stop("Cannot open a library with unapplied updates.")
      }
      tt <- command(args, session)
    } else {
      if (grepl("The library ", tt[[1]])) {
        stop("Problem loading library: ", tt[1])
      }
    }

    datasheets <- .dataframeFromSSim(tt, convertToLogical = c("isOutput", "isSingle"))
    datasheets$scope <- sapply(datasheets$scope, camel)

    if (!is.null(inPkg)) {
      args <- list(list = NULL, library = NULL, csv = NULL, lib = path)
      tt <- command(args, session)
      tt <- .dataframeFromSSim(tt)
      if (ncol(tt) < 2) {
        stop(command(args, session))
      }

      if (!exists("packageOptions")) {
        packageOptions <- basePackage(session)
      }
      expectedPackage <- packageOptions$name[packageOptions$name == package]
      if (!grepl(expectedPackage, tt$value[tt$property == "Package Name:"])) {
        stop(paste0("A library of that name and a different package type ", tt$value[tt$property == "Package Name:"], " already exists."))
      }
    }

    if (!is.null(addon)) {
      tt <- command(list(list = NULL, addons = NULL, csv = NULL, lib = path), session)
      tt <- .dataframeFromSSim(tt)
      cAdds <- subset(tt, enabled == "Yes")
      addon <- setdiff(addon, cAdds$name)

      for (i in seq(length.out = length(addon))) {
        tt <- command(list(create = NULL, addon = NULL, lib = path, name = addon[i]), session)
        if (tt[[1]] != "saved") {
          stop("Problem with addon ", addon[i], ": ", tt[[1]])
        }
      }
    }

    .Object@session <- session
    .Object@filepath <- path
    .Object@datasheetNames <- datasheets
    return(.Object)
  }
)

setGeneric(".ssimLibrary", function(name = NULL, package = NULL, session = NULL, addon = NULL, forceUpdate = F, overwrite = F) standardGeneric(".ssimLibrary"))

setMethod(".ssimLibrary", signature(name = "missingOrNULLOrChar"), function(name, package, session, addon, forceUpdate, overwrite = F) {
  return(new("SsimLibrary", name, package, session, addon, forceUpdate))
})

setMethod(".ssimLibrary", signature(name = "SsimObject"), function(name, package, session, addon, forceUpdate, overwrite) {
  if (class(name) == "SsimLibrary") {
    out <- name
  } else {
    out <- .ssimLibrary(name = .filepath(name), package, session = .session(name), addon, forceUpdate, overwrite)
  }
  return(out)
})

#' Create or open a library.
#'
#' Creates or opens an \code{\link{SsimLibrary}} object.
#' If summary = T, returns library summary info.
#' If summary = NULL, returns library summary info if ssimObject is an SsimLibrary, SsimLibrary object otherwise.
#'
#' @export
#' @details
#'
#' \itemize{
#'   \item {If name is SyncroSim Project or Scenario: }{Returns the \code{\link{SsimLibrary}} associated with the Project or Scenario.}
#'   \item {If name is NULL: }{Create/open a SsimLibrary in the current working directory with the filename SsimLibrary.ssim.}
#'   \item {If name is a string: }{If string is not a valid path treat as filename in working directory. If no file suffix provided in string then add .ssim. Attempts to open a library of that name. If library does not exist creates a library of type package in the current working directory.}
#'   \item {If given a name and a package: }{Create/open a library called <name>.ssim. Returns an error if the library already exists but is a different type of package.}
#' }
#' @param name Character string, Project/Scenario/SsimLibrary. The path to a library or SsimObject.
#' @param summary logical. Default T
#' @param package Character. The package type. The default is "stsim".
#' @param session Session. If NULL, session() will be used.
#' @param addon Character or character vector. One or more addons. See addon() for options.
#' @param forceUpdate Logical. If FALSE (default) user will be prompted to approve any required updates. If TRUE, required updates will be applied silently.
#' @param overwrite Logical. If TRUE an existing Library will be overwritten.
#' @return An \code{SsimLibrary} object.
#' @examples
#' \donttest{
#' # Create a library using the default session
#' myLibrary <- ssimLibrary(name = "myLib")
#'
#' # Open a library using the default session
#' myLibrary <- ssimLibrary(name = "myLib")
#'
#' # Create library using a specific session
#' mySession <- session("C:/Downloads/SyncroSim")
#' myLibrary <- ssimLibrary(name = "myLib", session = mySession)
#'
#' session(myLibrary)
#' filepath(myLibrary)
#' info(myLibrary)
#' }
#' @export
setGeneric("ssimLibrary", function(name = NULL, summary = NULL, package = NULL, session = NULL, addon = NULL, forceUpdate = F, overwrite = F) standardGeneric("ssimLibrary"))

#' @rdname ssimLibrary
setMethod("ssimLibrary", signature(name = "SsimObject"), function(name, summary, package, session, addon, forceUpdate, overwrite) {
  if (class(name) == "SsimLibrary") {
    out <- name
    if (is.null(summary)) {
      summary <- T
    }
  } else {
    out <- .ssimLibrary(name = .filepath(name), package, session = .session(name), addon, forceUpdate, overwrite)
    if (is.null(summary)) {
      summary <- F
    }
  }
  if (!summary) {
    return(out)
  }
  return(info(out))
})

#' @rdname ssimLibrary
setMethod("ssimLibrary", signature(name = "missingOrNULLOrChar"), function(name = NULL, summary = NULL, package, session, addon, forceUpdate, overwrite) {
  if (is.null(session)) {
    session <- .session()
  }
  if ((class(session) == "character") && (session == SyncroSimNotFound(warn = F))) {
    return(SyncroSimNotFound())
  }

  newLib <- new("SsimLibrary", name, package, session, addon, forceUpdate, overwrite)
  if (!is.null(summary) && summary) {
    return(info(newLib))
  }
  return(newLib)
})

# Information about an library
#
# Get basic information about a Library.
# Use project(summary==T) and scenario(summary=T) to get similar information about Project/Scenario
#
# @param ssimLibrary SsimLibrary.
# @export
setGeneric("info", function(ssimLibrary) standardGeneric("info"))

setMethod("info", signature(ssimLibrary = "SsimLibrary"), function(ssimLibrary) {
  args <- list(list = NULL, library = NULL, csv = NULL, lib = .filepath(ssimLibrary))
  tt <- command(args, .session(ssimLibrary))
  out <- .dataframeFromSSim(tt, localNames = T)
  return(out)
})

# Delete Library - internal helper function
#
# Deletes a SyncroSim library. Note this is irreversable.
#
# @param ssimLibrary SsimLibrary or path to a library
# @param force Logical. If FALSE (default) prompt to confirm that the library should be deleted. This is irreversable.
# @return "saved" or failure message.
# @export
setGeneric("deleteLibrary", function(ssimLibrary, force = F) standardGeneric("deleteLibrary"))

setMethod("deleteLibrary", signature(ssimLibrary = "SsimLibrary"), function(ssimLibrary, force) {
  return(deleteLibrary(.filepath(ssimLibrary), force))
})

setMethod("deleteLibrary", signature(ssimLibrary = "character"), function(ssimLibrary, force) {
  if (!file.exists(ssimLibrary)) {
    return(paste0("Library not found: ", ssimLibrary))
  }
  if (force) {
    answer <- "y"
  } else {
    answer <- readline(prompt = paste0("Do you really want to delete library ", ssimLibrary, "? (y/n): "))
  }
  if (answer == "y") {
    unlink(ssimLibrary)
    if (file.exists(ssimLibrary)) {
      return(paste0("Failed to delete ", ssimLibrary))
    }

    unlink(paste0(ssimLibrary, ".backup"), recursive = T, force = T)
    unlink(paste0(ssimLibrary, ".input"), recursive = T, force = T)
    unlink(paste0(ssimLibrary, ".output"), recursive = T, force = T)
    unlink(paste0(ssimLibrary, ".temp"), recursive = T, force = T)

    return("saved")
  } else {
    return("skipped")
  }
})
