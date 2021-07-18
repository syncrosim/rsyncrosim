# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

setMethod(
  f = "initialize", signature = "SsimLibrary",
  definition = function(.Object, name = NULL, package = NULL, session = NULL, addon = NULL, template = NULL, forceUpdate = FALSE, overwrite = FALSE) {
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

    packageOptions <- package(session, installed = "BASE")

    if (nrow(packageOptions) == 0) {
      stop("No base packages are installed.  Use addPackage() to install a package.")
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
        deleteLibrary(path, force = TRUE)
      }
    }

    # If library does not exist, create it
    if (!file.exists(path)) {
      
      if (is.null(package)){
        package <- "stsim"        
      }

      if (!is.element(package, packageOptions$name)) {
        stop(paste(package, "not a base package. Use package(installed = \"BASE\") to see options."))
      }
      
      pathBits <- strsplit(path, "/")[[1]]
      dir.create(paste(head(pathBits, -1), collapse = "/"), showWarnings = FALSE)

      if (!exists("packageOptions")) {
        packageOptions <- package(session, installed = "BASE")
      }
      
      # If no template specified, create an empty library
      if (is.null(template)) {
        args <- list(create = NULL, library = NULL, name = path, package = packageOptions$name[packageOptions$name == package])
        cStatus <- command(args, session)
        if (cStatus[1] != "saved") {
          stop("Problem creating library: ", cStatus[1])
        }
      }
      
      # If template specified, create library from template
      if (is.character(template)) {
        
        # Check if template exists first
        args <- list(list = NULL, templates = NULL,
                     package = packageOptions$name[packageOptions$name == package],
                     csv = NULL)
        tt <- command(args, session)
        tempsDataframe <- read.csv(text = tt)
        if (template %in% tempsDataframe$Name == FALSE) {
          stop(paste(template, "does not exist for package",
                     packageOptions$name[packageOptions$name == package]))
        } else {
          
          # Load template
          args <- list(create = NULL, library = NULL, name = path,
                       package = packageOptions$name[packageOptions$name == package],
                       template = template)
          cStatus <- command(args, session)
          if (grepl(cStatus[1], "Creating Library from Template")) {
            stop("Problem creating library: ", cStatus[1])
          }
          
          # Print out available scenarios for the template
          args <- list(list = NULL, scenarios = NULL, lib = path, csv = NULL)
          tt <- command(args, session)
          tempScenarios <- read.csv(text = tt)
          message(paste(c("Scenarios available in this template:",
                        tempScenarios$Name), collapse = "    "))
        }
      } 
      
      if (!is.null(template) & !is.character(template)) {
        stop(paste(template, "is not a valid template name"))
      }
    }

    # ensure the base package specified matches the base package on disk
    args <- c("list", "datasheets", "csv", paste0("lib=", path))
    tt <- command(args, session)

    if (grepl("Could not find package", tt[[1]])) {
      stop(paste(tt[[1]], "Use addPackage() to install this package."))
    }

    if (grepl("The library has unapplied updates", tt[[1]])) {
      if (is.null(inName) | forceUpdate) {
        answer <- "y"
      } else {
        message("The library has unapplied updates.\nDo you want to update library with path '", path, "' ?")
        answer <- readline(prompt = "(y/n):")
      }
      if (answer == "y") {
        UpdateArgs <- list(update = NULL, lib = path)

        if (backupEnabled(path)) {
          UpdateArgs <- c(UpdateArgs, list(backup = NULL))
        }

        updateMessage <- command(UpdateArgs, session)
        updateMessage <- paste(updateMessage, collapse = " ")

        if (grepl("Update complete", updateMessage, fixed = TRUE)) {
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
        packageOptions <- package(session, installed = "BASE")
      }
      expectedPackage <- packageOptions$name[packageOptions$name == package]
      if (!grepl(expectedPackage, tt$value[tt$property == "Package Name:"])) {
        stop(paste0("A library of that name and a different package type ", tt$value[tt$property == "Name:"], " already exists."))
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

setGeneric(".ssimLibrary", function(name = NULL, package = NULL, session = NULL, addon = NULL, template = NULL, forceUpdate = FALSE, overwrite = FALSE) standardGeneric(".ssimLibrary"))

setMethod(".ssimLibrary", signature(name = "missingOrNULLOrChar"), function(name, package, session, addon, template, forceUpdate, overwrite = FALSE) {
  return(new("SsimLibrary", name, package, session, addon, forceUpdate))
})

setMethod(".ssimLibrary", signature(name = "SsimObject"), function(name, package, session, addon, template, forceUpdate, overwrite) {
  if (class(name) == "SsimLibrary") {
    out <- name
  } else {
    out <- .ssimLibrary(name = .filepath(name), package, session = .session(name), addon, template, forceUpdate, overwrite)
  }
  return(out)
})

#' Create or open a library
#'
#' Creates or opens a \code{\link{SsimLibrary}} object.
#' If summary = TRUE, returns library summary info.
#' If summary = NULL, returns library summary info if ssimObject is a SsimLibrary, 
#' SsimLibrary object otherwise.
#' 
#' @param name Character string or object of class \code{\link{SsimLibrary}}, 
#'     \code{\link{Project}} or \code{\link{Scenario}} The path to a library 
#'     or SsimObject.
#' @param summary Logical. Default TRUE.
#' @param package Character. The package type. The default is "stsim".
#' @param session Session. If NULL, session() will be used.
#' @param addon Character or character vector. One or more addons. See \code{\link{addon}}
#'     for options.
#' @param template Character. Creates the library with the specified template.
#' @param forceUpdate Logical. If FALSE (default) user will be prompted to approve 
#'     any required updates. If TRUE, required updates will be applied silently.
#' @param overwrite Logical. If TRUE an existing Library will be overwritten.
#' 
#' @return 
#' Returns a \code{\link{SsimLibrary}} object.
#' 
#' @details
#'
#' \itemize{
#'   \item {If name is SyncroSim Project or Scenario: }{Returns the 
#'          \code{\link{SsimLibrary}} associated with the Project or Scenario.}
#'   \item {If name is NULL: }{Create/open a SsimLibrary in the current working 
#'          directory with the filename SsimLibrary.ssim.}
#'   \item {If name is a string: }{If string is not a valid path treat as filename 
#'          in working directory. If no file suffix provided in string then add 
#'          .ssim. Attempts to open a library of that name. If library does not 
#'          exist creates a library of type package in the current working directory.}
#'   \item {If given a name and a package: }{Create/open a library called <name>.ssim. 
#'          Returns an error if the library already exists but is a different type 
#'          of package.}
#' }
#' 
#' @examples
#' \donttest{
#' # Create or open a library using the default session
#' myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"))
#'
#' # Create library using a specific session
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"),
#'                          session = mySession)
#'
#' session(myLibrary)
#' filepath(myLibrary)
#' info(myLibrary)
#' 
#' # Load a library with add on
#' myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"),
#'                          overwrite = TRUE, package = "stsim",
#'                          addon = "stsimsf")
#' 
#' # Create library from template
#' addPackage("helloworldEnhanced")
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"), 
#'                          session = mySession,
#'                          package = "helloworldEnhanced",
#'                          template = "example-library",
#'                          overwrite = TRUE)
#' }
#' 
#' @export
setGeneric("ssimLibrary", function(name = NULL, summary = NULL, package = NULL, session = NULL, addon = NULL, template = NULL, forceUpdate = FALSE, overwrite = FALSE) standardGeneric("ssimLibrary"))

#' @rdname ssimLibrary
setMethod("ssimLibrary", signature(name = "SsimObject"), function(name, summary, package, session, addon, template, forceUpdate, overwrite) {
  if (class(name) == "SsimLibrary") {
    out <- name
    if (is.null(summary)) {
      summary <- TRUE
    }
  } else {
    out <- .ssimLibrary(name = .filepath(name), package, session = .session(name), addon, template, forceUpdate, overwrite)
    if (is.null(summary)) {
      summary <- FALSE
    }
  }
  if (!summary) {
    return(out)
  }
  return(info(out))
})

#' @rdname ssimLibrary
setMethod("ssimLibrary", signature(name = "missingOrNULLOrChar"), function(name = NULL, summary = NULL, package, session, addon, template, forceUpdate, overwrite) {
  if (is.null(session)) {
    session <- .session()
  }
  if ((class(session) == "character") && (session == SyncroSimNotFound(warn = FALSE))) {
    return(SyncroSimNotFound())
  }

  newLib <- new("SsimLibrary", name, package, session, addon, template, forceUpdate, overwrite)
  if (!is.null(summary) && summary) {
    return(info(newLib))
  }
  return(newLib)
})
