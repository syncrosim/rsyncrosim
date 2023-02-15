# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

setMethod(
  f = "initialize", signature = "SsimLibrary",
  definition = function(.Object, name = NULL, package = NULL, session = NULL, addon = NULL, template = NULL, forceUpdate = FALSE, overwrite = FALSE, useConda = NULL) {
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
        
        # Check if .ssim is appended to the template
        if (grepl(".ssim", template)) {
          template <- gsub(".ssim", "", template)
        }
        
        # Check if template exists first in base package
        args <- list(list = NULL, templates = NULL,
                     package = packageOptions$name[packageOptions$name == package],
                     csv = NULL)
        tt <- command(args, session)
        baseTempsDataframe <- read.csv(text = tt)
        baseTemplate <- paste0(package, "_", template)
        baseTemplateExists <- baseTemplate %in% baseTempsDataframe$Name
        
        if (!baseTemplateExists & !is.null(addon)) {
          allPackageOptions <- package(session)
          args <- list(list = NULL, templates = NULL,
                       package = allPackageOptions$name[allPackageOptions$name == addon],
                       csv = NULL)
          tt <- command(args, session)
          addonTempsDataframe <- read.csv(text = tt)
          addonTemplate <- paste0(addon, "_", template)
          addonTemplateExists <- addonTemplate %in% addonTempsDataframe
        } else {
          addonTemplateExists <- FALSE
        }
        
        if (baseTemplateExists) {
          template = baseTemplate
          tempPackage = package
        } else if (addonTemplateExists) {
          template = addonTemplate
          tempPackage = addon
        } else if (!is.null(addon)){
          stop(paste(template, "does not exist for package",
                     packageOptions$name[packageOptions$name == package],
                     "or addon", allPackageOptions$name[allPackageOptions$name == addon]))
        } else {
          stop(paste(template, "does not exist for package",
                     packageOptions$name[packageOptions$name == package]))
        }
          
        # Load template
        args <- list(create = NULL, library = NULL, name = path,
                     package = tempPackage,
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
    
    if (!is.null(useConda)){
      if (useConda == FALSE){
        tt <- command(list(setprop = NULL, lib = path, useconda = "no"), session)
      } else {
        tt <- command(list(setprop = NULL, lib = path, useconda = "yes"), session)
        if (useConda == TRUE){
          currentPackages <- unique(datasheets$package)
        }
        createCondaEnv(path, currentPackages, session)
      } 
    }

    .Object@session <- session
    .Object@filepath <- path
    .Object@datasheetNames <- datasheets
    return(.Object)
  }
)

setGeneric(".ssimLibrary", function(name = NULL, package = NULL, session = NULL, addon = NULL, template = NULL, forceUpdate = FALSE, overwrite = FALSE, useConda = NULL) standardGeneric(".ssimLibrary"))

setMethod(".ssimLibrary", signature(name = "missingOrNULLOrChar"), function(name, package, session, addon, template, forceUpdate, overwrite, useConda) {
  return(new("SsimLibrary", name, package, session, addon, forceUpdate))
})

setMethod(".ssimLibrary", signature(name = "SsimObject"), function(name, package, session, addon, template, forceUpdate, overwrite, useConda) {
  if (is(name, "SsimLibrary")) {
    out <- name
  } else {
    out <- .ssimLibrary(name = .filepath(name), package, session = .session(name), addon, template, forceUpdate, overwrite, useConda)
  }
  return(out)
})

#' Create or open a SsimLibrary
#'
#' @description 
#' Creates or opens a \code{\link{SsimLibrary}} object.
#' If \code{summary = TRUE}, returns SsimLibrary summary info.
#' If \code{summary = NULL}, returns SsimLibrary summary info if ssimObject is a SsimLibrary, 
#' SsimLibrary object otherwise.
#' 
#' @param name \code{\link{SsimLibrary}}, \code{\link{Project}} or 
#' \code{\link{Scenario}} object, or character string (i.e. path to a SsimLibrary 
#'     or SsimObject)
#' @param summary logical. Default is \code{TRUE}
#' @param package character. The package type. Default is "stsim"
#' @param session \code{\link{Session}} object. If \code{NULL} (default), session()
#'  will be used
#' @param addon character or character vector. One or more addon packages. See 
#' \code{\link{addon}} for options (optional)
#' @param template character. Creates the SsimLibrary with the specified template
#' (optional)
#' @param forceUpdate logical. If \code{FALSE} (default) user will be prompted to approve 
#'     any required updates. If \code{TRUE}, required updates will be applied silently
#' @param overwrite logical. If \code{TRUE} an existing SsimLibrary will be overwritten
#' @param useConda logical. If set to TRUE, then all packages associated with the 
#'  Library will have their Conda environments created and Conda environments will
#'  be used during runtime.If set to FALSE, then no packages will have their 
#'  Conda environments created and Conda environments will not be used during runtime.
#'  Default is FALSE.
#' 
#' @return 
#' Returns a \code{\link{SsimLibrary}} object.
#' 
#' @details  
#' Example arguments:
#' \itemize{
#'   \item {If name is SyncroSim Project or Scenario: }{Returns the 
#'          \code{\link{SsimLibrary}} associated with the Project or Scenario.}
#'   \item {If name is \code{NULL}: }{Create/open a SsimLibrary in the current working 
#'          directory with the filename SsimLibrary.ssim.}
#'   \item {If name is a string: }{If string is not a valid path treat as filename 
#'          in working directory. If no file suffix provided in string then add 
#'          .ssim. Attempts to open a SsimLibrary of that name. If SsimLibrary does not 
#'          exist creates a SsimLibrary of type package in the current working directory.}
#'   \item {If given a name and a package: }{Create/open a SsimLibrary called <name>.ssim. 
#'          Returns an error if the SsimLibrary already exists but is a different type 
#'          of package.}
#' }
#' 
#' @examples
#' \donttest{
#' # Make sure packages are installed
#' addPackage("stsim")
#' addPackage("stsimsf")
#' 
#' # Create or open a SsimLibrary using the default Session
#' myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"))
#'
#' # Create SsimLibrary using a specific Session
#' mySession <- session()
#' 
#' myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"),
#'                          session = mySession)
#'
#' # Retrieve SsimLibrary properties
#' session(myLibrary)
#' 
#' # Load a SsimLibrary with addon package
#' myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"),
#'                          overwrite = TRUE, package = "stsim",
#'                          addon = "stsimsf")
#' 
#' # Create SsimLibrary from template
#' addPackage("helloworldSpatial")
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"), 
#'                          session = mySession,
#'                          package = "helloworldSpatial",
#'                          template = "example-library",
#'                          overwrite = TRUE)
#'                          
#' }
#' 
#' @export
setGeneric("ssimLibrary", function(name = NULL, summary = NULL, package = NULL, session = NULL, addon = NULL, template = NULL, forceUpdate = FALSE, overwrite = FALSE, useConda = NULL) standardGeneric("ssimLibrary"))

#' @rdname ssimLibrary
setMethod("ssimLibrary", signature(name = "SsimObject"), function(name, summary, package, session, addon, template, forceUpdate, overwrite, useConda) {
  if (is(name, "SsimLibrary")) {
    out <- name
    if (is.null(summary)) {
      summary <- TRUE
    }
  } else {
    out <- .ssimLibrary(name = .filepath(name), package, session = .session(name), addon, template, forceUpdate, overwrite, useConda)
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
setMethod("ssimLibrary", signature(name = "missingOrNULLOrChar"), function(name = NULL, summary = NULL, package, session, addon, template, forceUpdate, overwrite, useConda) {
  if (is.null(session)) {
    session <- .session()
  }
  if ((is(session, "character")) && (is(session, SyncroSimNotFound(warn = FALSE)))) {
    return(SyncroSimNotFound())
  }

  newLib <- new("SsimLibrary", name, package, session, addon, template, forceUpdate, overwrite, useConda)
  if (!is.null(summary) && summary) {
    return(info(newLib))
  }
  return(newLib)
})
