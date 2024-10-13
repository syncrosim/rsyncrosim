# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

setMethod(
  f = "initialize", signature = "SsimLibrary",
  definition = function(.Object, name = NULL, packages = NULL, session = NULL, 
                        forceUpdate = FALSE, overwrite = FALSE, useConda = NULL) {
    
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

    if (is.null(name)) {
      e <- ssimEnvironment()
      name <- e$LibraryFilePath
    }

    if (is.na(name)) {
      stop("A library name is required.")
    }

    packageOptions <- .packages(session, installed = TRUE)

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
      
      pathBits <- strsplit(path, "/")[[1]]
      dir.create(paste(head(pathBits, -1), collapse = "/"), showWarnings = FALSE)
      
      # Create an empty library
      args <- list(create = NULL, library = NULL, name = path)
      cStatus <- command(args, session)
      if (cStatus[1] != "saved") {
        stop("Problem creating library: ", cStatus[1])
      }
    }
    
    # TODO: what exactly is below doing...
    # ensure the package specified matches the package on disk
    args <- c("list", "datasheets", "csv", paste0("lib=", path))
    tt <- command(args, session)
    
    if (grepl("Could not find package", tt[[1]])) {
      stop(paste(tt[[1]], "Use installPackage() to install this package."))
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
    
    if (!is.null(useConda)){
      if (useConda == FALSE){
        tt <- command(list(setprop = NULL, lib = path, useconda = "no"), session)
      } else {
        tt <- command(list(setprop = NULL, lib = path, useconda = "yes"), session)
      } 
    }

    .Object@session <- session
    .Object@filepath <- path
    .Object@datasheetNames <- datasheets
    return(.Object)
  }
)

setGeneric(".ssimLibrary", 
           function(name = NULL, packages = NULL, session = NULL, 
                    forceUpdate = FALSE, overwrite = FALSE, 
                    useConda = NULL) standardGeneric(".ssimLibrary"))

setMethod(".ssimLibrary", signature(name = "missingOrNULLOrChar"), 
          function(name, packages, session, forceUpdate, overwrite, useConda) {
  return(new("SsimLibrary", name, packages, session))
})

setMethod(".ssimLibrary", signature(name = "SsimObject"), 
          function(name, packages, session, forceUpdate, overwrite, useConda) {

  if (is(name, "SsimLibrary")) {
    out <- name
  } else {
    out <- .ssimLibrary(name = .filepath(name), packages, 
                        session = .session(name), forceUpdate, 
                        overwrite, useConda)
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
#' @param packages character or character vector. The SyncroSim Package(s) to
#'  add to the Library if creating a new Library (optional)
#' @param session \code{\link{Session}} object. If \code{NULL} (default), session()
#'  will be used
#' @param forceUpdate logical. If \code{FALSE} (default) user will be prompted to approve 
#'  any required updates. If \code{TRUE}, required updates will be applied silently.
#' @param overwrite logical. If \code{TRUE} an existing SsimLibrary will be overwritten
#' @param useConda logical. If set to TRUE, then all packages associated with the 
#'  Library will have their Conda environments created and Conda environments will
#'  be used during runtime.If set to FALSE, then no packages will have their 
#'  Conda environments created and Conda environments will not be used during runtime.
#'  Default is NULL
#' 
#' @return 
#' Returns a \code{\link{SsimLibrary}} object.
#' 
#' @details  
#' Example arguments:
#' \itemize{
#'   \item If name is SyncroSim Project or Scenario: Returns the 
#'          \code{\link{SsimLibrary}} associated with the Project or Scenario.
#'   \item If name is \code{NULL}: Create/open a SsimLibrary in the current working 
#'          directory with the filename SsimLibrary.ssim.
#'   \item If name is a string: If string is not a valid path treat as filename 
#'          in working directory. If no file suffix provided in string then add 
#'          .ssim. Attempts to open a SsimLibrary of that name. If SsimLibrary does not 
#'          exist creates a SsimLibrary of type package in the current working directory.
#'   \item If given a name and a package: Create/open a SsimLibrary called [name].ssim. 
#'          Returns an error if the SsimLibrary already exists but is a different type 
#'          of package.
#' }
#' 
#' @examples
#' \dontrun{
#' # Make sure packages are installed
#' installPackage("stsim")
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
#' # Create SsimLibrary from template
#' installPackage("helloworldSpatial")
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(tempdir(), "mylib"), 
#'                          session = mySession,
#'                          forceUpdate = TRUE,
#'                          packages = "helloworldSpatial",
#'                          overwrite = TRUE)
#'                          
#' }
#' 
#' @export
setGeneric("ssimLibrary", 
           function(name = NULL, summary = NULL, packages = NULL, session = NULL, 
                    forceUpdate = FALSE, overwrite = FALSE, 
                    useConda = NULL) standardGeneric("ssimLibrary"))

#' @rdname ssimLibrary
setMethod("ssimLibrary", signature(name = "SsimObject"), 
          function(name, summary, packages, session, forceUpdate, 
                   overwrite, useConda) {
            
  if (is(name, "SsimLibrary")) {
    out <- name
    if (is.null(summary)) {
      summary <- TRUE
    }
  } else {
    out <- .ssimLibrary(name = .filepath(name), packages,
                        session = .session(name), forceUpdate, 
                        overwrite, useConda)
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
setMethod("ssimLibrary", signature(name = "missingOrNULLOrChar"), 
          function(name = NULL, summary = NULL, packages, session, forceUpdate, 
                   overwrite, useConda) {
          
  if (is.null(session)) {
    session <- .session()
  }
            
  if ((is(session, "character")) && (is(session, SyncroSimNotFound(warn = FALSE)))) {
    return(SyncroSimNotFound())
  }

  newLib <- new("SsimLibrary", name, packages, session, forceUpdate, overwrite, useConda)
  
  # Add specified packages to the library
  packageOptions <- .packages(session, installed = TRUE)
  addedPackages <- .packages(newLib)
  
  # Check to make sure status of added packages is "OK"
  for (i in 1:nrow(addedPackages)){
    pkgRow <- addedPackages[i,]
    if (pkgRow$status != "OK"){
      warning(paste0(
        "The following package associated with this library is not properly installed: ", 
        pkgRow$name, " v", pkgRow$version, "\n\tUse installPackage(packages = '", 
        pkgRow$name, "', versions = '", pkgRow$version, "') to install."))
    }
  }
  
  if (!is.null(packages)) {
    for (pkg in packages){
      if (!is.null(pkg) && !is.element(pkg, packageOptions$name)) {
        stop(paste(pkg, "not currently installed. Use packages(session, installed = TRUE) to see options."))
      }
      if (pkg %in% addedPackages$name){
        next
      }
      addPackage(newLib, pkg)
    }
  }
  
  if (!is.null(summary) && summary) {
    return(info(newLib))
  }
  return(newLib)
})
