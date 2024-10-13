# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Add SyncroSim package(s)
#'
#' Adds package(s) to a \code{\link{SsimLibrary}}.
#'
#' @param ssimLibrary \code{\link{SsimLibrary}} object
#' @param packages character string or vector of package name(s)
#' @param versions character string or vector of package version(s). If 
#' \code{NULL} then uses the latest installed version of the package
#' @param forceUpdate logical. If \code{FALSE} (default) user will be prompted 
#'  to approve any required updates. If \code{TRUE}, required updates will 
#'  be applied silently.
#' 
#' @return
#' Invisibly returns \code{TRUE} upon success (i.e.successful addition 
#' of the package) or \code{FALSE} upon failure.
#' 
#' @seealso 
#' \code{\link{packages}}
#' 
#' @examples
#' \donttest{
#' # Install "stsim" and "stsimecodep" SyncroSim packages
#' installPackage(packages = c("stsim", "stsim"),
#'                versions = c("4.0.0", "4.0.1"))
#' installPackage("stsimecodep")
#' 
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' 
#' # Add package
#' addPackage(myLibrary, packages = "stsim", versions = "4.0.1")
#' addPackage(myLibrary, packages = "stsimecodep")
#' packages(myLibrary)
#' 
#' # Change package version
#' addPackage(myLibrary, packages = "stsim", versions = "4.0.0")
#' addPackage(myLibrary, packages = "stsim", versions = "4.0.1")
#' 
#' # Remove package
#' removePackage(myLibrary, packages = c("stsim", "stsimecodep"))
#' packages(myLibrary)
#' }
#' 
#' @export
setGeneric("addPackage", 
           function(ssimLibrary, packages, versions = NULL, 
                    forceUpdate = FALSE) standardGeneric("addPackage"))

#' @rdname addPackage
setMethod("addPackage", signature(ssimLibrary = "character"), 
          function(ssimLibrary, packages, versions, forceUpdate) {
  return(SyncroSimNotFound(ssimLibrary))
})

#' @rdname addPackage
setMethod("addPackage", signature(ssimLibrary = "SsimLibrary"), 
          function(ssimLibrary, packages, versions, forceUpdate) {
  
  # Check that list of packages and list of versions is same length
  if (!is.null(versions) && (length(versions) != length(packages))){
    stop("The number of versions supplied does not match the number of packages.")
  }
  
  sessionPkgs <- .packages(.session(ssimLibrary), installed = T)
  libraryPkgs <- .packages(ssimLibrary)
  retList <- list()
  schemaUpdate <- FALSE
  
  for (i in seq(length.out = length(packages))) {
    cPkg <- packages[i]
    
    newVersion <- "0.0.0"
    if (is.null(versions)){
      pkgVersions <- sessionPkgs[sessionPkgs$name == cPkg, ]$version
      if (length(pkgVersions) > 0){
        newVersion <- pkgVersions[length(pkgVersions)]
      }
    } else {
      newVersion <- versions[i]
    }
    
    # Check if another version of the package is already installed
    libPkgRow <- libraryPkgs[libraryPkgs$name == cPkg,]
    currentVersion <- libPkgRow$version
    
    if (nrow(libPkgRow) == 1){
      if (currentVersion == newVersion){
        print(paste0(cPkg, " v", newVersion, " has already been added to the ssimLibrary"))
        retList[[cPkg]] <- FALSE
        next  
      }
      
      # Determine whether a schema update is required
      currentMajor <- strsplit(currentVersion, ".", fixed = T)[[1]][1]
      currentMinor <- strsplit(currentVersion, ".", fixed = T)[[1]][2]
      newMajor <- strsplit(newVersion, ".", fixed = T)[[1]][1]
      newMinor <- strsplit(newVersion, ".", fixed = T)[[1]][2]
      
      if (newMajor > currentMajor){
        schemaUpdate <- TRUE
      } else if ((newMajor == currentMajor) && (newMinor > currentMinor)) {
        schemaUpdate <- TRUE
      }
      
      if (schemaUpdate && !forceUpdate){
          message(paste0("Updating this package version will require updates ",
                         "to the library. \nDo you want to update this library?"))
          answer <- readline(prompt = "(y/n):")
          
          if (answer == "n") {
            stop("Cannot change package version in library.")
        }
      }
    }
    
    sessPkgRow <- sessionPkgs[((sessionPkgs$name == cPkg) & (sessionPkgs$version == newVersion)), ]
    
    if (nrow(sessPkgRow) == 0) {
      print(paste0("Package ", cPkg, " v", newVersion, 
                   " is not among the available packages."))
      retList[[cPkg]] <- FALSE
      next
    }

    tt <- command(list(add = NULL, package = NULL, lib = .filepath(ssimLibrary), 
                       pkg = cPkg, ver = newVersion), .session(ssimLibrary))
    
    if (tt[1] == "saved"){
      message(paste0("Package <", cPkg, " v", newVersion, "> added"))
      retList[[cPkg]] <- TRUE
    } else {
      message(tt)
      retList[[cPkg]] <- FALSE
    }
  }
  
  if (schemaUpdate) {

      UpdateArgs <- list(update = NULL, lib = filepath(ssimLibrary))
      
      if (backupEnabled(filepath(ssimLibrary))) {
        UpdateArgs <- c(UpdateArgs, list(backup = NULL))
      }
      
      updateMessage <- command(UpdateArgs, session(ssimLibrary))
      updateMessage <- paste(updateMessage, collapse = " ")
      
      if (grepl("Update complete", updateMessage, fixed = TRUE)) {
        updateMessage <- "saved"
      }
      
      if (!identical(updateMessage, "saved")) {
        stop(updateMessage)
      }

  }

  return(invisible(retList))
})
