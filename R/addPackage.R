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
#' installPackage("stsim")
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
#' # Remove package
#' removePackage(myLibrary, packages = c("stsim", "stsimecodep"))
#' packages(myLibrary)
#' }
#' 
#' @export
setGeneric("addPackage", 
           function(ssimLibrary, packages, versions = NULL) standardGeneric("addPackage"))

#' @rdname addPackage
setMethod("addPackage", signature(ssimLibrary = "character"), 
          function(ssimLibrary, packages, versions) {
  return(SyncroSimNotFound(ssimLibrary))
})

#' @rdname addPackage
setMethod("addPackage", signature(ssimLibrary = "SsimLibrary"), 
          function(ssimLibrary, packages, versions) {
  
  # Check that list of packages and list of versions is same length
  if (!is.null(versions) && (length(versions) != length(packages))){
    stop("The number of versions supplied does not match the number of packages.")
  }
  browser()
  sessionPkgs <- .packages(.session(ssimLibrary), installed = T)
  libraryPkgs <- .packages(ssimLibrary)
  retList <- list()
  
  for (i in seq(length.out = length(packages))) {
    cPkg <- packages[i]
    
    cVer <- "0.0.0"
    if (is.null(versions)){
      pkgVersions <- sessionPkgs[sessionPkgs$name == cPkg, ]$version
      if (length(pkgVersions) > 0){
        cVer <- pkgVersions[length(pkgVersions)]
      }
    } else {
      cVer <- versions[i]
    }
    
    # Check if another version of the package is already installed
    libPkgRow <- libraryPkgs[libraryPkgs$name == cPkg,]
    
    if ((nrow(libPkgRow) == 1) & (libPkgRow$version != cVer)){
      .removePackage(ssimLibrary, cPkg)
    }
    
    # If the same version of the package is already installed, then skip
    libPkgRow <- libraryPkgs[((libraryPkgs$name == cPkg) & (libraryPkgs$version == cVer)), ]
  
    if (nrow(libPkgRow) > 0){
      print(paste0(cPkg, " v", cVer, " has already been added to the ssimLibrary"))
      retList[[cPkg]] <- FALSE
      next
    }
    
    sessPkgRow <- sessionPkgs[((sessionPkgs$name == cPkg) & (sessionPkgs$version == cVer)), ]
    
    if (nrow(sessPkgRow) == 0) {
      print(paste0("Warning - ", cPkg, " v", cVer, " is not among the available packages: ", 
                   paste(sessionPkgs$name, collapse = ",")))
      retList[[cPkg]] <- FALSE
      next
    }

    tt <- command(list(add = NULL, package = NULL, lib = .filepath(ssimLibrary), 
                       pkg = cPkg, version = cVer), .session(ssimLibrary))
    
    if (tt[1] == "saved"){
      message(paste0("Package <", cPkg, " v", cVer, "> added"))
      retList[[cPkg]] <- TRUE
    } else {
      message(tt)
      retList[[cPkg]] <- FALSE
    }
  }

  return(invisible(retList))
})
