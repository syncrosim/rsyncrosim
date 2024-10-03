# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Adds package to SyncroSim Installation
#'
#' This function installs a package to the SyncroSim \code{\link{Session}}.
#' If only the package name is provided as input, the function queries the 
#' SyncroSim package server for the specified package. If a file path is 
#' provided as input, the function installs a package to SyncroSim from a local 
#' package file (ends in ".ssimpkg"). The list of SyncroSim packages can be 
#' found \href{https://syncrosim.com/packages/}{here}.
#'
#' @param packages character string. The name or file path of the package to 
#' install
#' @param versions character string. The packages version(s) to install if
#' installing a package from the server. If \code{NULL} then installs the 
#' latest version 
#' @param session \code{\link{Session}} object. If \code{NULL} (default),
#' \code{session()} will be used
#' 
#' @return 
#' Invisibly returns \code{TRUE} upon success (i.e.successful 
#' install) and \code{FALSE} upon failure.
#' 
#' @examples
#' \dontrun{
#' # Create a new SyncroSim Session
#' mySession <- session()
#' 
#' # Install package from the package server
#' installPackage(packages="stsim", versions="4.0.1", session = mySession)
#' 
#' # Install package using a local file path
#' installPackage("c:/path/to/stsim.ssimpkg")
#' }
#' 
#' @export
setGeneric("installPackage", 
           function(packages, versions = NULL, session = NULL) standardGeneric("installPackage"))

#' @rdname installPackage
setMethod("installPackage", signature(session = "character"), 
          function(packages, versions, session) {
  return(SyncroSimNotFound(session))
})

#' @rdname installPackage
setMethod("installPackage", signature(session = "missingOrNULL"), 
          function(packages, versions, session) {
  session <- .session()
  return(installPackage(packages, versions, session))
})

#' @rdname installPackage
setMethod("installPackage", signature(session = "Session"), 
          function(packages, versions, session) {
            
  success <- FALSE
  progName <- "SyncroSim.PackageManager.exe"
  
  if (is.null(packages)) {
    stop("A package name or file path is required")
  }
  
  if (!is.null(versions) && (length(packages) != length(versions))){
    stop("The number of versions specified must match the number of packages.")
  }
  
  # Install from file
  if (length(packages) == 1 && grepl(".ssimpkg", packages)) {
    if (!file.exists(packages)) {
      tt <- paste0("Cannot find file: ", packages)
    } else {
      tt <- command(args = list(finstall = packages), session, program = progName)
      if (tt == "saved"){
        success <- TRUE
        tt <- paste0("Package installed from file <", packages, ">")
      }
    }
    
    return(invisible(success))
  } 
  
  # Install from folder
  if (length(packages) == 1 && dir.exists(packages)){
    if (!file.exists(file.path(packages, "package.xml"))){
      tt <- paste0("Package folder is not valid")
    }
    tt <- command(args = list(xinstall = packages), session, program = progName)
    if (tt[1] == "saved"){
      success <- TRUE
      tt <- paste0("Package installed from folder <", packages, ">")
    }
    
    return(invisible(success))
  } 
  
  # Install from server
  availSessionPkgs <- .packages(session, installed = F)
  installedSessionPkgs <- .packages(session, installed = T)
  retList <- list()
  
  for (i in seq(length.out = length(packages))) {
    cPkg <- packages[i]
    
    cVer <- "0.0.0"
    if (is.null(versions)){
      pkgVersions <- availSessionPkgs[availSessionPkgs$name == cPkg, ]$version
      if (length(pkgVersions) > 0){
        cVer <- pkgVersions[length(pkgVersions)]
      }
    } else {
      cVer <- versions[i]
    }
    
    sessPkgRow <- installedSessionPkgs[
      ((installedSessionPkgs$name == cPkg) & (installedSessionPkgs$version == cVer)),]
    
    if (nrow(sessPkgRow) > 0) {
      print(paste0("Package ", cPkg, " v", cVer, " is already installed."))
      retList[[cPkg]] <- FALSE
      next
    }
  
    sessPkgRow <- availSessionPkgs[
      ((availSessionPkgs$name == cPkg) & (availSessionPkgs$version == cVer)), ]
    
    if (nrow(sessPkgRow) == 0) {
      print(paste0("Package ", cPkg, " v", cVer, " is not available from package server."))
      retList[[cPkg]] <- FALSE
      next
    }
    
    tt <- command(list(install = cPkg, version = cVer), session, program = progName)
    
    if (tt[1] == "saved"){
      message(paste0("Package <", cPkg, " v", cVer, "> installed"))
      retList[[cPkg]] <- TRUE
    } else {
      message(tt)
      retList[[cPkg]] <- FALSE
    }
    
    return(invisible(retList))
  }
})
