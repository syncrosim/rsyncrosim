# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Removes a package from SyncroSim installation
#' 
#' @param packages character or character vector. The name(s) of the package(s) 
#' to uninstall
#' @param versions character or character vector. The version(s) of the 
#' package(s) to uninstall. If \code{NULL} then will uninstall all versions of 
#' the package(s).
#' @param session \code{\link{Session}} object. If \code{NULL} (default), 
#' \code{session()} will be used
#' 
#' @return 
#' Invisibly returns \code{TRUE} upon success (i.e.successful 
#' removal) and \code{FALSE} upon failure.
#' 
#' @examples 
#' \dontrun{
#' # Set SyncroSim session
#' mySession <- session()
#' 
#' # Install packages to SyncroSim session
#' installPackages(packages = c("stsim", "stsim"),
#'                 versions = c("4.0.0", "4.0.1"))
#' 
#' # Uninstalls specific version of package from SyncroSim session
#' uninstallPackage(packages = "stsim", versions = "4.0.0", session = mySession)
#' 
#' # Uninstalls all instances ofa package from SyncroSim session
#' uninstallPackage(packages = "stsim", session = mySession)
#' }
#' 
#' @export
setGeneric("uninstallPackage", 
           function(packages, versions = NULL, session = NULL) standardGeneric("uninstallPackage"))

#' @rdname uninstallPackage
setMethod("uninstallPackage", signature(session = "character"), 
          function(packages, versions, session) {
  return(SyncroSimNotFound(session))
})

#' @rdname uninstallPackage
setMethod("uninstallPackage", signature(session = "missingOrNULL"), 
          function(packages, versions, session) {
  session <- .session(session)
  return(uninstallPackage(packages, versions, session))
})

#' @rdname uninstallPackage
setMethod("uninstallPackage", signature(session = "Session"), 
          function(packages, versions, session) {
            
  installed <- .packages(session, installed = T)
  retList <- list()
  
  for (i in length(packages)){
    
    cPkg <- packages[i]
    
    if (!is.element(cPkg, installed$name)) {
      message(paste0("The package <", cPkg, "> is not installed."))
    }
    
    installedPkgRows <- installed[installed$name == cPkg,]
    
    if (!is.null(versions)){
      cVer <- versions[i]
      
      if (!is.element(cVer, installedPkgRows$version)){
        message(paste0("The package <", cPkg, " v", cVer, "> is not installed."))
      }
    } else {
      cVer <- installedPkgRows$version
    }
    
    for (v in cVer){
      tt <- command(args = list(remove = cPkg, version = v), session, 
                    program = "SyncroSim.PackageManager.exe")
      
      if (tt[1] == "saved"){
        message(paste0("Package <", cPkg, " v", v, "> removed"))
        retList[[cPkg]] <- TRUE
      } else {
        message(tt)
        retList[[cPkg]] <- FALSE
      }
    }
  }
  
  return(invisible(retList))
})
