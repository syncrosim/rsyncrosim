# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Adds a package to SyncroSim
#'
#' Adds a package to SyncroSim
#'
#' @param name Character string.  The name of the package to install from the online package server.
#' @param session Session.
#' @export
setGeneric('addPackage',function(name=NULL,session=NULL) standardGeneric('addPackage'))

#' @rdname addPackage
setMethod('addPackage', signature(name="character"), function(name,session) {
  if(is.null(session)){session=.session()}
  if((class(session)=="character")&&(session==SyncroSimNotFound(warn=F))){
    return(SyncroSimNotFound())
  }
  
  if (is.null(name)){
    stop("A package name is required.")
  }
  
  tt = command(args=list(install=name),session,program="SyncroSim.PackageManager.exe")
  return (tt)
}
)
