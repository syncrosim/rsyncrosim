# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Adds a package to SyncroSim
#'
#' Adds a package to SyncroSim
#'
#' @param filename Character string.  The path to a SyncroSim package file.
#' @param session Session.
#' @export
setGeneric('addPackageFile',function(filename=NULL,session=NULL) standardGeneric('addPackageFile'))

#' @rdname addPackageFile
setMethod('addPackageFile', signature(filename="character"), function(filename,session) {
  if(is.null(session)){session=.session()}
  if((class(session)=="character")&&(session==SyncroSimNotFound(warn=F))){
    return(SyncroSimNotFound())
  }
  
  if (is.null(filename)){
    stop("A file name is required.")
  }
  
  if (!file.exists(filename)){
    stop(paste0("Cannot find file: ", filename))
  }
  
  tt = command(args=list(finstall=filename),session,program="SyncroSim.PackageManager.exe")
  return(tt)
}
)
