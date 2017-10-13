# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Add module
#'
#' Add module or modules to SyncroSim
#'
#' @param filename Character string or vector of these. The path to an .ssimpkg file on disk, or a vector of filepaths.
#' @param session Session.
#' @export
setGeneric('addModule',function(filename,session=NULL) standardGeneric('addModule'))
#' @rdname addModule
setMethod('addModule', signature(filename="character"), function(filename,session) {
  if(is.null(session)){session=.session()}
  if((class(session)=="character")&&(session==SyncroSimNotFound(warn=F))){
    return(SyncroSimNotFound())
  }
  for(i in seq(length.out=length(filename))){
    cVal = filename[i]
    if(!file.exists(cVal)){
      stop(paste0("Cannot find ",cVal,"."))
    }
    tt = command(args=list(queuepackage=cVal),session,program="SyncroSim.ModuleManager.exe")
  }
  tt = command(args=list(installqueue=NULL),session,program="SyncroSim.ModuleManager.exe")
  return (tt)
}
)
