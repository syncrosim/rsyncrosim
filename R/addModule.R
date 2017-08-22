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
  #x=mySsim
  #value=c("C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg","C:/Program Files/SyncroSim/1/CorePackages/dynmult.ssimpkg")
  #value="C:/Program Files/SyncroSim/1/CorePackages/stockflow.ssimpkg"
  
  if(is.null(session)){session=.session()}
  if((class(session)=="character")&&(session==SyncroSimNotFound(warn=F))){
    return(SyncroSimNotFound())
  }
  for(i in seq(length.out=length(filename))){
    #i=1
    cVal = filename[i]
    if(!file.exists(cVal)){
      stop(paste0("Cannot find ",cVal,"."))
    }
    tt = command(args=list(queue=cVal),session,program="SyncroSim.ModuleManager.exe")
  }
  tt = command(args=list(installqueue=NULL),session,program="SyncroSim.ModuleManager.exe")
  #session@datasheetNames = .datasheets(x,scope="all",refresh=T)
  return (tt)
}
)
