# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Deletes a package
#'
#' Deletes a package
#' @param name Character. The name of the package to delete.
#' @param session Session.
#' @param force logical. If T, delete without requiring confirmation from user.
#' @return "saved" or error message.
#' @export
setGeneric('deletePackage',function(name, session=NULL, force=F) standardGeneric('deletePackage'))

#' @rdname deletePackage
setMethod('deletePackage', signature(session="character"), function(name, session, force) {
  return(SyncroSimNotFound(session))
})

#' @rdname deletePackage
setMethod('deletePackage', signature(session="missingOrNULL"), function(name,session,force) {
  session=.session(session)
  return(deletePackage(name, session, force))
})

#' @rdname deletePackage
setMethod('deletePackage', signature(session="Session"), function(name, session, force) {
  
  installed = package(session)
  
  if (!is.element(name, installed$name)){
    stop("The package is not installed.")
  }
  
  if(force){
    answer="y"
  }else{
    answer <- readline(prompt=paste0("Do you really want to remove package '", name, "'? (y/n)"))
  }
  
  if(answer=="y"){
    tt = command(args=list(uninstall=name),session,program="SyncroSim.PackageManager.exe")
    return(tt)
  }else{
    return(NULL)
  }
})
