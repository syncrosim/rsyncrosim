# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Update Package
#' 
#' Updates a SyncroSim package
#'
#' @param name Character string.  The name of the package to update.  If NULL, all packages will be updated.
#' @param session Session.
#' @param listonly Logical.  If TRUE, available updates are listed only.
#' @export
setGeneric('updatePackage',function(name=NULL, session=NULL, listonly=F) standardGeneric('updatePackage'))

#' @rdname updatePackage
setMethod('updatePackage', signature(session="character"), function(name, session, listonly) {
  return(SyncroSimNotFound(session))
})

#' @rdname updatePackage
setMethod('updatePackage', signature(session="missingOrNULL"), function(name, session, listonly) {
  session=.session()
  return(updatePackage(name, session, listonly))
})

#' @rdname updatePackage
setMethod('updatePackage', signature(session="Session"), function(name, session, listonly) {
  
  if (listonly){
    tt = command(args="--updates", session, program="SyncroSim.PackageManager.exe")
    return (cat(tt, sep="\n"))
  }
  
  tt = NULL;
  
  if (is.null(name)){
    
    answer <- readline(prompt="Update all packages? (y/n)")
    
    if (answer == "y"){
      tt = command(args="--updateall --force", session, program="SyncroSim.PackageManager.exe")      
    }
    
  }else{
    
    installed = package(session)
    
    if (!is.element(name, installed$name)){
      tt = paste(name, ": The package is not installed.")
    }else{
      tt = command(args=list(updatepkg=name), session, program="SyncroSim.PackageManager.exe")      
    }
  }
  
  if (grepl("The remote name could not be resolved", tt[1])){
    tt = "Could not connect to the package server."
  }
  
  return (tt)
}
)
