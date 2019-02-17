# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Installed or available packages
#'
#' Packages or installed or available for this version of SyncroSim.
#'
#' @param session Session.
#' @param installed Logical.  True to list installed packages and False to list available pacakges.
#' @return A dataframe of packages
#' @export
setGeneric('package',function(session, installed=T) standardGeneric('package'))

#' @rdname package
setMethod('package', signature(session="missingOrNULL"), function(session, installed=T) {
  session=.session()
  return(package(session, installed))
})

#' @rdname package
setMethod('package', signature(session="character"), function(session, installed=T) {
  return(SyncroSimNotFound(session, installed))
})
  
#' @rdname package
setMethod('package', signature(session="Session"), function(session, installed=T) {
  
  arg = "installed"
  
  if (!installed){
    arg = "available"
  }
  
  tt = command(c(arg),session,program="SyncroSim.PackageManager.exe")
  
  if(tt[1]=="saved"){
    out=data.frame(name=NA,displayName=NA,version=NA)
    out=subset(out,!is.na(name))
  }else{
    out = .dataframeFromSSim(tt,colNames=c("name","displayName","version"),csv=F)
  }
  return(out)
  
})
