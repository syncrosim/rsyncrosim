# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Installed packages
#'
#' Packages installed with this version of SyncroSim
#'
#' @param session Session.
#' @return A dataframe of packages
#' @export
setGeneric('package',function(session) standardGeneric('package'))
#' @rdname package
setMethod('package', signature(session="missingOrNULL"), function(session) {
  session=.session()
  return(package(session))
})
#' @rdname package
setMethod('package', signature(session="character"), function(session) {
  return(SyncroSimNotFound(session))
})
  
#' @rdname package
setMethod('package', signature(session="Session"), function(session) {
  tt = command(c("installed"),session,program="SyncroSim.PackageManager.exe")
  if(tt[1]=="saved"){
    out=data.frame(name=NA,displayName=NA,version=NA)
    out=subset(out,!is.na(name))
  }else{
    out = .dataframeFromSSim(tt,colNames=c("name","displayName","version"),csv=F)
  }
  return(out)
})
