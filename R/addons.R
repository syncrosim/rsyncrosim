# addons of an SsimLibrary or Session
# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' addons of an SsimLibrary or Session
#'
#' The addons of an SsimLibrary or Session.
#'
#' @param ssimObject SsimLibrary/Project/Scenario or Session.
#' @param all logical. If TRUE, all available addons are returned. Otherwise, only enabled addons.
#' @return A dataframe of addons.
#' @examples
#' addons(ssimLibrary(name="stsim"))
#' @export
setGeneric('addons',function(ssimObject,all=F) standardGeneric('addons'))
#' @rdname addons
setMethod('addons', signature(ssimObject="missingOrNULL"), function(ssimObject,all) {
  #x = myLibrary
  ssimObject=.session()
  tt = command(list(list=NULL,addons=NULL,csv=NULL),ssimObject)
  tt = .dataframeFromSSim(tt)
  #tt$shortName = gsub(":add-on-transformer","",tt$name,fixed=T)
  return(tt)
})

setMethod('addons', signature(ssimObject="Session"), function(ssimObject,all) {
  #x = myLibrary
  tt = command(list(list=NULL,addons=NULL,csv=NULL),ssimObject)
  tt = .dataframeFromSSim(tt)
  #tt$shortName = gsub(":add-on-transformer","",tt$name,fixed=T)
  return(tt)
})
#' @rdname addons
setMethod('addons', signature(ssimObject="SsimObject"), function(ssimObject,all) {
  #x = myLibrary
  enabled=NULL
  tt = command(list(list=NULL,addons=NULL,csv=NULL,lib=.filepath(ssimObject)),.session(ssimObject))
  tt = .dataframeFromSSim(tt)
  if(!all){
    tt=subset(tt,enabled=="Yes")
  }
  #tt$shortName = gsub(":add-on-transformer","",tt$name,fixed=T)
  return(tt)
})
