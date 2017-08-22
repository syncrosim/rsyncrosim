# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' addon(s) of an SsimLibrary or Session
#'
#' The addon(s) of an SsimLibrary or Session.
#'
#' @param ssimObject SsimLibrary/Project/Scenario or Session.
#' @return A dataframe of addons.
#' @examples
#' \dontrun{
#' addon(ssimLibrary(name="stsim"))
#' }
#' #test change #2
#' @export
setGeneric('addon',function(ssimObject) standardGeneric('addon'))
#' @rdname addon
setMethod('addon', signature(ssimObject="character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})
#' @rdname addon
setMethod('addon', signature(ssimObject="missingOrNULL"), function(ssimObject) {
  #x = myLibrary
  ssimObject=.session()
  tt = command(list(list=NULL,addons=NULL,csv=NULL),ssimObject)
  tt = .dataframeFromSSim(tt)
  #tt$shortName = gsub(":add-on-transformer","",tt$name,fixed=T)
  return(tt)
})
#' @rdname addon
setMethod('addon', signature(ssimObject="Session"), function(ssimObject) {
  #x = myLibrary
  tt = command(list(list=NULL,addons=NULL,csv=NULL),ssimObject)
  tt = .dataframeFromSSim(tt)
  #tt$shortName = gsub(":add-on-transformer","",tt$name,fixed=T)
  return(tt)
})
#' @rdname addon
setMethod('addon', signature(ssimObject="SsimObject"), function(ssimObject) {
  #x = myLibrary
  enabled=NULL
  tt = command(list(list=NULL,addons=NULL,csv=NULL,lib=.filepath(ssimObject)),.session(ssimObject))
  tt = .dataframeFromSSim(tt,convertToLogical=c("enabled"))
  #tt$shortName = gsub(":add-on-transformer","",tt$name,fixed=T)
  return(tt)
})
