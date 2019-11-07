# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
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
#' addon(ssimLibrary(name="mylib"))
#' @export
setGeneric('addon',function(ssimObject) standardGeneric('addon'))

#' @rdname addon
setMethod('addon', signature(ssimObject="character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname addon
setMethod('addon', signature(ssimObject="missingOrNULL"), function(ssimObject) {
  ssimObject=.session()
  tt = command(list(list=NULL,addons=NULL,csv=NULL),ssimObject)
  tt = .dataframeFromSSim(tt)
  return(tt)
})

#' @rdname addon
setMethod('addon', signature(ssimObject="Session"), function(ssimObject) {
  tt = command(list(list=NULL,addons=NULL,csv=NULL),ssimObject)
  tt = .dataframeFromSSim(tt)
  return(tt)
})

#' @rdname addon
setMethod('addon', signature(ssimObject="SsimObject"), function(ssimObject) {
  enabled=NULL
  tt = command(list(list=NULL,addons=NULL,csv=NULL,lib=.filepath(ssimObject)),.session(ssimObject))
  tt = .dataframeFromSSim(tt,convertToLogical=c("enabled"))
  return(tt)
})
