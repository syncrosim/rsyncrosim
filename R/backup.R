# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Backup an SsimLibrary.
#'
#' Backup an SsimLibrary.
#'
#' @param ssimObject SsimLibrary/Project/Scenario.
#' @export
setGeneric('backup',function(ssimObject) standardGeneric('backup'))
#' @rdname backup
setMethod('backup', signature(ssimObject="character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})
  
#' @rdname backup
setMethod('backup', signature(ssimObject="SsimObject"), function(ssimObject) {
  tt = command(list(backup=NULL,lib=.filepath(ssimObject),input=NULL,output=NULL),session=.session(ssimObject))
  return(tt)
})
