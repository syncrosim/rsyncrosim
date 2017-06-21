# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Backup an SsimLibrary.
#'
#' Backup an SsimLibrary.
#'
#' @param ssimObject SsimLibrary/Project/Scenario.
#' @export
setGeneric('backup',function(ssimObject) standardGeneric('backup'))

setMethod('backup', signature(ssimObject="SsimObject"), function(ssimObject) {
  #ssimObject=myLibrary
  tt = command(list(backup=NULL,lib=.filepath(ssimObject),input=NULL,output=NULL),session=.session(ssimObject))
  return(tt)
})
