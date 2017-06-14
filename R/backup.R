# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License

#' Backup an SsimLibrary.
#'
#' Backup an SsimLibrary.
#'
#' @param ssimLibrary SsimLibrary.
#' @export
setGeneric('backup',function(ssimLibrary) standardGeneric('backup'))

setMethod('backup', signature(ssimLibrary="SsimLibrary"), function(ssimLibrary) {
  #ssimObject=myLibrary
  command("--help --backup")
  tt = command(list(backup=NULL,lib=.filepath(ssimLibrary),input=NULL,output=NULL),session=.session(ssimLibrary))
  return(tt)
})
