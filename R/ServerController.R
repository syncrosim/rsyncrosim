# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
setOldClass("sockconn")
#' @include generics.R
NULL
#' ServerController class
#'
#' @examples
#' @name ServerController-class
#' @rdname ServerController-class
#' @export ServerController
ServerController <- setClass("ServerController",contains="sockconn")
#setMethod(f="initialize",signature="ServerController",
#          definition=function(.Object,model=NULL,name=NULL,cSession=NULL,backup=F,backupName="backup",backupOverwrite=T){
#remoteMsgBreakpointHit = 'breakpoint-hit',
#remoteMsgBreakpointContinue = 'breakpoint-continue',
#remoteMsgCallComplete = 'call-complete'
setGeneric('connect',function(x,...) standardGeneric('connect'))
setMethod('connect', signature(x="ServerController"),
          function(x,ipAddress='127.0.0.1',port=13000) {
  #port=13000;ipAddress='127.0.0.1'


  con = socketConnection(host = ipAddress, port=port)
  ## S3 method for class 'connection'
  #open(con, open = "r", blocking = TRUE, ...)
  ## S3 method for class 'connection'
  #close(con, type = "rw", ...)
  #flush(con)
  #isOpen(con, rw = "")
  #isIncomplete(con)

  if(!isOpen(con)){
    stop(paste0('Problem connecting to the SyncroSim server. IP:',ipAddress," Port:",port))
  }
  return(x=con)
})


