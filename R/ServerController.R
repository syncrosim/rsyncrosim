# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
setOldClass("sockconn")
#' @include generics.R
NULL
#' ServerController class
#'
#' @name ServerController-class
#' @rdname ServerController-class
# @export ServerController
ServerController <- setClass("ServerController",representation(connection="sockconn"))

#remoteMsgBreakpointHit = 'breakpoint-hit',
#remoteMsgBreakpointContinue = 'breakpoint-continue',
#remoteMsgCallComplete = 'call-complete'

#' @export
setGeneric('connection<-',function(object,value) standardGeneric('connection<-'))
#' @export
setGeneric('connection',function(x,...) standardGeneric('connection'))
setMethod('connection', signature(x="ServerController"), function(x) return(x@connection))
setMethod('connection',signature(x="missingOrNULLOrChar"),
  function(x='127.0.0.1',port=13000) {
  #port=13000;ipAddress='127.0.0.1'
  con = socketConnection(host = ipAddress, port=port,open="w+",encoding="UTF-8")
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
  return(con)
})
setReplaceMethod(
  f="connection",
  signature="ServerController",
  definition=function(object,value){
    if(!is.element("sockconn",class(value))){
      stop('Must assign a socket connection object.')
    }
    object@connection = value
    return (object)
  }
)
#' @export
setGeneric('readServer',function(x,...) standardGeneric('readServer'))



