# Author: Josie Hughes
# Date : December 2016
# Version 0.1
# Licence GPL v3
setOldClass("sockconn")
#' @include generics.R
NULL
#' BreakpointSession class
#'
#' @slot scenario A SyncroSim scenario
#' @slot connection A socket connection to the SyncroSim server.
#' @name BreakpointSession-class
#' @rdname BreakpointSession-class
#' @export BreakpointSession
BreakpointSession <- setClass("BreakpointSession",representation(scenario="Scenario",connection="sockconn"))
# @name Scenario
# @rdname Scenario-class
setMethod(f='initialize',signature="BreakpointSession",
          definition=function(.Object,scenario,ipAddress=NULL,port=NULL,quiet=T){
  #scenario = myScenario;ipAddress=NULL;port=NULL;quiet=F

  location = filepath(session(scenario)) #guaranteed to be valid
  if(is.null(ipAddress)){
    ipAddress = '127.0.0.1'
  }
  if(is.null(port)){
    port = 13000
  }

  #start the server
  args = list(ipaddress=ipAddress,port=port,quiet=quiet)
  tt = command(args,.session(scenario),program="/SyncroSim.Server.exe",wait=F)

  .Object@connection = connection(ipAddress,port)
  .Object@scenario = scenario

  return(.Object)
})
#' @export
breakpointSession<-function(scenario,ipAddress=NULL,port=NULL,quiet=T){
  return(new("BreakpointSession"),scenario,ipAddress,port,quiet)
}

#' @export
setGeneric('connection<-',function(x,value) standardGeneric('connection<-'))
setMethod('connection', signature(x="BreakpointSession"), function(x) return(x@connection))
setReplaceMethod(
  f='connection',
  signature="BreakpointSession",
  definition=function(x,value){
    if(!is.element("sockconn",class(value))){
      stop('Must assign a socket connection object.')
    }
    x@connection = value
    return (x)
  }
)



