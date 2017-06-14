# Author: Josie Hughes
# Date : December 2016
# Version 0.1
# Licence GPL v3
setOldClass("sockconn")
#' @include generics.R
#' @include scenario.R
NULL
# BreakpointSession class
#
# @slot scenario A SyncroSim scenario
# @slot connection A socket connection to the SyncroSim server.
# @slot name A name
# @name BreakpointSession-class
# @rdname BreakpointSession-class
# @export BreakpointSession
BreakpointSession <- setClass("BreakpointSession",representation(scenario="Scenario",connection="sockconn",name="character"))
# @name Scenario
# @rdname Scenario-class
setMethod(f='initialize',signature="BreakpointSession",
          definition=function(.Object,scenario,ipAddress='127.0.0.1',port=13000,quiet=T,name="Main",startServer=T){
  #scenario = myScenario;ipAddress=NULL;port=NULL;quiet=F

  location = filepath(session(scenario)) #guaranteed to be valid

  #start the server
  if(startServer){
    args = list(ipaddress=ipAddress,port=port,quiet=quiet)
    tt = command(args,.session(scenario),program="SyncroSim.Server.exe",wait=F)
  }
  .Object@connection = connection(ipAddress,port)
  .Object@scenario = scenario
  .Object@name = name

  return(.Object)
})
#' @export
breakpointSession<-function(scenario,ipAddress='127.0.0.1',port=13000,quiet=T,name="Main",startServer=T){
  return(new("BreakpointSession",scenario,ipAddress,port,quiet,name,startServer))
}

# @export
setGeneric('connection<-',function(x,value) standardGeneric('connection<-'))
# @describeIn connection Get the connection of a BreakpointSession.
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

#' @export
setGeneric('remoteCall',function(x,message,getResponse=T) standardGeneric('remoteCall'))
setMethod('remoteCall',signature(x="BreakpointSession"),function(x,message,getResponse) {
  #x = cBreakpointSession;message=msg;getResponse=T
  if(!getResponse){stop("handle this case")}

  ret=""
  tt = writeLines(message, connection(x),sep = "") #Gives an error

  while(getResponse){
    #TO DO: less stupid way of finding EOL.
    #res=""
    #cRes = character(0)
    #while((res=="")|(length(cRes)>0)){
    #  cRes =  readChar(connection(x),1024,useBytes=F)
    #  res=paste0(res,cRes)
    #  if(res==""){
    #    Sys.sleep(1)
    #  }
    #}
    #res = readChar(connection(x),1024,useBytes=F)
    #readChar(connection(x),30,useBytes=F)
    #while(isIncomplete(connection(x))){
    #  Sys.sleep(1)
    #}
    #Sys.sleep(1)
    #res = scan(connection(x),sep = "\n",what="character",nlines=1)
    #res=readLines(connection(x),n=1,ok=T)
    #RESUME HERE

    cRes = readChar(connection(x),1,useBytes=F)
    if(length(cRes)==0){
      #warning("No return message received")
      Sys.sleep(1) #try again in a while
    }else{
      #read the rest of the message
      res=""
      while(cRes!="\n"){
        res = paste0(res,cRes)
        cRes = readChar(connection(x),1,useBytes=F)
      }


      cmd=strsplit(res,"|",fixed=T)[[1]][1]
      #if(!cmd){break}#not (cmd and cmd.strip())s
      if(cmd=='breakpoint-hit'){
        #TO DO: move this to onBreakpointHit function
        #self.on_breakpoint_hit(res.split('|'))
        #res="breakpoint-hit|syncrosim-stochastic-time:break-before-iteration|stsim:core-transformer|1"
        split = strsplit(res,"|",fixed=T)[[1]]

        tt = onBreakpointHit(x,split)
        tt=writeLines('breakpoint-continue',connection(x),sep="")

      }else if(cmd=='call-complete'){
        split = strsplit(res,"|",fixed=T)[[1]]
        if (split[2] == 'FAILURE'){
          stop("Server returned a failure: ",split[3])
        }else{
          ret = split[3]
          break
        }
      }else{
        stop("server response invalid: ",cmd)
      }
    }
  }
  return(ret)
})

#' @export
setGeneric('onBreakpointHit',function(x,split) standardGeneric('onBreakpointHit'))
setMethod('onBreakpointHit',signature(x="BreakpointSession"),function(x,split) {
  cBreak = x@scenario@breakpoints[[split[2]]]
  #TO DO: consider ways to speed up scenario construction here.
  cResult = scenario(.project(x@scenario),scenario=as.numeric(split[4]))
  
  #multiband(cResult,"rebuild")

  cBreak@callback(cResult,iteration=as.numeric(split[5]),timestep=as.numeric(split[6]))

  #load modified data if availabl
  dataDir = paste0(.filepath(cResult),'.temp/Data')
  if(file.exists(dataDir)){
    msg = 'execute-command --name=data-ready'
    tt=remoteCall(x,msg)
    if(tt!="NONE"){
      stop("Something is wrong: ",tt)
    }
    unlink(dataDir,recursive=T)
  }
  NULL
})

# @export
setGeneric('setBreakpoints',function(x) standardGeneric('setBreakpoints'))
setMethod('setBreakpoints',signature(x="BreakpointSession"),function(x) {
  #x=cBreakpointSession
  cBreaks = .breakpoints(x@scenario)
  if(length(cBreaks)==0){stop("Expecting breakpoints")}
  ret =list()
  for(i in 1:length(cBreaks)){
    #i = 1
    cBreak = cBreaks[[i]]
    msg = paste0('set-breakpoint --name=',cBreak@breakpointName,' --trx=',cBreak@transformerName,' --granularity=',cBreak@arguments)

    ret[[names(cBreaks)[i]]] = remoteCall(x,msg)
  }
  return(ret)
})

#' @export
# The single argument function for parallel
runJobParallel<- function(cPars) {
  #bindToEnv(objNames=c('breakpointSession','remoteCall','setBreakpoints'))
  #function(cPars) {
  #cPars=args[[2]]
    #library(rsyncrosim) #NOTE: rsyncrosim must be installed properly in order for this to work.
    #cPars is a list, where x is path to the temporary library, session is the current session, port is a new port, and breaks are the current breakpoints
    #f = files[1];port=ports[1]
    # TO DO: if slow, consider ways to speed up scenario/library construction
    ret = tryCatch({

      cScn = scenario(.ssimLibrary(cPars$x,session=cPars$session,create=F),scenario=1)
      if(!exists("cScn")){
        stop("Problem with split-scenario: Can't find the library ",cPars$x,".")
      }
      cScn@breakpoints = cPars$breaks
      sess=breakpointSession(cScn,port=cPars$port,name=paste0("Child=",cPars$port),startServer=T)
      if(!exists("sess")){
        stop("Problem creating breakpoint session.")
      }
      msg = paste0('load-library --lib=\"',filepath(cScn),'\"')
      ret=remoteCall(sess,msg)
      if(ret!="NONE"){
        stop("Problem with ",msg," :",ret)
      }
      ret = setBreakpoints(sess)
      
      msg = paste0('run-scenario --sid=',1,' --jobs=1')
      ret = remoteCall(sess,msg)
      if(as.character(as.numeric(ret))!=ret){
        stop("Problem with ",msg," :",ret)
      }
      "saved"
    }, error = function(e) {
      print(e)
      return(e)
    }, finally = {
      resp = writeLines("shutdown", connection(sess),sep = "")
      close(connection(sess)) # Close the connection.
    })
  #}
}
