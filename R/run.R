# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Run scenarios
#'
#' Run one or more SyncroSim scenarios
#' 
#' @details
#' Note that breakpoints are ignored unless ssimObject is a single scenario.
#'
#' @param ssimObject SsimLibrary/Project/Scenario or a list of Scenarios. Or the path to a library on disk.
#' @param scenario character, integer, or vector of these. Scenario names or ids. Or NULL. Note that integer ids are slightly faster.
#' @param summary Logical. If FALSE (default) result Scenario objects are returned. If TRUE (faster) result scenario ids are returned.
#' @param jobs Iteger. The number of jobs to run. Passed to SyncroSim where multithreading is handled.
#' @param forceElements Logical. If TRUE then returns a single result scenario as a named list; otherwise returns a single result scenario as a Scenario object. Applies only when summary=FALSE.
#' @return If summary=F a result Scenario object or a named list of result Scenarios. The name is the parent scenario for each result. If summary=T returns summary info for result scenarios. 
#' @export
setGeneric('run',function(ssimObject,scenario=NULL,summary=F,jobs=1,forceElements=F) standardGeneric('run'))
#' @rdname run
setMethod('run', signature(ssimObject="character"), function(ssimObject,scenario,summary,jobs,forceElements) {
  if(ssimObject==SyncroSimNotFound(warn=F)){return(SyncroSimNotFound())}
  ssimObject = .ssimLibrary(ssimObject,create=F)
  out = run(ssimObject,scenario,summary,jobs,forceElements)
  return(out)
})
#' @rdname run
setMethod('run', signature(ssimObject="list"), function(ssimObject,scenario,summary,jobs,forceElements) {
  x = getIdsFromListOfObjects(ssimObject,expecting="Scenario",scenario=scenario)
  ssimObject = x$ssimObject
  scenario = x$objs
  out=run(ssimObject,scenario,summary,jobs,forceElements)
  return(out)
})
#' @rdname run
setMethod('run', signature(ssimObject="SsimObject"), function(ssimObject,scenario,summary,jobs,forceElements) {

  xProjScn = .getFromXProjScn(ssimObject,scenario=scenario,convertObject=T,returnIds=T,goal="scenario",complainIfMissing=T)
  #Now assume scenario is x is valid object and scenario is valid vector of scenario ids
  x = xProjScn$ssimObject
  scenario = xProjScn$scenario
  scenarioSet = xProjScn$scenarioSet
  
  if(!is.numeric(scenario)){stop("Error in run(): expecting valid scenario ids.")}
  
  out=list()
  addBits = seq(1,length(scenario))
  for(i in seq(length.out=length(scenario))){

    cScn = scenario[i]
    name = scenarioSet$name[scenarioSet$scenarioId == cScn][1]
    resultId = NA
    
    print(paste0("Running scenario [",cScn,"] ",name))

    if(class(x)=="Scenario"){
      breakpoints = NULL#breakpoints(x)
    }else{
      breakpoints=NULL
    }
    if((class(breakpoints)!="list")|(length(breakpoints)==0)){
      #TO DO: handle jobs, transformer and inpl.
      tt = command(list(run = NULL, lib = .filepath(x), sid = cScn, jobs = jobs), .session(x))

      for (i in tt) {
        if (startsWith(i, "Result scenario ID is:")) {
          resultId = strsplit(i, ": ", fixed = T)[[1]][2]
        } else {
          print(i)
        }
      }

      if (is.na(resultId)) {
        stop(print(tt))
      }      
    }else{

      # create a session
      cBreakpointSession=NULL#breakpointSession(x)
      #TO DO: multiple tries in connection
      
      # load a library
      msg =paste0('load-library --lib=\"',filepath(x),'\"')
      ret=NULL#remoteCall(cBreakpointSession,msg)
      if(ret!="NONE"){
        stop("Something is wrong: ",ret)
      }
      
      # set breakpoints
      ret = NULL#setBreakpoints(cBreakpointSession)
      if(ret!="NONE"){
        stop("Something is wrong: ",ret)
      }
      
      resultId = run(cBreakpointSession,jobs=jobs)
      #resp = writeLines("shutdown", connection(cBreakpointSession),sep = "")
      #close(connection(cBreakpointSession)) # Close the connection.
      resp=NULL
      cBreakpointSession=NULL
    }
    inScn = paste0(name," (",cScn,")")

    if(is.element(inScn,names(out))){inScn=paste(inScn,addBits[i])}
    if (!identical(resultId, suppressWarnings(as.character(as.numeric(resultId))))) {
      out[[inScn]]=tt
      print(tt)
    }else{
      if(summary){
        out[[inScn]] = as.numeric(resultId)
        scn = .scenario(x,scenario=as.numeric(resultId))
        multiband(scn,action="apply")
      }else{
        out[[inScn]] = .scenario(x,scenario=as.numeric(resultId))
        multiband(out[[inScn]],action="apply")
      }
    }   
  }

  if(summary&&(class(out)=="list")){
    #summary info for ids
    scnSelect = unlist(out)
    out = .scenario(x,scenario=scnSelect,summary=T)
  }
  
  if(!forceElements&&(class(out)=="list")&&(length(out)==1)){
    out=out[[1]]
  }
  return(out)
})

if(0){

setMethod('run',signature(ssimObject="BreakpointSession"),function(ssimObject,scenario,summary,jobs,forceElements) {
  x=ssimObject
  if(0){
    #PARALLEL DEBUG
    #use for debugging - setup code from 'run' function, signature(ssimObject="SsimLibrary")
    x=myScenario;jobs=2
    cBreakpointSession=breakpointSession(x)
    msg =paste0('load-library --lib=\"',filepath(x),'\"')
    ret=remoteCall(cBreakpointSession,msg)
    if(ret!="NONE"){
      stop("Something is wrong: ",ret)
    }
    ret = setBreakpoints(cBreakpointSession)
    if(ret!="NONE"){
      stop("Something is wrong: ",ret)
    }
    x=cBreakpointSession
  }
  
  msg = paste0('create-result --sid=',scenarioId(x@scenario))
  ret = remoteCall(x,msg)
  breaks = x@scenario@breakpoints
  newScn = scenario(x@scenario,scenario=as.numeric(ret))
  newScn@breakpoints = breaks
  x@scenario = newScn
  
  if(jobs==1){
    #make 1 job work first.
    msg = paste0('run-scenario --sid=',.scenarioId(x@scenario),' --jobs=1')
    ret = tryCatch({
      remoteCall(x,msg)
    }, warning = function(w) {
      print(w)
    }, error = function(e) {
      #resp = writeLines("shutdown", connection(x),sep = "")
      #close(connection(x)) # Close the connection.
      resp=NULL
      stop(e)
    })
  }else{
    msg = paste0('split-scenario --sid=',scenarioId(x@scenario),' --jobs=',jobs)
    
    tt = tryCatch({
      remoteCall(x,msg)
    }, warning = function(w) {
      print(w)
    }, error = function(e) {
      #resp = writeLines("shutdown", connection(x),sep = "")
      #close(connection(x)) # Close the connection.
      resp=NULL
      stop(e)
    })
    
    tempPath = paste0(filepath(x@scenario),".temp/Scenario-",.scenarioId(x@scenario),"/SSimJobs")
    tempFiles = list.files(tempPath,include.dirs=F)
    tempFiles = tempFiles[grepl(".ssim",tempFiles,fixed=T)&!grepl(".ssim.input",tempFiles,fixed=T)&!grepl(".ssim.output",tempFiles,fixed=T)]
    if(length(tempFiles)<=1){
      #resp = writeLines("shutdown", connection(x),sep = "")
      #close(connection(x)) # Close the connection.
      resp=NULL
      stop("Problem with split-scenario: only one job was created. This is known problem caused by dependencies that has not yet been fixed.")
    }else{
      jobs = length(tempFiles)
    }
    
    #if(tt!="NONE"){
    #  stop("Something is wrong: ",tt)
    #}
    threads = c()
    
    jobs = min(jobs,parallel::detectCores())
    
    files = paste0(filepath(x@scenario),".temp/Scenario-",scenarioId(x@scenario),"/SSimJobs/Job-",seq(1:jobs),".ssim")
    
    port =   as.numeric(strsplit(summary(x@connection)[[1]],":")[[1]][2])
    ports=port+ seq(1,jobs)
    #make list of arguments for parLapply()
    args = list()
    for(i in 1:length(files)){
      #i = 1
      args[[i]]=list(x=files[i],session=session(x@scenario),port=ports[i],breaks = breakpoints(x@scenario))
    }
    
    #Following http://www.win-vector.com/blog/2016/01/parallel-computing-in-r/
    parallelCluster = parallel::makeCluster(jobs,outfile=paste0(dirname(filepath(x@scenario)),"/parallelLog.txt"))
    parallel::clusterEvalQ(parallelCluster, library(rsyncrosim))
    print(parallelCluster)
    
    #TO DO: catch error messages properly in parallel processing...
    ret = tryCatch({
      parallel::parLapply(parallelCluster,args,runJobParallel)
    }, warning = function(w) {
      print(tt)
      print(w)
    }, error = function(e) {
      print(tt)
      #resp = writeLines("shutdown", connection(x),sep = "")
      #close(connection(x)) # Close the connection.
      resp=NULL
      stop(e)
    })
    
    # Shutdown cluster neatly
    if(!is.null(parallelCluster)) {
      parallel::stopCluster(parallelCluster)
      parallelCluster = c()
    }
    print(ret)
    
    msg = paste0('merge-scenario --sid=',scenarioId(x@scenario))
    ret = tryCatch({
      remoteCall(x,msg)
    }, warning = function(w) {
      print(w)
    }, error = function(e) {
      #resp = writeLines("shutdown", connection(x),sep = "")
      #close(connection(x)) # Close the connection.
      resp=NULL
      stop(e)
    })
    
    #remove temporary directory
    unlink(paste0(filepath(x@scenario),".temp/Scenario-",scenarioId(x@scenario)),recursive=T)
    
    return(ret)
  }
})
}
