# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Set or remove Scenario dependency(s), or get existing dependencies.
#'
#' @details 
#' 
#' If dependency==NULL, other arguments are ignored, and set of existing dependencies is returned.
#' Otherwise, returns list of saved or error messages for each dependency of each scenario.
#'
#' @param ssimObject SsimLibrary/Scenario/Project. The scenario to which a dependency is to be added (or has already been added if remove=TRUE). If SsimLibrary/Project, the scenario argument must specify the scenario(s). If ssimObject is a Scenario, scenario argument is ignored.
#' @param dependency Scenario, character string, integer, or list/vector of these. The scenario(s) that is the source of the dependency. If NULL other arguments are ingored and the list of existing dependencies is returned.
#' @param scenario character string, integer, or vector of these. Name or ID of scenario(s) to which a dependency is to be added (or has been already added if remove=TRUE). If NULL then ssimObject must be a Scenario.
#' @param remove logical. If F (default) dependencies are added. If T, dependencies are removed.
#' @param force logical. If F (default) prompt before removing dependencies. 
#' @return If dependency!=NULL, character string (saved or error message) or list of these. Otherwise, a dataframe of existing dependencies, or list of these.
#' @export
setGeneric('dependency',function(ssimObject,dependency=NULL,scenario=NULL,remove=F,force=F) standardGeneric('dependency'))

setMethod('dependency', signature(ssimObject="list"), function(ssimObject,dependency,scenario,remove,force) {
  if(class(ssimObject[[1]])!="Scenario"){
    stop("ssimObject must be an SsimLibrary/Project/Scenario, or list of Scenarios.")
    
  }
  if(!is.null(scenario)){
    warning("scenario argument is ignored when ssimObject is a list of scenarios.")
    scenario=NULL
  }
  
  outList=list()
  for(i in 1:length(ssimObject)){
    #i=1
    cScn = ssimObject[[i]]
    cOutName = paste0(.name(cScn)," [",.scenarioId(cScn),"]")
    
    cOut = dependency(cScn,dependency,scenario,remove,force)
    outList[[cOutName]]=cOut
  }
  return(outList)
})
  

setMethod('dependency', signature(ssimObject="SsimObject"), function(ssimObject,dependency,scenario,remove,force) {
  #ssimObject = myScenario; dependency="Dependency Scenario"; scenario=NULL;remove=F;force=T
  if(class(ssimObject)!="Scenario"){
    if(is.null(scenario)){
      stop("If ssimObject is not a Scenario scenario argument must be specified.")
    }
  }
  xProjScn = .getFromXProjScn(ssimObject,scenario=scenario,convertObject=F,returnIds=T,goal="scenario",complainIfMissing=T)
  #Now assume scenario is x is valid object and scenario is valid vector of scenario ids
  x = xProjScn$ssimObject
  scenario = xProjScn$scenario
  scenarioSet = xProjScn$scenarioSet
  
  outResults=list()
  for (i in seq(length.out=length(scenario))){
    #i =1 
    cScn = scenario[i]
    outName =paste0(scenarioSet$name[scenarioSet$scenarioId==cScn]," [",cScn,"]")
    #get set of existing dependencies
    args = list(list=NULL,dependencies=NULL,lib=.filepath(x),sid=cScn,csv=NULL)
    tt= command(args,.session(x))  
    if(!grepl("ID,Name",tt[1],fixed=T)){
      stop(tt[1])
    }
    dependencySet = .dataframeFromSSim(tt)
    names(dependencySet)[names(dependencySet)=="iD"]="scenarioId"
    
    #if no dependency, dependency info for each scenario
    if(is.null(dependency)){
      outResults[[outName]] = dependencySet        
      next
    }

    allScns = .scenario(.ssimLibrary(x),summary=T)
      
    outResults[[outName]]= list()
    for(j in seq(length.out=length(dependency))){
      #j=1
      cDepRaw = dependency[[j]]
      cDep=NULL
      if(class(cDepRaw)=="Scenario"){
        cDep = .scenarioId(cDepRaw)
      }
      if(class(cDepRaw)=="character"){
        if(!is.element(cDepRaw,allScns$name)){
           warning(cDepRaw,": dependency scenario not found in library, so ignored.")
          outResults[[outName]][[cDepRaw]]="Dependency not found in library, so ignored."  
        }
        cDep = allScns$scenarioId[allScns$name==cDepRaw]
      }
      if(class(cDepRaw)=="integer"){
        if(!is.element(cDepRaw,allScns$scenarioId)){
          warning(cDepRaw,": dependency scenario not found in library, so ignored.")
          outResults[[outName]][[cDepRaw]]="Dependency not found in library, so ignored."  
        }
        cDep = cDepRaw
      }
      if(is.null(cDep)){
        stop("dependency must be a Scenario, character string, integer, or vector/list of these.")
      }
      cDepOutName = paste0(allScns$name[allScns$scenarioId==cDep]," [",cDep,"]")

      #if add
      if(!remove){
        if(is.element(cDep,dependencySet$scenarioId)){
          msg = paste0(cDepOutName," is already a dependency of ", outName)
          warning(msg)  
          outResults[[outName]][[cDepOutName]]=msg
        }else{
          args = list(create=NULL,dependency=NULL,lib=.filepath(x),sid=cScn,did=cDep)
          tt = command(args,.session(x))
          if(tt[1]!="saved"){
            warning("Scenario ",outName,", Dependency ",cDepOutName," error: ",tt[1])
          }
          outResults[[outName]][[cDepOutName]]=tt[1]
        }
      }else{#remove
        if(!is.element(cDep,dependencySet$scenarioId)){
          msg = paste0(cDepOutName," is not a dependency of ", outName)
          warning(msg)  
          outResults[[outName]][[cDepOutName]]=msg
        }else{
          if(force){
            answer="y"
          }else{
            answer <- readline(prompt=paste0("Do you really want to remove dependency ",cDepOutName," from ", cOut,"? (y/n): "))
          }
          if(answer=="y"){
            args = list(delete=NULL,dependency=NULL,lib=.filepath(x),sid=cScn,did=cDep,force=NULL)
            tt = command(args,.session(x))
            if(tt[1]!="saved"){
              warning("Scenario ",outName,", Dependency ",cDepOutName," error: ",tt[1])
            }
            outResults[[outName]][[cDepOutName]]=tt[1]
          }else{
            outResults[[outName]][[cDepOutName]] = "skipped"
          }
        }
      }
    }
  }
  if(length(outResults)==1){
    outResults=outResults[[1]]
  }
  return(outResults)
})