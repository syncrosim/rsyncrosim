# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Set or remove Scenario dependency(s), or get existing dependencies.
#'
#' @details 
#' 
#' If dependency==NULL, other arguments are ignored, and set of existing dependencies is returned in order of precedence (from highest to lowest precedence).
#' Otherwise, returns list of saved or error messages for each dependency of each scenario.
#' 
#' Note that the order of dependencies can be important - dependencies added most recently take precedence over existing dependencies.
#' So, dependencies included in the dependency argument take precedence over any other existing dependencies.
#' If the dependency argument includes more than one element, elements are ordered from lowest to highest precedence.
#'
#' @param scenario Scenario. The scenario to which a dependency is to be added (or has already been added if remove=TRUE). 
#' @param dependency Scenario, character string, integer, or list/vector of these. The scenario(s) that are the source of the dependency, in order from lowest to highest precedence. If NULL other arguments are ingored and the list of existing dependencies is returned.
# @param scenario character string, integer, or vector of these. Name or ID of scenario(s) to which a dependency is to be added (or has been already added if remove=TRUE). If NULL then ssimObject must be a Scenario. Note that integer ids are slightly faster.
#' @param remove logical. If F (default) dependencies are added. If T, dependencies are removed.
#' @param force logical. If F (default) prompt before removing dependencies. 
#' @return If dependency!=NULL, character string (saved or error message) or list of these. Otherwise, a dataframe of existing dependencies, or list of these.
#' @export
setGeneric('dependency',function(scenario,dependency=NULL,remove=F,force=F) standardGeneric('dependency'))
#' @describeIn dependency Get, set, or remove dependencies of a Scenario.
setMethod('dependency', signature(scenario="Scenario"), function(scenario,dependency,remove,force) {
  #scenario = myScenario; dependency="Dependency Scenario";remove=F;force=T
  
  x=scenario
  cScn = scenarioId(scenario)
  cScnName = .name(scenario)
  outName =paste0(cScnName," [",cScn,"]")
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
    return(dependencySet) 
  }
  
  allScns = .scenario(.ssimLibrary(x),summary=T)
  
  outResults= list()
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
        outResults[[cDepRaw]]="Dependency not found in library, so ignored."  
      }
      cDep = allScns$scenarioId[allScns$name==cDepRaw]
    }
    if(class(cDepRaw)=="integer"){
      if(!is.element(cDepRaw,allScns$scenarioId)){
        warning(cDepRaw,": dependency scenario not found in library, so ignored.")
        outResults[[cDepRaw]]="Dependency not found in library, so ignored."  
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
        #msg = paste0(cDepOutName," is already a dependency of ", outName)
        #warning(msg)  
        #outResults[[cDepOutName]]=msg
        #to guarantee order of provided dependency, remove then re-add
        args = list(delete=NULL,dependency=NULL,lib=.filepath(x),sid=cScn,did=cDep,force=NULL)
        tt = command(args,.session(x))
        if(tt[1]!="saved"){
          warning("Scenario ",outName,", Dependency ",cDepOutName," error: ",tt[1])
        }
      }
      args = list(create=NULL,dependency=NULL,lib=.filepath(x),sid=cScn,did=cDep)
      tt = command(args,.session(x))
      if(tt[1]!="saved"){
        warning("Scenario ",outName,", Dependency ",cDepOutName," error: ",tt[1])
      }
      outResults[[cDepOutName]]=tt[1]
    }else{#remove
      if(!is.element(cDep,dependencySet$scenarioId)){
        msg = paste0(cDepOutName," is not a dependency of ", outName)
        warning(msg)  
        outResults[[cDepOutName]]=msg
      }else{
        if(force){
          answer="y"
        }else{
          answer <- readline(prompt=paste0("Do you really want to remove dependency ",cDepOutName," from ", outName,"? (y/n): "))
        }
        if(answer=="y"){
          args = list(delete=NULL,dependency=NULL,lib=.filepath(x),sid=cScn,did=cDep,force=NULL)
          tt = command(args,.session(x))
          if(tt[1]!="saved"){
            warning("Scenario ",outName,", Dependency ",cDepOutName," error: ",tt[1])
          }
          outResults[[cDepOutName]]=tt[1]
        }else{
          outResults[[cDepOutName]] = "skipped"
        }
      }
    }
  }
  return(outResults)
})