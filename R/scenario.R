# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

setMethod(f='initialize',signature="Scenario",
    definition=function(.Object,ssimLibrary=NULL,project=NULL,name=NULL,id=NULL,sourceScenario=NULL,scenarios=NULL){
    #assume this is being called from scenario fn or getFromXProjScn(). ssimObject and pid are valid, id is valid if not null, and duplicate name problems have been sorted out. 

    .Object@breakpoints = list()
    .Object@parentId = 0
    x=ssimLibrary

    #For fast processing - quickly return without system calls if scenario exists
    if(is.null(scenarios)){
      scenarios = getScnSet(x)      
    }
    allScenarios=scenarios
    if(!is.null(name)){cName=name;scenarios=subset(scenarios,name==cName)}
    if(!is.null(id)){scenarios=subset(scenarios,scenarioId==id)}
    if(!is.null(project)){scenarios=subset(scenarios,projectId==project)}
    
    findScn = subset(scenarios,!is.na(scenarioId))
    if(nrow(findScn)>1){
      stop("Something is wrong.")
    }

    #If found only one, open it.
    if(nrow(findScn)==1){
      if(!is.null(sourceScenario)){
        stop("Scenario ",name," already exists. Delete the scenario before replacing it.")
      }
      if(findScn$isResult=="Yes"){
        parentBit = strsplit(findScn$name,"[",fixed=T)[[1]][2]
        parent = strsplit(parentBit,"]",fixed=T)[[1]][1]
        .Object@parentId = as.numeric(parent)
      }

      #Go ahead and create the Scenario object without issuing system commands to make sure it is ok
      .Object@session=.session(x)
      .Object@filepath=.filepath(x)
      .Object@datasheetNames = .datasheets(x,scope="all",refresh=T)
      .Object@scenarioId = as.numeric(findScn$scenarioId)
      .Object@projectId = as.numeric(findScn$projectId)
      return(.Object)
    }
    
    #If given an id for a scenario that does not yet exist, complain
    if(is.null(name)){
      stop(paste0("The library does not contain scenario id ",id,". Please provide a name for the new scenario - the id will be assigned automatically by SyncroSim."))
    }
    
    #Now go ahead to handle odder cases
    #Assume pid is valid, and name is defined, not duplicated, etc
    #x can be either a project or a library - but need a project in order to create a new scenario
    pid=project

    #Create a new scenario
    if(is.null(sourceScenario)){
      tt = command(list(create=NULL,scenario=NULL,lib=.filepath(x),name=name,pid=pid),.session(x))
    }else{
      sid=sourceScenario
      slib=.filepath(x)
      if(class(sourceScenario)=="numeric"){
        if(!is.element(sourceScenario,allScenarios$scenarioId)){
          stop("Source scenario id ",sourceScenario," not found in SsimLibrary.")
        }
      }
      if(class(sourceScenario)=="character"){
        sourceOptions = subset(allScenarios,name==sourceScenario)
        if(nrow(sourceOptions)==0){
          stop(paste0("Source scenario name ",sourceScenario," not found in SsimLibrary."))
        }
        if(nrow(sourceOptions)>1){
          stop(paste0("There is more than one scenario called ",sourceScenario," in the SsimLibrary. Please provide a sourceScenario id: ",paste(sourceOptions$scenarioId,collapse=",")))
        }
        sid=sourceOptions$scenarioId
      }
      
      if(class(sourceScenario)=="Scenario"){
        sid=.scenarioId(sourceScenario)
        slib=.filepath(sourceScenario)
        sourceScnName = name(sourceScenario)
      }else{
        sourceScnName = subset(allScenarios,scenarioId==sid)$name
      }
      
      if(name=="GetSourceCopyCopyCopy"){
        copyName = paste(sourceScnName,"- Copy")
        if(!is.element(copyName,allScenarios$name)){
          name = copyName
        }else{
          done=F
          count=0
          while(!done){
            count=count+1
            cName =paste0(copyName,count)
            if(!is.element(cName,allScenarios$name)){
              name=cName
              done=T
            }
          }
        }
      }
      
      tt = command(list(copy=NULL,scenario=NULL,slib=slib,tlib=.filepath(x),name=name,sid=sid,pid=pid),.session(x))
    }
    id = as.numeric(strsplit(tt,": ")[[1]][2])

    .Object@session=.session(x)
    .Object@filepath=.filepath(x)
    .Object@datasheetNames = .datasheets(x,refresh=T,scope="all")
    .Object@scenarioId = as.numeric(id)
    .Object@projectId = as.numeric(pid)
    return(.Object)
  }
)

#' Create or open one or more Scenarios.
#'
#' If summary = FALSE, returns one or more \code{\link{Scenario}} objects representing a SyncroSim scenarios.
#' If summary = TRUE, returns scenario summary info.
#'
#' @details
#'
#' For each element of scenario:
#' \itemize{
#'   \item {If element/project/ssimObject uniquely identifies an existing scenario: }{Returns the existing Scenario}
#'   \item {If element/project/ssimObject uniquely identifies more than one existing scenario: }{Error}
#'   \item {If element/project/ssimObject do not identify an existing scenario or project: }{Error}
#'   \item {If element/project/ssimObject do not identify an existing scenario and element is numeric: }{Error - a name is required for new scenarios. SyncroSim will automatically assign an id when a scenario is created.}
#'   \item {If element/project/ssimObject do not identify an existing scenario and do identify a project, and element is a character string: }{Creates a new Scenario named element in the project. SyncroSim automatically assigns an id. If sourceScenario is not NULL the new scenario will be a copy of sourceScenario.}
#' }
#'
#' @param ssimObject SsimLibrary/Project or character. An ssimObject containing a filepath to a library, or a filepath.
#' @param scenario Character, integer, or vector of these. Names or ids of one or more scenarios. Note integer ids are slightly faster.
#' @param sourceScenario Character or integer. If not NULL, new scenarios will be copies of the sourceScenario.
#' @param summary Logical. If TRUE then loads and returns the scenario(s) in a named vector/dataframe with the scenarioId, name, description, owner, dateModified, readOnly, parentID. Default is TRUE if scenario=NULL, FALSE otherwise.
#' @param results Logical. If TRUE only return result scenarios.
#' @param forceElements Logical. If TRUE then returns a single scenario as a named list; otherwise returns a single scenario as a Scenario object. Applies only when summary=FALSE.
#' @param overwrite Logical. If TRUE an existing Scenario will be overwritten.
#' @return A \code{Scenario} object representing a SyncroSim scenario, a list of Scenario objects, or a dataframe of scenario names and descriptions.
#' @examples
#' # Create a new scenario
#' myLibrary = ssimLibrary(name="stsim")
#' myProject = project(myLibrary,project="a project") 
#' myScenario = scenario(myProject,scenario="a scenario",overwrite=T)
#' @name scenario
#' @export
scenario <- function(ssimObject=NULL,scenario=NULL,sourceScenario=NULL,summary=NULL,results=F,forceElements=F,overwrite=F){ 
  
  if(is.character(ssimObject)&&(ssimObject==SyncroSimNotFound(warn=F))){
    return(SyncroSimNotFound())
  }
  
  if (is.null(ssimObject)){
    e = ssimEnvironment()
    ssimObject = ssimLibrary(e$LibraryFilePath)
    scenario=as.integer(e$ScenarioId)
  }
  
  isResult=NULL
  #if ssimObject is a scenario return the scenario
  if(is.element(class(ssimObject),c("Scenario"))&is.null(scenario)){
    if(is.null(summary)){summary=F}
    if(!summary){
      convertObject=T
      returnIds=F
    }else{
      convertObject=F
      returnIds=T
    }
  }else{
    #set summary default
    if(is.null(summary)){
      if(is.null(scenario)){
        if(is.null(sourceScenario)){
          summary=T
        }else{
          summary=F
          scenario = "GetSourceCopyCopyCopy"
        }
      }else{
        summary=F
      }
    }
    convertObject=F
    returnIds=T
  }
  
  xProjScn  =.getFromXProjScn(ssimObject,project=NULL,scenario=scenario,convertObject=convertObject,returnIds=returnIds,goal="scenario",complainIfMissing=F)
  
  if(class(xProjScn)=="Scenario"){
#    if (create){
#      stop(paste0("Cannot overwrite existing scenario.  Use overwrite=T.",project)) 
#    }
    if (!overwrite){
      return(xProjScn)      
    }
  }
  
  if(class(xProjScn)!="list"){
    stop("something is wrong")
  }
  ssimObject=xProjScn$ssimObject
  project=xProjScn$project
  scenario=xProjScn$scenario
  allScenarios = xProjScn$scenarioSet
  if(is.element("order",names(allScenarios))){
    scnSet=subset(allScenarios,!is.na(order))
  }else{
    if(nrow(allScenarios)>0){
      allScenarios$order=seq(1,nrow(allScenarios))
    }
    scnSet=allScenarios
  }

  if(results){
    scnSet = subset(scnSet,!is.element(isResult,c(NA,F,"No")))
  }
  
  if(nrow(scnSet)==0){
    if(summary){
      scnSet$exists = NULL
      scnSet$order=NULL
      return(scnSet)
    }else{
      stop("Error in scenario(): No scenarios to get or make.") 
    }
  }
  #if all projects exist and summary, simply return summary
  if((sum(is.na(scnSet$exists))==0)&summary){
    scnSet=subset(scnSet,!is.na(order))
    scnSet=scnSet[order(scnSet$order),]
    scnSet[scnSet$readOnly == "FALSE", "readOnly"] <- "No"
    scnSet[scnSet$readOnly == "TRUE", "readOnly"] <- "Yes"
    scnSet$exists = NULL
    scnSet$order=NULL
    return(scnSet)
  }
  
  #Now assume scenario is defined
  #distinguish existing scenarios from those that need to be made
  areIds = is.numeric(scenario)

  #if scenarios need to be made, pass all scenarios in library
  makeSum = sum(!is.na(scnSet$order)&is.na(scnSet$exists))
  libScns=subset(allScenarios,!is.na(exists))
  if(makeSum>0){
    if(!is.null(sourceScenario)&&(class(sourceScenario)!="Scenario")){
      libScns = getScnSet(ssimObject) #get all scenarios for library, not just those from ssimObject
      #check validity in new("Scenario",...)
    }
  }else{
    if(!is.null(sourceScenario)){
      warning("sourceScenario was ignored because scenario already exists.")
    }
  }

  #make scnenarios/scenario objects
  scnsToMake = subset(scnSet,!is.na(order))
  if(overwrite){
    for (i in seq(length.out=nrow(scnsToMake))){
      if(is.na(scnsToMake$exists[i])){
        next
      }
      ret = delete(ssimObject,scenario=scnsToMake$scenarioId[i],force=T)
      cRow=scnsToMake[i,]
      scnsToMake[i,]=NA
      scnsToMake$name[i]=cRow$name;scnsToMake$projectId[i]=cRow$projectId;scnsToMake$order[i]=cRow$order
    }
    libScns = getScnSet(ssimObject)
  }
  if(summary|results){scnsToMake=subset(scnsToMake,is.na(exists))}
  if(nrow(scnsToMake)==0){
    fullScnSet=fullScnSet[order(fullScnSet$order),]
    fullScnSet$exists=NULL;fullScnSet$order=NULL
    return(fullScnSet)
  }
  if(results&(nrow(scnsToMake)>0)){
    stop(paste0("Could not find these scenarios in the ssimObject. To create them, set results=F: ",paste(scnsToMake$name,collapse=",")))
  }
  scnsToMake=scnsToMake[order(scnsToMake$order),]
  scnList = list()
  for(i in seq(length.out=nrow(scnsToMake))){
    cRow = scnsToMake[i,]
    if(!is.na(cRow$exists)){
      if (create){
        stop(paste0("Cannot overwrite existing scenario: Use overwrite=T: ",cRow$name)) 
      }
      scnList[[as.character(scnsToMake$scenarioId[i])]]=new("Scenario",ssimObject,project=cRow$projectId,id=cRow$scenarioId,scenarios=cRow)
    }else{
      obj=new("Scenario",ssimObject,project=cRow$projectId,name=cRow$name,sourceScenario=sourceScenario,scenarios=libScns)
      scnList[[as.character(.scenarioId(obj))]]=obj
    }
  }
  
  if(!summary){
    if((length(scnList)==1)&!forceElements){
      scnList=scnList[[1]]
    }
    return(scnList)  
  }
  
  scnSetOut=getScnSet(ssimObject)
  scnSetOut$exists=NULL
  idList=data.frame(scenarioId = as.numeric(names(scnList)),order=seq(1:length(scnList)))
  scnSetOut =merge(idList,scnSetOut,all.x=T)
  if(sum(is.na(scnSetOut$name))>0){
    stop("Something is wrong with scenario()")
  }
  
  scnSetOut=scnSetOut[order(scnSetOut$order),]
  scnSetOut$order=NULL
  return(scnSetOut)
} 
