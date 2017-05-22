#Internal helper - return uniquely identified and valid SyncroSim object
#' @export
.getFromXProjScn<-function(ssimObject,project=NULL,scenario=NULL,convertObject=T,returnIds=F,goal=NULL){
  #ssimObject=ssimObject;project=NULL;scenario=scenario;goal="scenario"
  #If x is scenario, ignore project and scenario arguments
  if(!is.element(class(ssimObject),c("character","SsimLibrary","Project","Scenario"))){
    stop("ssimObject should be a filepath, or an SsimLibrary/Scenario object.")
  }
  
  if(class(ssimObject)=="character"){
    ssimObject=.ssimLibrary(ssimObject)
  }
  
  #Check for conflicts between ssimObject and project/scenario.
  if(is.element(class(ssimObject),c("Project","Scenario"))&(!is.null(project))){
    warning("project argument is ignored when ssimObject is a Project/Scenario or list of these.")
    project=NULL
  }
  
  if(is.element(class(ssimObject),c("Scenario"))&(!is.null(scenario))){
    warning("scenario argument is ignored when ssimObject is a Scenario or list of these.")
    scenario=NULL
  }
  
  #If the goal is a project, return one or more, or complain
  if(!is.null(goal)&&(goal=="project")){
    #if ssimObject is a scenario, return the parent project 
    if((class(ssimObject)=="Scenario")){
      if(convertObject|!returnIds){
        ssimObject=new("Project",ssimObject,id=.projectId(ssimObject))
      }
    }
    if(is.element(class(ssimObject),c("Project","Scenario"))){
      if(returnIds){
        project=.projectId(ssimObject)
      }else{
        return(ssimObject)
      }
    }
    scenario=NULL
    #if not returned, need to get project
    
    #get current project info
    projectSet = getProjectSet(ssimObject)

    if(is.null(project)){
      if(nrow(projectSet)==0){
        if(returnIds){
          projectSet$exists=NULL
          return(list(ssimObject=ssimObject,project=NULL,scenario=NULL,projectSet=projectSet))
        }else{
          stop("No projects found in library.")
        }
      }
      project=projectSet$id
    }
    
    #Now assume project is defined
    #distinguish existing projects from those that need to be made
    areIds = is.numeric(project)

    if(areIds){
      mergeBit = data.frame(id=as.numeric(as.character(project)))
    }else{
      mergeBit = data.frame(name=project,stringsAsFactors=F)
    }
    mergeBit$order = seq(1:length(project))
    fullProjectSet = merge(projectSet,mergeBit,all=T)
    missingNames = subset(fullProjectSet, is.na(fullProjectSet$exists)&(!is.na(fullProjectSet$order))&is.na(fullProjectSet$name))
    if(areIds&(nrow(missingNames)>0)){
      stop("Project ids (",paste(missingNames$id,collapse=",") ,") not found in ssimObject. To make new projects, please provide names (as one or more character strings) to the project argument. SyncroSim will automatically assign project ids.")
    }

    #Stop if an element of project corresponds to more than one existing row of the project list
    if(!areIds){
      checkDups = subset(fullProjectSet,!is.na(order))
      dupNames = subset(as.data.frame(table(checkDups$name)),Freq>1)
      if(nrow(dupNames)>0){
        #report the first error only
        cName = dupNames$Var1[1]
        cIds = checkDups$id[checkDups$name==cName]
        stop(paste0("The library contains more than one project called ",cName,". Specify a project id: ",paste(cIds,collapse=",")))
      }
    }  
    
    smallProjectSet=subset(fullProjectSet,!is.na(order))
    if(!returnIds){
      if(nrow(smallProjectSet)>1){
        stop("Cannot uniquely identify a project from ssimObject/project arguments.")
      }
      if(!smallProjectSet$exists){
        stop("Project ",project," not found in the ssimObject.")
      }
      return(new("Project",ssimObject,id=project,projects=fullProjectSet))
    }
    if(sum(is.na(smallProjectSet$exists))==0){
      project=smallProjectSet$id
    }
    
    return(list(ssimObject=ssimObject,project=project,scenario=scenario,projectSet=fullProjectSet))
    
  }

  #if goal is scenario, and we have one, return immediately
  if(!is.null(goal)&&(goal=="scenario")){
    if(is.element(class(ssimObject),c("Scenario"))){
      if(returnIds){
        project=.projectId(ssimObject)
        scenario=.scenarioId(ssimObject)
      }else{
        return(ssimObject)
      }
    }

    if(class(ssimObject)=="Project"){
      project=.projectId(ssimObject)
    }
    
    scnSet = getScnSet(ssimObject)
    if(!is.null(project)){
      scnSet=subset(scnSet,is.element(pid,project))
    }
    if(!is.null(scenario)&&is.numeric(scenario)){
      scnSet=subset(scnSet,is.element(id,scenario))    
    }  
    if(is.null(scenario)){
      if(nrow(scnSet)==0){
        if(returnIds){
          scnSet$exists=NULL
          return(list(ssimObject=ssimObject,project=NULL,scenario=NULL,scenarioSet=scnSet))
        }else{
          stop("No scenarios found in ssimObject.")
        }
      }
      scenario=scnSet$id
    }
    
    #Now assume scenario is defined
    #distinguish existing scenarios from those that need to be made
    areIds = is.numeric(scenario)
    if(areIds){
      mergeBit = data.frame(id=scenario)
    }else{
      mergeBit = data.frame(name=scenario,stringsAsFactors=F)
    }
    if(!is.null(project)){
      mergeBit$pid = project
    }
    mergeBit$order = seq(1:length(scenario))
    fullScnSet = merge(scnSet,mergeBit,all=T)
    if(areIds){
      makeProblems = subset(fullScnSet,is.na(name)&!is.na(order))
      if(nrow(makeProblems)>0){
        stop("Scenario ids (",paste(makeProblems$id,collapse=",") ,") not found in ssimObject. To make new scenarios, please provide names (as one or more character strings) to the scenario argument. SyncroSim will automatically assign scenario ids.")
      }
    }

    #For scenarios that need to be made, assign project or fail  
    makeSum = sum(!is.na(fullScnSet$order)&is.na(fullScnSet$exists))
    if(makeSum>0){
      if(is.null(project)){
        allProjects = project(ssimObject,summary=T)
        if(nrow(allProjects)>1){
          stop("Can't create new scenarios because there is more than one project in the ssimObject. Please specify the Project ssimObject to which new scenarios should belong.")
        }
        if(nrow(allProjects)==0){
          obj = project(ssimObject,project="project1")
          project = .projectId(obj)
        }else{
          project = allProjects$id
        }
      }
      if(is.null(project)){
        stop("Something is wrong")
      }
      fullScnSet$pid[!is.na(fullScnSet$order)&is.na(fullScnSet$exists)]=project
    }
    
    #Stop if an element of scenarios corresponds to more than one existing row of the scenario list
    if(!areIds){
      checkDups = subset(fullScnSet,!is.na(order))
      dupNames = subset(as.data.frame(table(checkDups$name)),Freq>1)
      if(nrow(dupNames)>0){
        #report the first error only
        cName = dupNames$Var1[1]
        cIds = checkDups$id[checkDups$name==cName]
        stop(paste0("The ssimObject contains more than one scenario called ",cName,". Specify a scenario id: ",paste(cIds,collapse=",")))
      }
    }  
    
    smallScenarioSet=subset(fullScnSet,!is.na(order))
    if(!returnIds){
      if(nrow(smallScenarioSet)>1){
        stop("Cannot uniquely identify a scenario from ssimObject/scenario arguments.")
      }
      if(!smallScenarioSet$exists){
        stop("Scenario ",scenario," not found in the ssimObject.")
      }
      return(new("Scenario",ssimObject,id=scenario,scenarios=fullScnSet))
    }
    if(sum(is.na(smallScenarioSet$exists))==0){
      scenario=smallScenarioSet$id
    }
    
    return(list(ssimObject=ssimObject,project=project,scenario=scenario,scenarioSet=fullScnSet))
    
  }

  if(class(x)=="Scenario"){
    if(!is.null(scenario)){
      if(is.character(scenario)&&!identical(scenario,name(x))){
        stop(paste0("Scenario name mismatch: ",name(x),",",scenario))
      }
      if(is.numeric(scenario)&&!identical(scenario,scenarioId(x))){
        stop(paste0("Scenario id mismatch: ",scenarioId(x),",",scenario))
      }
    }
    return(x)
  }
  if(is.character(x)){
    x = .ssimLibrary(name=x)
  }
  if(is.null(project)&is.null(scenario)){
    return(x)
  }

  if(!is.null(scenario)){
    if(class(scenario)=="Scenario"){
      return(scenario)
    }
    return(.scenario(x,project,scenario=scenario))
  }

  if(class(x)=="Project"){
    if(!is.null(project)){
      if(is.character(project)&&!identical(name(x),project)){
        stop(paste0("Project name mismatch: ",name(x),",",project))
      }
      if(is.numeric(project)&&!identical(projectId(x),project)){
        stop(paste0("Project id mismatch: ",projectId(x),",",project))
      }
    }
    return(x)
  }

  if(!is.null(project)){
    if(class(project)=="Project"){
      return(project)
    }
    return(.project(x,project=project))    
  }
  stop(paste0("Could not identify a SsimLibrary, Project or Scenario from x, project, and scenario arguments."))
}
