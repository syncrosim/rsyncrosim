#Internal helper - return uniquely identified and valid SyncroSim object
#' @export
.getFromXProjScn<-function(x,project=NULL,scenario=NULL){
  #x=myLibrary;scenario=myScenario
  #If x is scenario, ignore project and scenario arguments
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
