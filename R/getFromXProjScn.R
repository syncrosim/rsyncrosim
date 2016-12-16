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
      if(is.numeric(scenario)&&!identical(scenario,id(x))){
        stop(paste0("Scenario id mismatch: ",id(x),",",scenario))
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
    if(class(scenario)=="character"){
      return(.scenario(x,project,name=scenario))
    }
    if(class(scenario)=="numeric"){
      return(.scenario(x,project,id=scenario))
    }
  }

  if(class(x)=="Project"){
    if(!is.null(project)){
      if(is.character(project)&&!identical(name(x),project)){
        stop(paste0("Project name mismatch: ",name(x),",",project))
      }
      if(is.numeric(project)&&!identical(id(x),project)){
        stop(paste0("Project id mismatch: ",id(x),",",project))
      }
    }
    return(x)
  }

  if(!is.null(project)){
    if(class(project)=="Project"){
      return(project)
    }
    if(class(project)=="character"){
      return(.project(x,name=project))
    }
    if(class(project)=="numeric"){
      return(.project(x,id=project))
    }
  }
  stop(paste0("Could not identify a SSimLibrary, Project or Scenario from x, project, and scenario arguments."))
}
