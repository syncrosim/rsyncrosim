# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Delete library, project, scenario, datasheet
#'
#' Deletes one or more items. Note this is irreversable.
#'
#' @param ssimObject SsimLibrary/Project/Scenario, or path to a library. 
#' @param project character string, numeric, or vector of these. One or more project names or ids. Note that project argument is ignored if ssimObject is a list. Note that integer ids are slightly faster.
#' @param scenario character string, numeric, or vector of these. One or more scenario names or ids. Note that scenario argument is ignored if ssimObject is a list. Note that integer ids are slightly faster.
#' @param datasheet character string or vector of these. One or more datasheet names.
#' @param force logical. If FALSE (default), user will be prompted to approve removal of each item.
#' @return A list of "saved" or failure messages for each item.
#' @examples
#' #TODO  - update examples
#' myLibrary = ssimLibrary(session=devSession)
#' myProject = project(myLibrary,project="a project")
#' project(myLibrary)
#' removeProject(myLibrary,project="a project")
#' project(myLibrary)
#'
#' @export
# Note delete supports character paths because sometimes we want to delete a library without updating it.
# Note delete supports project/scenario arguments because sometimes we want to delete objects without creating them.
setGeneric('delete',function(ssimObject,project=NULL,scenario=NULL,datasheet=NULL,force=F) standardGeneric('delete'))
#' @rdname delete
setMethod('delete', signature(ssimObject="character"), function(ssimObject,project,scenario,datasheet,force) {
  
  if(is.null(datasheet)&is.null(project)&is.null(scenario)){
    return(deleteLibrary(ssimObject,force))
  }else{
    if(ssimObject==SyncroSimNotFound(warn=F)){
      return(SyncroSimNotFound())
    }
    
    ssimObject=.ssimLibrary(ssimObject,create=F)
    return(delete(ssimObject,project,scenario,datasheet,force))
  }
})
#' @rdname delete
setMethod('delete', signature(ssimObject="SsimObject"), function(ssimObject,project,scenario,datasheet,force) {
  xProjScn=.getFromXProjScn(ssimObject,project=project,scenario=scenario,returnIds=T,convertObject=F,complainIfMissing=T)
  
  #expect to have a vector of valid project or scenario ids - checking already done
  x=xProjScn$ssimObject
  project=xProjScn$project
  scenario=xProjScn$scenario
  goal=xProjScn$goal
  
  if(goal=="library"){
    if(is.null(datasheet)){
      out=deleteLibrary(ssimObject,force)
    }else{
      datasheets=.datasheets(ssimObject)
      out = deleteDatasheet(datasheet,datasheets,cProj=NULL,cScn=NULL,cProjName=NULL,cScnName=NULL,force=force)
    }
    return(out)
  }
  
  if(goal=="project"){
    allProjects = xProjScn$projectSet
    
    if(!is.numeric(project)){
      stop("Error in delete: expect to have valid project ids.")
    }
    
    if(!is.null(datasheet)){
      if(is.element(class(ssimObject),c("Project","Scenario"))){
        datasheets=.datasheets(ssimObject,refresh=T)
      }else{
        datasheets=.datasheets(.project(ssimObject,project = project[1]))
      }
    }
    
    out = list()
    for(i in seq(length.out=length(project))){
      cProj = project[i]
      name=allProjects$name[allProjects$projectId==cProj]
      
      #If datasheets(s) specified delete them. Otherwise delete the projects.
      if(!is.null(datasheet)){
        outBit = deleteDatasheet(x,datasheet,datasheets,cProj=cProj,cScn=NULL,cProjName=name,cScnName=NULL,out=out,force=force)
      }else{
        if(force){
          answer="y"
        }else{
          answer <- readline(prompt=paste0("Do you really want to delete project ",name,"(",cProj,")? (y/n): "))
        }
        if(answer=="y"){
          outBit = command(list(delete=NULL,project=NULL,lib=.filepath(x),pid=cProj,force=NULL),.session(x))
        }else{
          outBit = "skipped"
        }
        
      }
      out[[as.character(cProj)]]=outBit
    }
    if(length(out)==1){
      out=out[[1]]
    }
    
    return(out)
  }
  
  if(goal=="scenario"){
    allScenarios = xProjScn$scenarioSet
    
    if(!is.numeric(scenario)){
      stop("Error in delete: expect to have valid scenario ids.")
    }
    
    if(!is.null(datasheet)){
      if(is.element(class(ssimObject),c("Scenario"))){
        datasheets=.datasheets(ssimObject,refresh=T)
        scenarioSet = scenario(.ssimLibrary(ssimObject),summary=T)
      }else{
        datasheets=.datasheets(.scenario(ssimObject,scenario = scenario[1]))
        scenarioSet = scenario(ssimObject,summary=T)
      }
    }
    out = list()
    for(i in seq(length.out=length(scenario))){
      cScn = scenario[i]
      name = allScenarios$name[allScenarios$scenarioId==cScn]
      if(!is.null(datasheet)){
        cProj = subset(scenarioSet,scenarioId==cScn)$projectId
        outBit = deleteDatasheet(datasheet,datasheets,cProj=cProj,cScn=cScn,cProjName="",cScnName=name,out=out,force=force)
      }else{
        if(force){
          answer="y"
        }else{
          answer <- readline(prompt=paste0("Do you really want to remove scenario ",name,"(",cScn,")? (y/n): "))
        }
        if(answer=="y"){
          outBit = command(list(delete=NULL,scenario=NULL,lib=.filepath(x),sid=cScn,force=NULL),.session(x))
        }else{
          outBit = "skipped"
        }
        out[[as.character(cScn)]]=outBit
      }
    }

    if(length(out)==1){
      out=out[[1]]
    }
    return(out)
  }
  stop("Error in delete().")
})

if(0){ #only support lists of objects when combined output is necessary.
  setMethod('delete', signature(ssimObject="list"), function(ssimObject,project,scenario,datasheet,force) {
    x = getIdsFromListOfObjects(ssimObject,project=project)
    ssimObject = x$ssimObject
    expecting=x$expecting
    if(expecting=="Project"){
      return(delete(ssimObject,project=x$objs,scenario=NULL,datasheet=datasheet,force=force))
    }
    if(expecting=="Scenario"){
      return(delete(ssimObject,project=NULL,scenario=x$objs,datasheet=datasheet,force=force))
    }
    
    if(expecting=="SsimLibrary"){
      out = list()
      for(i in seq(length.out=length(x$objs))){
        if(is.null(datasheet)){
          cObj = x$objs[i]
          out[[.filepath(cObj)]]=deleteLibrary(cObj,force)
        }else{
          out[[.filepath(cObj)]]=delete(cObj,project=NULL,scenario=NULL,datasheet=datasheet,force=force)
        }
      }
      return(out)
    }
    stop("Problem with ssimObject: should be a list of SsimLibraries/Projects/Scenarios or paths to libraries.")
  })
}
