# Author: Josie Hughes
# Date : November 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
#' @include ssimLibrary.R
#' @include project.R
NULL
#' SyncroSim Scenario class
#'
#' \code{Scenario} object representing a SyncroSim Project.
#'
#' @seealso See \code{\link{scenario}} for options when creating or loading an SyncroSim Scenario.
#' @slot session The session associated with the library.
#' @slot filepath The path to the library on disk.
#' @slot datasheetNames Names and scope of all datasheets in library.
#' @slot pid The project id.
#' @slot name The scenario name.
#' @slot id The scenario id.
#' @slot parentId For a result scenario, this is the id of the parent scenario. 0 indicates this is not a result scenario.
#' @name Scenario-class
#' @rdname Scenario-class
#' @export Scenario
Scenario <- setClass("Scenario", contains="SSimLibrary",representation(pid="numeric",name="character",id="numeric",parentId="numeric"))
# @name Scenario
# @rdname Scenario-class
setMethod(f="initialize",signature="Scenario",
    definition=function(.Object,ssimLibrary=NULL,project=NULL,name=NULL,id=NULL,create=T,scenarios=NULL,sourceScenario=NULL){
    #ssimLibrary = myLibrary  #.project(myLibrary,id=1)#ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
    # id=NULL;name=NULL;project=NULL;scenarios=NULL;create=T;sourceScenario=NULL
    if(is.character(id)){id = as.numeric(id)}

    .Object@parentId = 0
    x=NULL
    if(!is.null(ssimLibrary)){
      x=ssimLibrary
    }else{
      x=project
    }
    if(is.null(x)){
      stop("Specify a library and (optional) project to create or open a scenario.")
    }
    if(is.character(x)){
      x=.ssimLibrary(name=x)
    }

    #For fast processing - quickly return without system calls if scenario exists and can be easily identified
    if(is.null(scenarios)){
      scenarios = .scenarios(x,names=T)
    }
    findScn = scenarios
    if(!is.null(name)){
      cName=name
      findScn = subset(findScn,name==cName)
    }
    if(!is.null(id)){
      cId = as.character(id)
      findScn = subset(findScn,id==cId)
    }
    pid=project
    if(class(x)=="Project"){
      pid = .id(x)
    }
    if(class(project)=="Project"){
      pid = .id(project)
    }
    if(!is.null(pid)){
      cPid = as.character(pid)
      findScn = subset(findScn,pid==cPid)
    }
    cProjects=NULL
    if((nrow(findScn)!=1)&&(class(pid)=="character")){
      cProjects = projects(x,names=T)
      findProject = subset(cProjects,name==project)
      findScn = subset(findScn,is.element(pid,findProject$id))
      if(nrow(findProject)>0){
        pid = as.numeric(findProject$id)
      }
    }
    #If found only one, open it.
    if(nrow(findScn)==1){name=findScn$name}

    if(is.null(id)&is.null(name)&(nrow(findScn)>0)){
        name = "Scenario"
        cName=name
        findScn = subset(findScn,name==cName)
    }

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
      .Object@id = as.numeric(findScn$id)
      .Object@name = findScn$name
      .Object@pid = as.numeric(findScn$pid)
      return(.Object)
    }

    #Now go ahead to handle odder cases
    #x can be either a project or a library - but need a project in order to create a new scenario

    if(create){
      if(!is.null(pid)&(class(x)=="SSimLibrary")){
        if(length(pid)>1){
          stop(paste0("The library contains more than one project called ",project,". Specify a project id:",paste(pid,collapse=",")))
        }
        if(class(pid)=="numeric"){
          x = .project(x,id=pid)
        }else{
          x = .project(x,name=pid)
        }
      }
    }

    #if given a library, can only open an existing scenario
    if((class(x)=="SSimLibrary")||!create||(nrow(findScn)>0)){
      if(nrow(findScn)==0){
        if(!create){
          stop(paste0("Scenario ",name," (id=",id,") does not exist. Provide a project and set create=T to create a new scenario."))
        }
        if(is.null(cProjects)){
          cProjects = projects(x,names=T)
        }
        if(nrow(cProjects)>1){
          stop(paste0("Scenario ",name," (id=",id,") does not exist. Provide a project to create a new scenario."))
        }else{
          x = project(x)
        }
      }
      if(nrow(findScn)>1){
        stop(paste0("More than one scenario was identified. Please provide more information. See scenarios(x,names=T) for options."))
      }
      if(class(x)!="Project"){
        stop("Something is wrong. Can't identify a unique existing scenario, and need a project to create a new scenario")
      }
    }

    #Now assume we have a project, and are permitted to create a new scenario
    pid=.id(x) #If pid conflicts with project id, ignore pid.
    if(nrow(findScn)>0){stop("Something is wrong")}

    #If given an id for a scenario that does not yet exist, complain
    if(!is.null(id)){
      stop(paste0("The library does not contain scenario id ",id,". Please provide a name for the new scenario - the id will be assigned automatically by SyncroSim."))
    }

    #Create a new scenario
    if(is.null(name)){
      #allScenarios = scenarios(.ssimLibrary(x),names=T)
      #if(nrow(allScenarios)==0){
      #  name = "Scenario1"
      #}else{
      #  name =paste0("Scenario",max(allScenarios$id)+1)
      #}
      name="Scenario"
    }
    if(is.null(sourceScenario)){
      tt = command(list(create=NULL,scenario=NULL,lib=.filepath(x),name=name,pid=pid),.session(x))
    }else{
      if(is.character(sourceScenario)){
        findSource = subset(scenarios,name==sourceScenario)
      }else{
        findSource = subset(scenarios,id==sourceScenario)
      }
      findSource=subset(findSource,isResult=No)
      if(nrow(findSource)==0){
        stop("Source scenario ",sourceScenario," not found.")
      }
      if(nrow(findSource)>1){
        stop("There is more than one scenario named ",sourceScenario,". Specify an id: ",paste(findSource$id,collapse=","))
      }
      tt = command(list(copy=NULL,scenario=NULL,lib=.filepath(x),name=name,sid=findSource$id),.session(x))
    }
    id = as.numeric(strsplit(tt,": ")[[1]][2])

    .Object@session=.session(x)
    .Object@filepath=.filepath(x)
    .Object@datasheetNames = .datasheets(x,refresh=T,scope="all")
    .Object@id = as.numeric(id)
    .Object@name = name
    .Object@pid = as.numeric(pid)
    return(.Object)
  }
)
#' Create or open a scenario
#'
#' Creates or opens a \code{\link{Scenario}} object representing a SyncroSim scenario.
#' @details
#'
#' \itemize{
#'   \item {If name/id/project uniquely identifies an existing scenario: }{Returns the existing Scenario}
#'   \item {If name/id/project uniquely identifies more than one existing scenario: }{Error}
#'   \item {If project is NULL, and name/id do not uniquely idenfity an existing scenario: }{Error}
#'   \item {If project is not NULL, name is NULL, and id/project do not idenfity an existing scenario: }{Creates a new Scenario called "Scenario". The id argument is ignored, as SyncroSim automatically assigns an id. If sourceScenario is not NULL the new scenario will be a copy of sourceScenario.}
#'   \item {If project is not NULL, name is not NULL, and name/id/project do not idenfity an existing scenario: }{Creates a new Scenario called <name>. The id argument is ignored, as SyncroSim automatically assigns an id. If sourceScenario is not NULL the new scenario will be a copy of sourceScenario.}
#' }
#'
#' @param ssimLibrary An SSimLibrary object or name, or an object that contains an SSimLibrary. If a name is given, the library will be opened using the default session.
#' @param project A Project object, project name, or project id.
#' @param name The scenario name.
#' @param id The scenario id.
#' @param create If TRUE, create scenario if one does not exist. If FALSE, only return an existing scenario
#' @param scenarios A dataframe of existing scenarios produced by scenarios(). Use to speed processing.
#' @param sourceScenario The name or id of a scenario to copy.
#' @return A \code{Scenario} object representing a SyncroSim scenario.
#' @examples
#' # Create a new default scenario
#' myLibrary = ssimLibrary(model="stsim",name="stsim")
#' myProject = project(myLibrary) #If no name is given, creates a project named "Project".
#' myScenario = scenario(myProject)
#'
#' @name scenario
# @rdname Scenario-class
#' @export
scenario <- function(ssimLibrary=NULL,project=NULL,name=NULL,id=NULL,create=T,scenarios=NULL,sourceScenario=NULL) new("Scenario",ssimLibrary,project,name,id,create,scenarios,sourceScenario)

setMethod('name', signature(x="Scenario"), function(x) {
  return(x@name)
})
setReplaceMethod(
  f="name",
  signature="Scenario",
  definition=function(x,value){
    #x=myScenario;value="New Name"
    tt = command(list(rename=NULL,scenario=NULL,lib=.filepath(x),sid=.id(x),name=value),.session(x))
    if(!identical(tt,"Success!")){
      stop(tt)
    }
    x@name = value
    return (x)
  }
)

setMethod('id', signature(x="Scenario"), function(x) {
  return(x@id)
})

#' The write status of a Scenario
#'
#' Whether or not the scenario is readOnly
#'
#' @param x An Scenario object.
#' @return TRUE or FALSE
#' @export
setGeneric('readOnly',function(x) standardGeneric('readOnly'))
setMethod('readOnly', signature(x="Scenario"), function(x) {
  #x=myScenario
  stop("not done yet")
  return(x)
})

#' The parent scenario id of a SyncroSim Scenario.
#'
#' The id of the parent of a SyncroSim results scenario.
#' 0 if x is not a results scenario.
#'
#' @param x A Scenario object.
#' @export
setGeneric('parentId',function(x) standardGeneric('parentId'))
setMethod('parentId', signature(x="Scenario"), function(x) {
  return(x@parentId)
})


#' The pid of a SyncroSim Scenario.
#'
#' The project id of a SyncroSim Scenario
#'
#' @param x An Scenario object.
#' @export
setGeneric('pid',function(x) standardGeneric('pid'))
setMethod('pid', signature(x="Scenario"), function(x) {
  return(x@pid)
})
#' The project id of a SyncroSim Scenario.
#'
#' The project id of a SyncroSim Scenario
#'
#' @param x An Scenario object.
#' @export
projectId = pid

#' @describeIn ssimLibrary Get the SSimLibrary associated with a SyncroSim Scenario.
setMethod('ssimLibrary', signature(model="Scenario"), function(model) {
  #model=cScn
  out = .ssimLibrary(name=.filepath(model),session=.session(model))
  return(out)
})




