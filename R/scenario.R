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
#' @slot pid The project id.
#' @slot name The scenario name.
#' @slot id The scenario id.
#' @name Scenario-class
#' @rdname Scenario-class
#' @export Scenario
Scenario <- setClass("Scenario", contains="SSimLibrary",representation(pid="numeric",name="character",id="numeric"))
# @name Scenario
# @rdname Scenario-class
setMethod(f="initialize",signature="Scenario",
    definition=function(.Object,ssimLibrary=NULL,project=NULL,name=NULL,id=NULL){
    #ssimLibrary = myLibrary  #.project(myLibrary,id=1)#ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
    # id = NULL;name="New Thing";project="My new project name"

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

    pid=project

    #x can be either a project or a library - but need a project in order to create a new scneario

    if(!is.null(pid)&(class(x)=="SSimLibrary")){
      if(class(pid)=="numeric"){
        x = .project(x,id=pid)
      }else{
        x = .project(x,name=pid)
      }
    }
    cScenarios = scenarios(x,names=T)

    #if given a library, can only open an existing scenario
    if(class(x)=="SSimLibrary"){
      if(!is.null(name)){
        cName =name
        cNames = subset(cScenarios,name==cName)
        if(!is.null(id)){
          cId = as.character(id)
          cNames = subset(cNames,id==cId)
        }
        if(nrow(cNames)==0){
          stop(paste0("Scenario ",name," (id=",id,") does not exist. A project is needed to create a new scenario."))
        }

        if(nrow(cNames)>1){
          stop(paste0("The library contains more than one scenario named '",name,"'. Please specify a project or scenario id. See scenarios(x,names=T) for options."))
        }
        id = cNames$id
      }

      if (!is.null(id)){
        if(!is.element(id,cScenarios$id)){
          stop(paste0("Scenario id ",id," does not exist. A project is needed to create a new scenario."))
        }
        name = cScenarios$name[cScenarios$id==id]
        pid = cScenarios$pid[cScenarios$id==id]
        x = .project(x,id=pid)
      }else{
        if(nrow(cScenarios)==1){
          name = cScenarios$name
          pid = cScenarios$pid
          id = cScenarios$id
          x = .project(x,id=pid)
        }else{
          stop(paste0("Cannot open or create a scenario. A project is needed to create a new scenario, and more information is required to open an existing scenario."))
        }
      }
    }

    #Now assume we have a project
    if(class(x)=="SSimLibrary") stop("Something is wrong")
    pid=.id(x) #If pid conflicts with project id, ignore pid.
    cPid = pid
    cScenarios = subset(cScenarios,pid==cPid)

    if(!is.null(id)){
      cId = id
      cScenarios = subset(cScenarios,id==cId)
    }

    if(!is.null(name)){
      cName = name
      cScenarios = subset(cScenarios,name==cName)
    }

    if(nrow(cScenarios)==1){
      name=cScenarios$name
      id = cScenarios$id
      pid = cScenarios$pid
    }

    if(nrow(cScenarios)>1){
      stop(paste0("The project contains more than one scenario named '",name,"'. Please specify a scenario id: ",paste(cScenarios$id,collapse=",")))
    }

    if(nrow(cScenarios)==0){
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
      tt = command(list(create=NULL,scenario=NULL,lib=.filepath(x),name=name,pid=pid),.session(x))
      id = as.numeric(strsplit(tt,": ")[[1]][2])
    }

    .Object@session=.session(x)
    .Object@filepath=.filepath(x)
    .Object@id = as.numeric(id)
    .Object@name = name
    .Object@pid = as.numeric(pid)
    return(.Object)
  }
)
#' Create or open a scenario
#'
#' Creates or opens an \code{\link{Scenario}} object representing a SyncroSim scenario.
#' @details
#'
#' \itemize{
#'   \item {If name/id/project uniquely identifies an existing scenario: }{Returns the existing Scenario}
#'   \item {If name/id/project uniquely identifies more than one existing scenario: }{Error}
#'   \item {If project is NULL, and name/id do not uniquely idenfity an existing scenario: }{Error}
#'   \item {If project is not NULL, name is NULL, and id/project do not idenfity an existing scenario: }{Creates a new Scenario called "Scenario". The id argument is ignored, as SyncroSim automatically assigns an id.}
#'   \item {If project is not NULL, name is not NULL, and name/id/project do not idenfity an existing scenario: }{Creates a new Scenario called <name>. The id argument is ignored, as SyncroSim automatically assigns an id.}
#' }
#'
#' @param ssimLibrary An SSimLibrary object or name, or an object that contains an SSimLibrary. If a name is given, the library will be opened using the default session.
#' @param project A Project object, project name, or project id.
#' @param name The scenario name.
#' @param id The scenario id.
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
scenario <- function(ssimLibrary=NULL,project=NULL,name=NULL,id=NULL) new("Scenario",ssimLibrary,project,name,id)
.scenario = scenario

setMethod('name', signature(x="Scenario"), function(x) {
  return(x@name)
})

setMethod('id', signature(x="Scenario"), function(x) {
  return(x@id)
})



