# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
#' @include AAAClassDefinitions.R
NULL
# @name Project
# @rdname Project-class
setMethod(f='initialize',signature="Project",
    definition=function(.Object,ssimLibrary,name=NULL,id=NULL,projects,sourceProject=NULL){
    #ssimLibrary = myLibrary  #.project(myLibrary,project=1)#ssimLibrary(name= "C:/Temp/NewLibrary.ssim",session=devSsim)
    # id = NULL;name=NULL;projects=NULL;create=T;projects=NULL
      
    #This constructor is only called from projects - assume that ssimLibrary really is an object, projects is defined, and the project is not redundant.
    x=ssimLibrary

    #For fast processing - quickly return without system calls if projects exists and can be easily identified
    findPrj = projects

    if(!is.null(id)){
      cId = as.character(id)
      findPrj = subset(findPrj,id==cId)
    }
    if(!is.null(name)){
      pre = findPrj
      cName=name
      findPrj = subset(findPrj,name==cName)
      if(!is.null(id)&&(nrow(pre)>0)&&(nrow(findPrj)==0)){
        stop(paste0("The library already contains a project id ",id," with a different name ",pre$name))
      }
    }
    if(is.null(id)&is.null(name)&(nrow(findPrj)==1)){
      name = findPrj$name
    }
    if(is.null(id)&is.null(name)&(nrow(findPrj)>0)){
      name = "Project"
      cName = name
      findPrj = subset(findPrj,name==cName)
    }

    if(nrow(findPrj)==1){
      #Go ahead and create the Projects object without issuing system commands to make sure it is ok
      .Object@session=.session(x)
      .Object@filepath=.filepath(x)
      .Object@datasheetNames = .datasheets(x,scope="all",refresh=T)
      .Object@projectId = as.numeric(findPrj$id)
      return(.Object)
    }

    #Now go ahead to handle odder cases
    if(nrow(findPrj)>0){
      stop(paste0("The library contains more than one project called ",name,". Specify a project id: ",paste(findPrj$id,collapse=",")))
    }

    #If given an id for a project that does not yet exist, complain
    if(!is.null(id)){
      stop(paste0("The library does not contain project id ",id,". Please provide a name for the new project - the id will be assigned automatically by SyncroSim."))
    }

    #Create a new project
    if(is.null(name)){
      #allScenarios = scenario(.ssimLibrary(x))
      #if(nrow(allScenarios)==0){
      #  name = "Scenario1"
      #}else{
      #  name =paste0("Scenario",max(allScenarios$id)+1)
      #}
      name="Project"
    }
    if(!is.null(sourceProject)){
      #complain if source project does not exist.
      if(is.numeric(sourceProject)){
        
        if(!is.element(sourceProject,projects$id)){
          stop(paste0("sourceProject id ",sourceProject," not found in the library."))
        }
        sourcePID = sourceProject
      }else{
        if(!is.element(sourceProject,projects$name)){
          stop(paste0("sourceProject name ",sourceProject," not found in the library."))
        }
        sourcePID=projects$id[projects$name==sourceProject]
      }
      tt = command(list(copy=NULL,project=NULL,lib=.filepath(x),pid=sourcePID,name=name),.session(x))
    }else{
      tt = command(list(create=NULL,project=NULL,lib=.filepath(x),name=name),.session(x))
    }
    
    if(!grepl("Project ID is:",tt,fixed=T)){
      stop(tt)
    }

    id = as.numeric(strsplit(tt,": ")[[1]][2])

    .Object@session=.session(x)
    .Object@filepath=.filepath(x)
    .Object@datasheetNames = .datasheets(x,scope="all",refresh=T)
    .Object@projectId = as.numeric(id)
    return(.Object)
  }
)
#' Create or open a project or projects.
#'
#' If summary = FALSE, returns one or more \code{\link{Project}} objects representing a SyncroSim projects.
#' If summary = TRUE, returns project summary info.
#'
#' @details
#' For each element of project:
#' \itemize{
#'   \item {If element identifies an existing project: }{Returns the existing Project}
#'   \item {If element identifies more than one project: }{Error}
#'   \item {If element does not identify an existing project: }{Creates a new Project named element. Note that SyncroSim automatically assign an id to a new project.}
#' }
#'
#' @param ssimObject SsimLibrary/Scenario or character. An ssimObject containing a filepath to a library, or a filepath.
#' @param project Character, integer, or vector of these. Names or ids of one or more projects.
#' @param sourceProject Character or integer. If not NULL, new projects will be copies of the sourceProject.
#' @param summary Logical. If TRUE then return the project(s) in a dataframe with the projectId, name, description, owner, dateModified, readOnly. Default is TRUE if project=NULL and ssimObject is not Scenario/Project, FALSE otherwise.
#' @param forceElements Logical. If TRUE then returns a single project as a named list; otherwise returns a single project as a Project object. Applies only when summary=FALSE.
#' @return A \code{Project} object representing a SyncroSim project, or a dataframe of project names and descriptions.
#' @examples
#' #TODO – update examples
#' # Create a new project
#' myLibrary = ssimLibrary(name="stsim")
#' myProject = project(ssimLibrary=mySsimLibrary, project="My new project name")
#'
#' # Get a named list of existing projects
#' myProjects = project(myLibrary,summary=F) # Each element in the list is named by a character version of the project ID
#' names(myProjects)   # vector of the project names (using base R names function)
#' #TO DO: base R function names returns project id's, not names. Do we want to overwrite the base function?
#'
#' # Get an existing project. Assume that name uniquely identifies a single project - give error if not
#' myProject = myProjects[[1]]
#' myProject = project(myLibrary, project="My new project name")
#'
#' # Get/set the project properties - for now we can only set the name
#' name(myProject)
#' name(myProject) = "New project name" #  - committed to db immediately
#' ssimLibrary(myProject) # Returns a SyncroSimLibrary object for the project
#' @name project
# @rdname Project-class
#' @export
project <- function(ssimObject,project=NULL,sourceProject=NULL,summary=NULL,forceElements=F){
  #ssimObject= myLib;project=c(1,2);summary=F;forceElements=F
  if(!is.element(class(ssimObject),c("character","SsimLibrary","Project","Scenario"))){
    stop("ssimObject should be a filepath, or an SsimLibrary/Scenario object.")
  }
  #if ssimObject is a scenario, return the parent project
  if((class(ssimObject)=="Scenario")&is.null(project)){
    if(is.null(summary)){summary=F}
    project=.projectId(ssimObject)
  }
  #if ssimObject is a project, return it
  if((class(ssimObject)=="Project")&is.null(project)){
    if(is.null(summary)){summary=F}
    if(!summary){
      return(ssimObject)
    }
    project = .projectId(ssimObject)
  }
    
  if(class(ssimObject)=="character"){
    ssimObject=.ssimLibrary(ssimObject)
  }
  
  #get current project info
  tt = command(list(list=NULL,projects=NULL,csv=NULL,lib=.filepath(ssimObject)),.session(ssimObject))
  if(identical(tt,"saved")){
    projectSet = data.frame(id=NA,name=NA,exists=NA)
    projectSet=subset(projectSet,!is.na(id))
  }else{
    projectSet=.dataframeFromSSim(tt)
    names(projectSet)[names(projectSet)=="iD"]="id"
    projectSet$exists = T
  }

  #set summary default
  if(is.null(summary)){
    if(is.null(project)){summary=T}else{summary=F}
  }

  #projects aren't specified, simply return the project list without opening or creating any projects
  if(is.null(project)){
    if(summary){
      projectSet$exists = NULL
      return(projectSet)
    }else{
      project = projectSet$id
    }
  }
  
#project=c(1,2)
  #Now assume project is defined
  #distinguish existing projects from those that need to be made
  areIds = suppressWarnings(sum(as.character(as.numeric(project))!=as.character(project),na.rm=T))
  
  if(areIds){
    mergeBit = data.frame(id=as.numeric(as.character(project)))
  }else{
    mergeBit = data.frame(name=project,stringsAsFactors=F)
  }
  mergeBit$order = seq(1:length(project))
  fullProjectSet = merge(projectSet,mergeBit,all=T)
  fullProjectSet$name[is.na(fullProjectSet$name)]=paste0("project",fullProjectSet$id[is.na(fullProjectSet$name)])

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
    
  #make projects/project objects
  projectsToMake = subset(fullProjectSet,!is.na(order))
  if(summary){projectsToMake=subset(projectsToMake,is.na(exists))}
  projectsToMake=projectsToMake[order(projectsToMake$order),]
  projectList = list()
  for(i in seq(length.out=nrow(projectsToMake))){
    #i = 1
    cRow = projectsToMake[i,]
    if(!is.na(cRow$exists)){
      projectList[[as.character(projectsToMake$id[i])]]=new("Project",ssimObject,id=cRow$id,projects=projectSet)
      
    }else{
      projectList[[as.character(projectsToMake$id[i])]]=new("Project",ssimObject,name=cRow$name,projects=projectSet,sourceProject=sourceProject)
    }
  }
  
  if(!summary){
    if((length(projectList)==1)&!forceElements){
      projectList=projectList[[1]]
    }
    return(projectList)
    
  }
  tt = command(list(list=NULL,projects=NULL,csv=NULL,lib=.filepath(ssimObject)),.session(ssimObject))
  if(identical(tt,"saved")){
    projectSetOut = data.frame(id=NA,name=NA)
    projectSetOut=subset(projectSetOut,!is.na(id))
  }else{
    projectSetOut=.dataframeFromSSim(tt)
    names(projectSetOut)[names(projectSetOut)=="iD"]="id"
  }

  idList=data.frame(id = as.numeric(names(projectList)),order=seq(1:length(projectList)))
  projectSetOut =merge(idList,projectSetOut,all.x=T)
  if(sum(is.na(projectSetOut$name))>0){
    stop("Something is wrong with project()")
  }
  projectSetOut=projectSetOut[order(projectSetOut$order),]
  projectSetOut$order=NULL
  return(projectSetOut)
} 

setMethod('name', signature(ssimObject="Project"), function(ssimObject) {
  info = project(ssimObject,summary=T)
  return(info$name)
})

setMethod('projectId', signature(ssimObject="Project"), function(ssimObject) {
  return(ssimObject@projectId)
})

setReplaceMethod(
  f='name',
  signature="Project",
  definition=function(ssimObject,value){
    #x=myProject;value="New Name"
    tt = command(list(setprop=NULL,lib=.filepath(ssimObject),pid=.projectId(ssimObject),name=value),.session(ssimObject))
    if(!identical(tt,"saved")){
      stop(tt)
    }
    return (ssimObject)
  }
)



