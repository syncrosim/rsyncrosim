# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

setMethod(f='initialize',signature="Project",definition=function(.Object,ssimLibrary,name=NULL,id=NULL,projects=NULL,sourceProject=NULL){
          
    #This constructor is only called from projects and getFromXProjScn - assume that ssimLibrary really is an object, projects is defined, and the project is not redundant.
    x=ssimLibrary

    #For fast processing - quickly return without system calls if projects exists and can be easily identified
    if(is.null(projects)){projects=getProjectSet(x)}
    findPrj = projects

    if(!is.null(id)){
      findPrj = subset(findPrj,projectId==id)
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
      if(!is.null(sourceProject)){
        warning("Project ", name," (",findPrj$projectId,") already exists, so sourceProject argument was ignored.")
      }
      #Go ahead and create the Projects object without issuing system commands to make sure it is ok
      .Object@session=.session(x)
      .Object@filepath=.filepath(x)
      .Object@datasheetNames = .datasheets(x,scope="all",refresh=T)
      .Object@projectId = as.numeric(findPrj$projectId)
      return(.Object)
    }

    #Now go ahead to handle odder cases
    if(nrow(findPrj)>0){
      stop(paste0("The library contains more than one project called ",name,". Specify a project id: ",paste(findPrj$projectId,collapse=",")))
    }

    #If given an id for a project that does not yet exist, complain
    if(!is.null(id)){
      stop(paste0("The library does not contain project id ",id,". Please provide a name for the new project - the id will be assigned automatically by SyncroSim."))
    }

    #Create a new project
    if(is.null(name)){
      name="Project"
    }
    if(!is.null(sourceProject)){
      #complain if source project does not exist.
      sourcePID = NA
      slib = .filepath(x)
      if(class(sourceProject)=="numeric"){
        
        if(!is.element(sourceProject,projects$projectId)){
          stop(paste0("sourceProject id ",sourceProject," not found in the library."))
        }
        sourcePID = sourceProject
      }
      if(class(sourceProject)=="character"){
        if(!is.element(sourceProject,projects$name)){
          stop(paste0("sourceProject name ",sourceProject," not found in the library."))
        }
        sourcePID=projects$projectId[projects$name==sourceProject]
      }
      if(class(sourceProject)=="Project"){
        slib=.filepath(sourceProject)
        sourcePID = .projectId(sourceProject)
      } 
   
      if(is.na(sourcePID)){
        stop("Source project must be a number, project name, or Project object.")
      }
      
      if(name=="GetSourceCopyCopyCopy"){
        sourceProjectName = subset(projects,projectId==sourcePID)$name
        
        copyName = paste(sourceProjectName,"- Copy")
        if(!is.element(copyName,projects$name)){
          name = copyName
        }else{
          done=F
          count=0
          while(!done){
            count=count+1
            cName =paste0(copyName,count)
            if(!is.element(cName,projects$name)){
              name=cName
              done=T
            }
          }
        }
      }
      tt = command(list(copy=NULL,project=NULL,slib=slib,tlib=.filepath(x),pid=sourcePID,name=name),.session(x))      
    }else{
      tt = command(list(create=NULL,project=NULL,lib=.filepath(x),name=name),.session(x))
    }
    
    if(!grepl("Project ID is:",tt[1],fixed=T)){
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
#' @param project Character, integer, or vector of these. Names or ids of one or more projects. Note that integer ids are slightly faster.
#' @param sourceProject Character, integer, or Project object. If not NULL, new projects will be copies of the sourceProject.
#' @param summary Logical. If TRUE then return the project(s) in a dataframe with the projectId, name, description, owner, dateModified, readOnly. Default is TRUE if project=NULL and ssimObject is not Scenario/Project, FALSE otherwise.
#' @param forceElements Logical. If TRUE then returns a single project as a named list; otherwise returns a single project as a Project object. Applies only when summary=FALSE.
#' @param overwrite Logical. If TRUE an existing Project will be overwritten.
#' @return A \code{Project} object representing a SyncroSim project, or a dataframe of project names and descriptions.
#' @examples
#' \dontrun{
#' #Load a Library and create a new Project
#' myLibrary = ssimLibrary(name="stsim")
#' myProject = project(ssimLibrary=myLibrary, project="My new project name")
#'
#' #Get a named list of existing Projects.
#' #Each element in the list is named by a character version of the Project ID.
#' myProjects = project(myLibrary,summary=F) 
#' names(myProjects)   # vector of the project ids
#'
#' #Get an existing Project. 
#' myProject = myProjects[[1]]
#' myProject = project(myLibrary, project="My new project name")
#'
#' #Get/set the project properties
#' name(myProject)
#' name(myProject) = "New project name"
#' }
#' @name project
#' @export
project <- function(ssimObject=NULL,project=NULL,sourceProject=NULL,create=F,summary=NULL,forceElements=F,overwrite=F){
  
  if(create){
    warning("create argument deprecated and no longer required.")
    if (overwrite){create=F}
  } 
    
  if((class(ssimObject)=="character")&&(ssimObject==SyncroSimNotFound(warn=F))){
    return(SyncroSimNotFound())
  }
  
  if (is.null(ssimObject)){
    e = ssimEnvironment()
    ssimObject = ssimLibrary(e$LibraryFilePath)
    project=as.integer(e$ProjectId)
  }
  
  #if ssimObject is a scenario or project, return the project
  if(is.element(class(ssimObject),c("Scenario","Project"))&is.null(project)){
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
      if(is.null(project)){
        if(is.null(sourceProject)){
          summary=T
        }else{
          summary=F
          project = "GetSourceCopyCopyCopy"
        }
      }else{
        summary=F
      }
    }
    convertObject=T
    returnIds=T
  }
  
  xProjScn  =.getFromXProjScn(ssimObject,project=project,scenario=NULL,convertObject=convertObject,returnIds=returnIds,goal="project",complainIfMissing=F)
  
  if(class(xProjScn)=="Project"){
    if (create){
      stop(paste0("Cannot overwrite existing project.  Use overwrite=T.",project)) 
    }
    if (!overwrite){
      return(xProjScn)      
    }
  }
  
  if(class(xProjScn)!="list"){
    stop("something is wrong")
  }
  ssimObject=xProjScn$ssimObject
  project=xProjScn$project
  allProjects = xProjScn$projectSet
  if(is.element("order",names(allProjects))){
    projectSet=subset(allProjects,!is.na(order))
  }else{
    if(nrow(allProjects)>0){
      allProjects$order=seq(1,nrow(allProjects))
    }
    projectSet=allProjects
  }
  if(nrow(projectSet)==0){
    if(summary){
      projectSet$exists = NULL
      projectSet$order=NULL
      return(projectSet)
    }else{
      stop("Error in project(): No projects to get or make.") 
    }
  }
  #if all projects exist and summary, simply return summary
  if((sum(is.na(projectSet$exists))==0)&summary){
    projectSet=subset(projectSet,!is.na(order))
    projectSet=projectSet[order(projectSet$order),]
    projectSet[projectSet$readOnly == "FALSE", "readOnly"] <- "No"
    projectSet[projectSet$readOnly == "TRUE", "readOnly"] <- "Yes"
    projectSet$exists = NULL
    projectSet$order=NULL
    return(projectSet)
  }

  #Now assume project is defined
  #distinguish existing projects from those that need to be made
  areIds = is.numeric(project)
  
  #make projects/project objects
  projectsToMake = projectSet
  if(summary){projectsToMake=subset(projectsToMake,is.na(exists))}
  projectsToMake=projectsToMake[order(projectsToMake$order),]
  projectList = list()
  
  for(i in seq(length.out=nrow(projectsToMake))){
    cRow = projectsToMake[i,]
    projExists = !is.na(cRow$exists)
    
    if (projExists){
      if(create){
        stop(paste0("Cannot overwrite existing project.  Use overwrite=T: ",cRow$name)) 
      }else if (overwrite){
        command(list(delete=NULL,project=NULL,lib=.filepath(ssimObject),pid=cRow$projectId,force=NULL),.session(ssimObject))
        allProjects[i, "exists"] <- NA
        projectsToMake[i, "exists"] <- NA        
      }
    }
  } 
  
  for(i in seq(length.out=nrow(projectsToMake))){
    cRow = projectsToMake[i,]
    projExists = !is.na(cRow$exists)
    
    if(projExists){
      projectList[[as.character(projectsToMake$projectId[i])]]=new("Project",ssimObject,id=cRow$projectId,projects=subset(allProjects,!is.na(exists)),sourceProject=sourceProject)
    }else{
      obj=new("Project",ssimObject,name=cRow$name,projects=subset(allProjects,!is.na(exists)),sourceProject=sourceProject)
      projectList[[as.character(.projectId(obj))]]=obj
    }
  }
  
  if(!summary){
    if((length(projectList)==1)&!forceElements){
      projectList=projectList[[1]]
    }
    return(projectList)
    
  }
  projectSetOut=getProjectSet(ssimObject)
  projectSetOut$exists = NULL
  idList=data.frame(id = as.numeric(names(projectList)),order=seq(1:length(projectList)))
  projectSetOut =merge(idList,projectSetOut,all.x=T)
  if(sum(is.na(projectSetOut$name))>0){
    stop("Something is wrong with project()")
  }
  projectSetOut=projectSetOut[order(projectSetOut$order),]
  projectSetOut$order=NULL
  return(projectSetOut)
}
