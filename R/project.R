# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
#' @include ssimLibrary.R
NULL
#' SyncroSim Project class
#'
#' \code{Project} object representing a SyncroSim Project.
#'
#' @seealso See \code{\link{project}} for options when creating or loading an SyncroSim Project.
#' @slot session The session associated with the library.
#' @slot filepath The path to the library on disk.
#' @slot datasheetNames Names and scopes of datasheets in the library.
#' @slot name The project name
#' @slot id The project id
#' @name Project-class
#' @rdname Project-class
#' @export Project
Project <- setClass("Project", contains="SSimLibrary",representation(name="character",id="numeric"))
# @name Project
# @rdname Project-class
setMethod(f='initialize',signature="Project",
    definition=function(.Object,ssimLibrary,name=NULL,id=NULL,create=T,projects=NULL){
    #ssimLibrary = myLibrary  #.project(myLibrary,id=1)#ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
    # id = NULL;name=NULL;projects=NULL;create=T;projects=NULL
    x=ssimLibrary
    if(is.character(x)){
      x=.ssimLibrary(name=x)
    }

    #For fast processing - quickly return without system calls if projects exists and can be easily identified
    if(is.null(projects)){
      projects = .projects(x,names=T)
    }
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
      .Object@id = as.numeric(findPrj$id)
      .Object@name = findPrj$name
      return(.Object)
    }

    #Now go ahead to handle odder cases
    if(nrow(findPrj)>0){
      stop(paste0("The library contains more than one project called ",name,". Specify a project id:",paste(findPrj$id,collapse=",")))
    }

    if(!create){
      stop(paste0("The library does not contain a project called ",name," (",id,"). Set create=T to make one."))
    }

    #If given an id for a project that does not yet exist, complain
    if(!is.null(id)){
      stop(paste0("The library does not contain project id ",id,". Please provide a name for the new project - the id will be assigned automatically by SyncroSim."))
    }

    #Create a new project
    if(is.null(name)){
      #allScenarios = scenarios(.ssimLibrary(x),names=T)
      #if(nrow(allScenarios)==0){
      #  name = "Scenario1"
      #}else{
      #  name =paste0("Scenario",max(allScenarios$id)+1)
      #}
      name="Project"
    }
    tt = command(list(create=NULL,project=NULL,lib=.filepath(x),name=name),.session(x))
    if(!grepl("Project ID is:",tt,fixed=T)){
      stop(tt)
    }

    id = as.numeric(strsplit(tt,": ")[[1]][2])

    .Object@session=.session(x)
    .Object@filepath=.filepath(x)
    .Object@datasheetNames = .datasheets(x,scope="all",refresh=T)
    .Object@id = as.numeric(id)
    .Object@name = name
    return(.Object)
  }
)
#' Create or open a project.
#'
#' Creates or opens an \code{\link{Project}} object representing a SyncroSim project.
#'
#' @details
#' \itemize{
#'   \item {If name/id uniquely identify an existing project: }{Returns the existing Project}
#'   \item {If name/id identify more than one project: }{Error}
#'   \item {If name/id don't identify an existing project, and name is not specified: }{Creates a new Project called "Project". The id argument is ignored, as SyncroSim automatically assigns an id.}
#'   \item {If name/id don't identify an existing project, and name is specified: }{Creates a new Project called <name>. The id argument is ignored, as SyncroSim automatically assigns an id.}
#' }
#'
#' @param ssimLibrary An SSimLibrary object, representing the library that contains the project.
#' @param name The project name.
#' @param id The project id.
#' @param create If TRUE, create project if one does not exist. If FALSE, only return an existing project
#' @param projects A dataframe of existing projects produced by projects(). Use to speed processing.
#' @return A \code{Project} object representing a SyncroSim project.
#' @examples
#' # Create a new project
#' myLibrary = ssimLibrary(model="stsim",name="stsim")
#' myProject = project(myLibrary) #If no name is given, creates a project named "Project<ID>".
#' myProject = project(ssimLibrary=mySsimLibrary, name="My new project name")
#'
#' # Get a named list of existing projects
#' myProjects = projects(myLibrary) # Each element in the list is named by a character version of the project ID
#' names(myProjects)   # vector of the project names (using base R names function)
#' #TO DO: base R function names returns project id's, not names. Do we want to overwrite the base function?
#'
#' # Get an existing project. Assume that name uniquely identifies a single project - give error if not
#' myProject = myProjects[[1]]
#' myProject = project(myLibrary, name="My new project name")
#'
#' # Get/set the project properties - for now we can only set the name
#' name(myProject)
#' name(myProject) = "New project name" #  - committed to db immediately
#' ssimLibrary(myProject) # Returns a SyncroSimLibrary object for the project
#' @name project
# @rdname Project-class
#' @export
project <- function(ssimLibrary,name=NULL,id=NULL,create=T,projects=NULL) new("Project",ssimLibrary,name,id,create,projects)

setMethod('name', signature(x="Project"), function(x) {
  return(x@name)
})

setMethod('id', signature(x="Project"), function(x) {
  return(x@id)
})

setReplaceMethod(
  f='name',
  signature="Project",
  definition=function(x,value){
    #x=myProject;value="New Name"
    tt = command(list(setprop=NULL,lib=.filepath(x),pid=.id(x),name=value),.session(x))
    if(!identical(tt,"Success!")){
      stop(tt)
    }
    x@name = value
    return (x)
  }
)

#' @describeIn ssimLibrary Get the SSimLibrary associated with a SyncroSim Project.
setMethod('ssimLibrary', signature(name="Project"), function(name) {
  out = .ssimLibrary(name=.filepath(name),session=.session(name))
  return(out)
})



