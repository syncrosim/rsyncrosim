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
#' @slot name The project name
#' @slot id The project id
#' @name Project-class
#' @rdname Project-class
#' @export Project
Project <- setClass("Project", contains="SSimLibrary",representation(name="character",id="numeric"))
# @name Project
# @rdname Project-class
setMethod(f="initialize",signature="Project",
    definition=function(.Object,ssimLibrary,name=NULL,id=NULL){
    #ssimLibrary = myLibrary#ssimLibrary(model="stsim", name= "C:/Temp/NewLibrary.ssim",session=devSsim)
    # id = NULL;name=NULL
    cProjects = projects(ssimLibrary,names=T)
    if(is.null(name)&is.null(id)){
      #stop("Project must be identified by a name or id.")
      #if(nrow(cProjects)==0){
      #  name = "Project1"
      #}else{
      #  name= paste0("Project",max(as.numeric(cProjects$id))+1)
      #}
      name="Project"
    }

    #If project already exists, return the details.
    #Complain if id and name don't match, or if there is more than one project with a name and id is not specified.
    if(nrow(cProjects)>0){#&&((is.null(id)||is.element(id,cProjects$id))||(is.null(name)||is.element(name,cProjects$name)))){
      if(!is.null(id)&&is.element(id,cProjects$id)){
        cName = cProjects$name[cProjects$id==id]
        if(!is.null(name)&&!identical(cName,name)){
          stop(paste0("The library already contains a project id ",id," with a different name ",cName))
        }
        name=cName
      }
      if(is.null(id)&&!is.null(name)&&is.element(name,cProjects$name)){
        numProjects = nrow(subset(cProjects,name==name))
        if(is.null(id)&(numProjects>1)){
          stop(paste0("The library contains more than one project called ",name,". Please provide an id:",paste(cProjects$id[cProjects$name==name],collapse=",")))
        }
        id = cProjects$id[cProjects$name==name]
      }
    }

    #If given an id for a project that does not yet exist, complain
    if(!is.null(id)&&!is.element(id,cProjects$id)){
      stop(paste0("The library does not contain project id ",id,". Please provide a name for the new project - the id will be assigned automatically by SyncroSim."))
    }

    #If project does not yet exist, make it.
    if(is.null(id)){
      tt = command(list(create=NULL,project=NULL,lib=.filepath(ssimLibrary),name=name),.session(ssimLibrary))
      id =strsplit(tt,": ")[[1]][2]
    }
    .Object@session=.session(ssimLibrary)
    .Object@filepath=.filepath(ssimLibrary)
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
project <- function(ssimLibrary,name=NULL,id=NULL) new("Project",ssimLibrary,name,id)
.project = project

setMethod('name', signature(x="Project"), function(x) {
  return(x@name)
})

setMethod('id', signature(x="Project"), function(x) {
  return(x@id)
})


#' Set the project name
#'
#' Set the name of a SyncroSim project.
#'
#' @param x A SyncroSim \code{\link{Project}} object.
#' @param value The new project name.
#' @export
setGeneric('name<-',function(x,value) standardGeneric('name<-'))
setReplaceMethod(
  f="name",
  signature="Project",
  definition=function(x,value){
    #x=myProject
    #TO DO: console command for renaming a project.
    return (x)
  }
)

#' @describeIn ssimLibrary Get the SSimLibrary associated with a SyncroSim Project.
setMethod('ssimLibrary', signature(model="Project"), function(model) {
  out = .ssimLibrary(name=.filepath(model),session=.session(model))
  return(out)
})



