# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

setMethod(f = "initialize", signature = "Project", 
          definition = function(.Object, ssimLibrary, name = NULL, id = NULL, 
                                projects = NULL, sourceProject = NULL) {

  ProjectId <- NULL
  Name <- NULL
  
  # This constructor is only called from projects and getFromXProjScn - assume that ssimLibrary really is an object, projects is defined, and the project is not redundant.
  x <- ssimLibrary

  # For fast processing - quickly return without system calls if projects exists and can be easily identified
  if (is.null(projects)) {
    projects <- getProjectSet(x)
  }
  findPrj <- projects

  if (!is.null(id)) {
    findPrj <- subset(findPrj, ProjectId == id)
  }
  if (!is.null(name)) {
    pre <- findPrj
    cName <- name
    findPrj <- subset(findPrj, Name == cName)
    if (!is.null(id) && (nrow(pre) > 0) && (nrow(findPrj) == 0)) {
      stop(paste0("The library already contains a project id ", id, " with a different name ", pre$Name))
    }
  }
  if (is.null(id) & is.null(name) & (nrow(findPrj) == 1)) {
    name <- findPrj$Name
  }
  if (is.null(id) & is.null(name) & (nrow(findPrj) > 0)) {
    name <- "Project"
    cName <- name
    findPrj <- subset(findPrj, Name == cName)
  }

  if (nrow(findPrj) == 1) {
    if (!is.null(sourceProject)) {
      warning("Project ", name, " (", findPrj$ProjectId, ") already exists, so sourceProject argument was ignored.")
    }
    # Go ahead and create the Projects object without issuing system commands to make sure it is ok
    .Object@session <- .session(x)
    .Object@filepath <- .filepath(x)
    .Object@datasheetNames <- .datasheets(x, scope = "all", refresh = TRUE)
    .Object@projectId <- as.numeric(findPrj$ProjectId)
    return(.Object)
  }

  # Now go ahead to handle odder cases
  if (nrow(findPrj) > 0) {
    stop(paste0("The library contains more than one project called ", name, ". Specify a project id: ", paste(findPrj$ProjectId, collapse = ",")))
  }

  # If given an id for a project that does not yet exist, complain
  if (!is.null(id)) {
    stop(paste0("The library does not contain project id ", id, ". Please provide a name for the new project - the id will be assigned automatically by SyncroSim."))
  }

  # Create a new project
  if (is.null(name)) {
    name <- "Project"
  }
  if (!is.null(sourceProject)) {
    # complain if source project does not exist.
    sourcePID <- NA
    slib <- .filepath(x)
    if (is(sourceProject, "numeric")) {
      if (!is.element(sourceProject, projects$ProjectId)) {
        stop(paste0("sourceProject id ", sourceProject, " not found in the library."))
      }
      sourcePID <- sourceProject
    }
    if (is(sourceProject, "character")) {
      if (!is.element(sourceProject, projects$Name)) {
        stop(paste0("sourceProject name ", sourceProject, " not found in the library."))
      }
      sourcePID <- projects$ProjectId[projects$Name == sourceProject]
    }
    if (is(sourceProject, "Project")) {
      slib <- .filepath(sourceProject)
      sourcePID <- .projectId(sourceProject)
    }

    if (is.na(sourcePID)) {
      stop("Source project must be a number, project name, or Project object.")
    }

    if (name == "GetSourceCopyCopyCopy") {
      sourceProjectName <- subset(projects, ProjectId == sourcePID)$Name

      copyName <- paste(sourceProjectName, "- Copy")
      if (!is.element(copyName, projects$Name)) {
        name <- copyName
      } else {
        done <- FALSE
        count <- 0
        while (!done) {
          count <- count + 1
          cName <- paste0(copyName, count)
          if (!is.element(cName, projects$Name)) {
            name <- cName
            done <- TRUE
          }
        }
      }
    }
    tt <- command(list(copy = NULL, project = NULL, slib = slib, 
                       tlib = .filepath(x), pid = sourcePID, name = name), 
                  .session(x))
  } else {
    tt <- command(list(create = NULL, project = NULL, lib = .filepath(x), 
                       name = name), .session(x))
  }

  if (!grepl("Project Id is:", tt[1], fixed = TRUE)) {
    stop(tt)
  }

  id <- as.numeric(strsplit(tt, ": ")[[1]][2])

  .Object@session <- .session(x)
  .Object@filepath <- .filepath(x)
  .Object@datasheetNames <- .datasheets(x, scope = "all", refresh = TRUE)
  .Object@projectId <- as.numeric(id)
  return(.Object)
})

#' Create or open Project(s)
#'
#' Creates or retrieves a \code{\link{Project}} or multiple Projects from a
#' SsimLibrary.
#'
#' @param ssimObject \code{\link{SsimLibrary}} or \code{\link{Scenario}} object, 
#'     or a character string (i.e. a filepath)
#' @param project \code{\link{Project}} object, character, integer, or vector 
#' of these. Names or ids of one or more Projects. Note that integer ids are 
#' slightly faster (optional)
#' @param sourceProject \code{\link{Project}} object, character, or integer. If 
#'     not \code{NULL} (default), new Projects will be copies of the sourceProject
#' @param summary logical. If \code{TRUE} then return the Project(s) in a data.frame with 
#'     the projectId, name, description, owner, dateModified, readOnly. Default 
#'     is \code{TRUE} if \code{project=NULL} and SsimObject is not Scenario/Project, \code{FALSE} 
#'     otherwise
#' @param forceElements logical. If \code{TRUE} then returns a single Project as a named 
#'     list; otherwise returns a single project as a \code{\link{Project}} object. 
#'     Applies only when \code{summary=FALSE} Default is \code{FALSE}
#' @param overwrite logical. If \code{TRUE} an existing Project will be overwritten. 
#' Default is \code{FALSE}
#' 
#' @details
#' For each element of project:
#' \itemize{
#'   \item If element identifies an existing Project: Returns the existing Project.
#'   \item If element identifies more than one Project: Error.
#'   \item If element does not identify an existing Project: Creates a new Project 
#'   named element. Note that SyncroSim automatically assigns an id to a new Project.
#' }
#' 
#' @return 
#' Returns a \code{\link{Project}} object representing a SyncroSim Project. If 
#' summary is \code{TRUE}, returns a data.frame of Project names and descriptions.
#' 
#' @examples
#' \donttest{
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib_project")
#' 
#' # Set the SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession) 
#' myProject <- project(ssimObject = myLibrary, project = "My project name")
#' myproject2 <- project(ssimObject = myLibrary, project = "My new project name")
#'
#' # Get a named list of existing Projects
#' # Each element in the list is named by a character version of the Project ID
#' myProjects <- project(myLibrary, summary = FALSE)
#' names(myProjects)
#'
#' # Get an existing Project.
#' myProject <- myProjects[[1]]
#' myProject <- project(myLibrary, project = "My new project name")
#'
#' # Get/set the Project properties
#' name(myProject)
#' name(myProject) <- "New project name"
#' 
#' # Create a new Project from a copy of an existing Project
#' myNewProject <- project(myLibrary, project = "My copied project",
#'                         sourceProject = 1)
#' 
#' # Overwrite an existing Project
#' myNewProject <- project(myLibrary, project = "My copied project",
#'                         overwrite = TRUE)
#' }
#' 
#' @name project
#' @export
project <- function(ssimObject = NULL, project = NULL, sourceProject = NULL, 
                    summary = NULL, forceElements = FALSE, overwrite = FALSE) {
  
  if ((is(ssimObject, "character")) && (is(ssimObject, SyncroSimNotFound(warn = FALSE)))) {
    return(SyncroSimNotFound())
  }
  
  if (is.null(ssimObject)) {
    e <- ssimEnvironment()
    ssimObject <- ssimLibrary(e$LibraryFilePath)
    project <- as.integer(e$ProjectId)
  }

  # if ssimObject is a scenario or project, return the project
  if (is.element(class(ssimObject), c("Scenario", "Project")) & is.null(project)) {
    if (is.null(summary)) {
      summary <- FALSE
    }
    if (!summary) {
      convertObject <- TRUE
      returnIds <- FALSE
    } else {
      convertObject <- FALSE
      returnIds <- TRUE
    }
  } else {
    # set summary default
    if (is.null(summary)) {
      if (is.null(project)) {
        if (is.null(sourceProject)) {
          summary <- TRUE
        } else {
          summary <- FALSE
          project <- "GetSourceCopyCopyCopy"
        }
      } else {
        summary <- FALSE
      }
    }
    convertObject <- TRUE
    returnIds <- TRUE
  }

  xProjScn <- .getFromXProjScn(ssimObject, project = project, scenario = NULL, 
                               convertObject = convertObject, 
                               returnIds = returnIds, goal = "project", 
                               complainIfMissing = FALSE)

  if (is(xProjScn, "Project")) {
    if (!overwrite) {
      return(xProjScn)
    }
  }

  if (!is(xProjScn, "list")) {
    stop("something is wrong")
  }
  
  ssimObject <- xProjScn$ssimObject
  project <- xProjScn$project
  allProjects <- xProjScn$projectSet
  
  if (is.element("order", names(allProjects))) {
    projectSet <- subset(allProjects, !is.na(order))
  } else {
    if (nrow(allProjects) > 0) {
      allProjects$order <- seq(1, nrow(allProjects))
    }
    projectSet <- allProjects
  }
  
  if (nrow(projectSet) == 0) {
    if (summary) {
      projectSet$exists <- NULL
      projectSet$order <- NULL
      return(projectSet)
    } else {
      stop("Error in project(): No projects to get or make.")
    }
  }
  
  # if all projects exist and summary, simply return summary
  if ((sum(is.na(projectSet$exists)) == 0) & summary) {
    projectSet <- subset(projectSet, !is.na(order))
    projectSet <- projectSet[order(projectSet$order), ]
    projectSet[projectSet$IsReadOnly == "FALSE", "IsReadOnly"] <- "No"
    projectSet[projectSet$IsReadOnly == "TRUE", "IsReadOnly"] <- "Yes"
    projectSet$exists <- NULL
    projectSet$order <- NULL
    return(projectSet)
  }

  # Now assume project is defined
  # distinguish existing projects from those that need to be made
  areIds <- is.numeric(project)

  # make projects/project objects
  projectsToMake <- projectSet
  if (summary) {
    projectsToMake <- subset(projectsToMake, is.na(exists))
  }
  projectsToMake <- projectsToMake[order(projectsToMake$order), ]
  projectList <- list()

  for (i in seq(length.out = nrow(projectsToMake))) {
    cRow <- projectsToMake[i, ]
    projExists <- !is.na(cRow$exists)

    if (projExists) {
      if (overwrite) {
        command(list(delete = NULL, project = NULL, lib = .filepath(ssimObject), 
                     pid = cRow$ProjectId, force = NULL), .session(ssimObject))
        allProjects[i, "exists"] <- NA
        projectsToMake[i, "exists"] <- NA
      }
    }
  }

  for (i in seq(length.out = nrow(projectsToMake))) {
    cRow <- projectsToMake[i, ]
    projExists <- !is.na(cRow$exists)

    if (projExists) {
      projectList[[as.character(projectsToMake$ProjectId[i])]] <- new("Project", ssimObject, id = cRow$ProjectId, projects = subset(allProjects, !is.na(exists)), sourceProject = sourceProject)
    } else {
      obj <- new("Project", ssimObject, name = cRow$Name, projects = subset(allProjects, !is.na(exists)), sourceProject = sourceProject)
      projectList[[as.character(.projectId(obj))]] <- obj
    }
  }

  if (!summary) {
    if ((length(projectList) == 1) & !forceElements) {
      projectList <- projectList[[1]]
    }
    return(projectList)
  }
  
  projectSetOut <- getProjectSet(ssimObject)
  projectSetOut$exists <- NULL
  idList <- data.frame(id = as.numeric(names(projectList)), order = seq(1:length(projectList)))
  projectSetOut <- merge(idList, projectSetOut, all.x = TRUE)
  
  if (sum(is.na(projectSetOut$name)) > 0) {
    stop("Something is wrong with project()")
  }
  
  projectSetOut <- projectSetOut[order(projectSetOut$order), ]
  projectSetOut$order <- NULL
  
  return(projectSetOut)
}
