# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

setMethod(
  f = "initialize", signature = "Folder",
  definition = function(.Object, ssimObject, folder, parentFolder = NULL, create = FALSE) {
    Name <- NULL
    FolderId <- NULL
    
    x <- ssimObject
    
    # Set some defaults
    ProjectId <- x@projectId
    ParentId <- 0
    
    # Depending on whether x is a ssimLibrary or Project, grab corresponding folder data
    folders <- getFolderData(x)
    allFolders <- folders
    
    # Get folder name and validate
    if (is.character(folder)){
      folders <- subset(folders, Name == folder)
      
      # If more than 1 folder retrieved, then name is not unique
      if ((nrow(folders) > 1) & (create == FALSE)) {
        stop("folder provided is not unique. Either set create=TRUE to create another folder with the same name or specify a unique name.")
      }
      
      Name <- folder

    } else if (is.numeric(folder)){
      
      folders <- subset(folders, FolderId == folder)
      
      # If no folders retrieved, then ID does not yet exist
      if (nrow(folders) == 0){
        stop(paste0("The library does not contain folder ID ", 
                    folder,
                    ". Please provide a name for the new folder - the ID will be assigned automatically by SyncroSim."))
      }
      
      if (create == TRUE) {
        stop("Cannot create a new folder from a folder ID. Please provide a name for the new folder and the ID will be assigned automatically.")
      }
      
      Name <- folders$Name
      
    } else{
      stop("folder argument must be a character or integer.")
    }

    # If one folder retrieved, then open folder
    if ((nrow(folders) == 1) & (create == FALSE)) {
      .Object@folderId <- folders$FolderId
      .Object@parentId <- getParentFolderId(x, folders$FolderId)
      .Object@session <- .session(x)
      .Object@filepath <- .filepath(x)
      .Object@projectId <- ProjectId
      return(.Object)
    }
    
    # If parentFolder provided, then create parameters for nested folder
    if (!is.null(parentFolder)) {
      
      # Grab parent folder ID and validate inputs
      if (is(parentFolder, "Folder")) {
        ParentId <- parentFolder@folderId 
        
      } else if (is.character(parentFolder)) {
        parentFolderData <- subset(allFolders, Name == parentFolder)
        
        if (nrow(parentFolderData) == 0) {
          stop(paste0("The library does not contain a folder with the name ", 
                      parentFolder,
                      ". Please provide a valid parent folder name."))
        }
        
        ParentId <- parentFolderData$FolderId
        
      } else if (is.numeric(parentFolder)) {
        parentFolderData <- subset(allFolders, FolderId == parentFolder)
        
        if (nrow(parentFolderData) == 0) {
          stop(paste0("The library does not contain a folder with the ID ", 
                      parentFolder,
                      ". Please provide a valid parent folder ID"))
        }
        
        ParentId <- parentFolderData$FolderId
        
      } else {
        stop("The parentFolder argument must be a character, integer, or SyncroSim folder object.")
      }
      
      # Create nested folder
      args <- list(lib = .filepath(x), create = NULL, folder = NULL, 
                   name = Name, tfid = ParentId)
      tt <- command(args = args, session = .session(x))
      FolderId <- as.integer(strsplit(tt, ": ")[[1]][2])
      
    } else {
      
      # If parentFolder not provided, then create a new folder at the root
      if (!is(x, "Project")){
        stop("Can only create a new folder at the project root if the ssimObject provided is a SyncroSim Project.")
      }
      
      args <- list(lib = .filepath(x), create = NULL, folder = NULL, 
                   name = Name, tpid = ProjectId)
      tt <- command(args = args, session = .session(x))
      FolderId <- as.integer(strsplit(tt, ": ")[[1]][2])
    }
    
    .Object@folderId <- FolderId
    .Object@parentId <- getParentFolderId(x, FolderId)
    .Object@session <- .session(x)
    .Object@filepath <- .filepath(x)
    .Object@projectId <- ProjectId
    return(.Object)
  }
)

#' Create or open a Folder
#'
#' Create or open a \code{\link{Folder}} from a SyncroSim
#' \code{\link{Project}}.
#'
#' @param ssimObject \code{\link{SsimLibrary}} or \code{\link{Project}} object.
#' @param folder character or integer. If character, then will either open an
#' existing folder if \code{create=FALSE}, or will create a new folder with the 
#' given name if the folder does not exist yet or \code{create=TRUE} (Default). 
#' If integer, will open the existing folder with the given folder ID (if the
#' ID exists).
#' @param parentFolder character, integer, or SyncroSim Folder object. If not 
#' \code{NULL} (Default), the new folder will be created inside of the
#' specified parent folder
#' @param create logical. Whether to create a new folder if the folder name given
#' already exists in the SyncroSim library. If \code{FALSE} (Default), then will 
#' return the existing folder with the given name. If \code{TRUE}, then will
#' return a new folder with the same name as an existing folder (but different
#' folder ID)
#' 
#' @return 
#' A \code{Folder} object representing a SyncroSim folder.
#' 
#' @examples
#' \donttest{
#' # Set the file path and name of the new SsimLibrary
#' myLibraryName <- file.path(tempdir(),"testlib")
#' 
#' # Set the SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession) 
#' myProject <- project(myLibrary, project = "My Project")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Create a new folder
#' myFolder <- folder(myProject, folder = "New Folder")
#' 
#' # Create a nested folder within "New Folder"
#' myNestedFolder <- folder(myProject, folder = "New Nested Folder", 
#'                          parentFolder = myFolder)
#'                          
#' # Retrieve a dataframe of all folders in a project
#' folder(myProject)
#' }
#' 
#' @name folder
#' @export
folder <- function(ssimObject = NULL, folder = NULL, parentFolder = NULL, create = FALSE) {
  if (is.character(ssimObject) && (ssimObject == SyncroSimNotFound(warn = FALSE))) {
    return(SyncroSimNotFound())
  }
  
  # if ssimObject is a scenario throw an error
  if (is.element(class(ssimObject), c("Scenario"))) {
    stop("Cannot create a folder at the Scenario-level.")
  }
  
  # Return folder data if no folder argument is specified
  if (is.null(folder)){
    folders <- getFolderData(ssimObject)
    
    if (is(ssimObject, "Project")){
      # Filter for folders in the project
      pid <- .projectId(ssimObject)
      df <- getLibraryStructure(ssimObject)
      projInd <- which((df$id == pid) & (df$item == "Project"))
      projRow <- df[projInd, ]
      childInd <- projInd + 1
      childRow <- df[childInd, ]
      childLevel <- as.numeric(childRow$level)
      ids <- c()
      
      while (childLevel > as.numeric(projRow$level)){
        if (childRow$item == "Folder"){
          ids <- c(ids, childRow$id)
        }
        childInd <- childInd + 1
        childRow <- df[childInd, ]
        
        if (is.na(childRow$id)){
          childLevel <- 0
        } else{
          childLevel <- as.numeric(childRow$level)
        }
      }
      
      folders <- folders[which(folders$FolderId %in% ids), ]
    }
    
    return(folders)
  }
  
  obj <- new("Folder", ssimObject, folder = folder, parentFolder = parentFolder, create = create)
  
  return(obj)
}
