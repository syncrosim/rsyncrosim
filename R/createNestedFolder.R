# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Create a nested folder within another folder
#'
#' Create a new nested folder for a SyncroSim \code{\link{Project}}. 
#' The folder will be created within another Project folder and can be
#' used to organize Scenarios in the SyncroSim User Interface.
#'
#' @param ssimObject \code{\link{Project}} object
#' @param parentFolderId integer
#' @param folderName character string representing folder name
#' 
#' @return 
#' A Character String: the nested folder ID value
#' 
#' @examples
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session and SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(ssimLibrary)
#' 
#' # Create a new folder at the Project level and extract the folder Id
#' parentFolderIdString <- createProjectFolder(myProject, "NewFolder")
#' parentFolderId <- as.integer(strsplit(parentFolderIdString, ": ")[[1]][2])
#' 
#' # Create nested folder within project folder
#' createNestedFolder(myProject, parentFolderId, "NewNestedFolder")
#' }
#' 
#' @export
setGeneric("createNestedFolder", function(ssimObject, parentFolderId, folderName) standardGeneric("createNestedFolder"))

#' @rdname createNestedFolder
setMethod("createNestedFolder", signature(ssimObject = "character"), function(ssimObject, parentFolderId, folderName) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname createNestedFolder
setMethod("createNestedFolder", signature(ssimObject = "Project"), function(ssimObject, parentFolderId, folderName) {
  args <- list(lib = .filepath(ssimObject), create = NULL, folder = NULL, 
               name = folderName, tfid = parentFolderId)
  browser()
  tt <- command(args = args, session = .session(ssimObject))
  # folderId <- as.integer(strsplit(tt, ": ")[[1]][2])
  return(tt)
})
