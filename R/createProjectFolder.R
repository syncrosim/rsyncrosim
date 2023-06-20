# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Create folder at the Project level
#'
#' Create a new folder for a SyncroSim \code{\link{Project}}. 
#' The folder will be created at the SyncroSim Project root and can be
#' used to organize Scenarios in the SyncroSim User Interface.
#'
#' @param ssimObject \code{\link{Project}} object
#' @param folderName character string representing folder name
#' 
#' @return 
#' A Character String: the folder ID value
#' 
#' @examples
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session and SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' 
#' # Create a new folder at the Project level
#' createProjectFolder(myProject, "NewFolder")
#' }
#' 
#' @export
setGeneric("createProjectFolder", function(ssimObject, folderName) standardGeneric("createProjectFolder"))

#' @rdname createProjectFolder
setMethod("createProjectFolder", signature(ssimObject = "character"), function(ssimObject, folderName) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname createProjectFolder
setMethod("createProjectFolder", signature(ssimObject = "Project"), function(ssimObject, folderName) {
  args <- list(lib = .filepath(ssimObject), create = NULL, folder = NULL, 
               name = folderName, tpid = ssimObject@projectId)
  tt <- command(args = args, session = .session(ssimObject))
  # folderId <- as.integer(strsplit(tt, ": ")[[1]][2])
  return(tt)
})
