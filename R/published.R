# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Publication status of a SyncroSim Folder
#'
#' Retrieves or sets the publication status of a SyncroSim \code{\link{Folder}}.
#' Note that only one folder can be tagged for publication at a time, so if 
#' the publication status of a Folder is set to \code{TRUE}, then all other
#' Folders will have publication status set to \code{FALSE}.
#'
#' @param folder \code{\link{Folder}} object
#' @param value logical. If \code{TRUE} the Folder will be tagged for 
#' publication and all other Folders will have publication status set to 
#' \code{FALSE}. Default is \code{FALSE}
#' 
#' @return 
#' A logical: \code{TRUE} if the Folder is tagged for publication and 
#' \code{FALSE} otherwise.
#' 
#' @examples 
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myFolder <- folder(myProject, folder = "New Folder")
#' 
#' # Retrieve publication status of the Folder
#' published(myFolder)
#' 
#' # Set the publication status of the Folder
#' published(myFolder) <- TRUE
#' }
#' 
#' @export
setGeneric("published", function(folder) standardGeneric("published"))

#' @rdname published
setMethod("published", signature(folder = "character"), function(folder) {
  return(SyncroSimNotFound(folder))
})

#' @rdname published
setMethod("published", signature(folder = "Folder"), function(folder) {
  info <- getFolderData(folder)
  publishedStatus <- info$Published
  if (publishedStatus == "No") {
    return(FALSE)
  } else {
    return(TRUE)
  }
  return(info$Published)
})

#' @rdname published
#' @export
setGeneric("published<-", function(folder, value) standardGeneric("published<-"))

#' @rdname published
setReplaceMethod(
  f = "published",
  signature = "character",
  definition = function(folder, value) {
    return(folder)
  }
)

#' @rdname published
setReplaceMethod(
  f = "published",
  signature = "Folder",
  definition = function(folder, value) {
    if (!is(value, "logical")) {
      stop("published value must be TRUE or FALSE.")
    }
    if (value == TRUE) {
      islite <- "yes"
    } else {
      islite <- "no"
    }
    
    tt <- command(list(setprop = NULL, lib = .filepath(folder), fid = .folderId(folder), islite = islite), .session(folder))
    if (!identical(tt, "saved")) {
      stop(tt)
    }
    return(folder)
  }
)
