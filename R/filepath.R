# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves the path to a SyncroSim object on disk
#'
#' Retrieves the path to a SyncroSim \code{\link{Session}}, 
#' \code{\link{SsimLibrary}}, \code{\link{Project}} or \code{\link{Scenario}} 
#' on disk.
#'
#' @param ssimObject An object containing a filepath (of class \code{\link{Session}}, 
#'     \code{\link{Project}}, or \code{\link{SsimLibrary}}).
#' 
#' @return 
#' A character string: the path to a SyncroSim object on disk.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' 
#' # Get the file path
#' myFilePath <- filepath(myLibrary)
#' }
#' 
#' @export
setGeneric("filepath", function(ssimObject) standardGeneric("filepath"))

#' @rdname filepath
setMethod("filepath", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname filepath
setMethod("filepath", signature(ssimObject = "Session"), function(ssimObject) ssimObject@filepath)

#' @rdname filepath
setMethod("filepath", signature(ssimObject = "SsimObject"), function(ssimObject) ssimObject@filepath)

#' Retrieves the temporary file path to a SyncroSim object on disk
#'
#' Retrieves the temporary file path to a SyncroSim \code{\link{Session}}, 
#' \code{\link{SsimLibrary}}, \code{\link{Project}} or \code{\link{Scenario}} 
#' on disk.
#'
#' @param ssimObject An object containing a filepath (of class \code{\link{Session}}, 
#'     \code{\link{Project}}, or \code{\link{SsimLibrary}}).
#' 
#' @return 
#' A character string: the temporary file path to a SyncroSim object on disk.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' 
#' # Get the file path
#' myFilePath <- tempfilepath(myLibrary)
#' }
#' 
#' @export
setGeneric("tempfilepath", function(ssimObject) standardGeneric("tempfilepath"))

#' @rdname tempfilepath
setMethod("tempfilepath", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname tempfilepath
setMethod("tempfilepath", signature(ssimObject = "Session"), function(ssimObject) stop("This function is not valid for session objects."))

#' @rdname tempfilepath
setMethod("tempfilepath", signature(ssimObject = "SsimObject"), function(ssimObject) paste0(ssimObject@filepath, ".temp/RSyncroSim"))
