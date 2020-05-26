# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' The path to a SyncroSim object on disk
#'
#' The path to a SyncroSim Session, SSimLibarary, Project or Scenario on disk.
#'
#' @param ssimObject An object containing a filepath.
#' 
#' @return 
#' A character string: the path to a SyncroSim object on disk.
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

#' The temporary file path to a SyncroSim object on disk
#'
#' The temporary file path to a SyncroSim Session, SSimLibarary, Project or Scenario on disk.
#'
#' @param ssimObject An object containing a filepath.
#' 
#' @return 
#' A character string: the temporary file path to a SyncroSim object on disk.
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
