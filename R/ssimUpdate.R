# Copyright (c) 2021 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Apply updates
#'
#' Apply updates to a \code{\link{SsimLibrary}}, or a \code{\link{Project}} or 
#' \code{\link{Scenario}} associated with a Library.
#'
#' @param ssimObject An object of class \code{\link{Session}}, \code{\link{Project}}, 
#' or \code{\link{SsimLibrary}}.
#' 
#' @return 
#' This function invisibly returns `TRUE` upon success (i.e.successful 
#' update) and `FALSE` upon failure.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' 
#' # Update Library
#' ssimUpdate(myLibrary)
#' 
#' myProject <- project(myLibrary, name = "My Project")
#' 
#' # Update Project
#' ssimUpdate(myProject)
#' 
#' myScenario <- scenario(myLibrary, name = "MyScenario")
#' 
#' # Update scenario
#' ssimUpdate(myScenario)
#' }
#' 
#' @export
setGeneric("ssimUpdate", function(ssimObject) standardGeneric("ssimUpdate"))

#' @rdname ssimUpdate
setMethod("ssimUpdate", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname ssimUpdate
setMethod("ssimUpdate", signature(ssimObject = "SsimObject"), function(ssimObject) {
  success <- FALSE
  tt <- command(list(update = NULL, lib = .filepath(ssimObject)), .session(ssimObject))
  if (!is.na(tt[1])){ 
    if (tt == "saved"){
      message("Library successfully updated")
      success <- TRUE
    } else{
      message(tt)
    }
  }
  return(invisible(success))
})
  