# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Retrieves The parent scenario id of a SyncroSim Scenario.
#t
#' Retrieves the id of the parent of a SyncroSim results scenario.
#'
#' @param scenario A object of class \code{\link{Scenario}}.
#' 
#' @return 
#' An integer id of the parent scenario. If the input scenario does not have a
#' parent, the function returns `NA`.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' myProject <- project(myLibrary)
#' myScenario <- scenario(myProject)
#' 
#' parentId(myScenario)
#' }
#' 
#' @export
setGeneric("parentId", function(scenario) standardGeneric("parentId"))

#' @rdname parentId
setMethod("parentId", signature(scenario = "character"), function(scenario) {
  return(SyncroSimNotFound(scenario))
})

#' @rdname parentId
setMethod("parentId", signature(scenario = "Scenario"), function(scenario) {
  if (scenario@parentId == 0) {
    return(NA)
  }
  return(scenario@parentId)
})
