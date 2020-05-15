# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' The parent scenario id of a SyncroSim Scenario.
#'
#' Retrives the id of the parent of a SyncroSim results scenario.
#'
#' @param scenario A Scenario object.
#' 
#' @return 
#' An integer id of the parent scenario. If the input scenario does not have a
#' parent, the function returns `NA`.
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
