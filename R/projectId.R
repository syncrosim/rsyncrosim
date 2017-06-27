# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' The projectId of a SyncroSim project or scenario.
#'
#' The projectId of a SyncroSim Project or Scenario.
#'
#' @param ssimObject Project/Scenario.
#' @return An integer project id.
#' @export
setGeneric('projectId',function(ssimObject) standardGeneric('projectId'))
#' @rdname projectId
setMethod('projectId', signature(ssimObject="Project"), function(ssimObject) {
  return(ssimObject@projectId)
})
#' @rdname projectId
setMethod('projectId', signature(ssimObject="Scenario"), function(ssimObject) {
  return(ssimObject@projectId)
})
