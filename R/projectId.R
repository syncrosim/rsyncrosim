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
#' @describeIn projectId The id of a Project.
setMethod('projectId', signature(ssimObject="Project"), function(ssimObject) {
  return(ssimObject@projectId)
})
#' @describeIn projectId The projectId of a Scenario.
setMethod('projectId', signature(ssimObject="Scenario"), function(ssimObject) {
  return(ssimObject@projectId)
})
