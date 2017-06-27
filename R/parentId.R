# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' The parent scenario id of a SyncroSim Scenario.
#'
#' The id of the parent of a SyncroSim results scenario.
#' NA if scenario is not a results scenario.
#'
#' @param scenario A Scenario object.
#' @return An integer id of the parent scenario.
#' @export
setGeneric('parentId',function(scenario) standardGeneric('parentId'))
#' @rdname parentId
setMethod('parentId', signature(scenario="Scenario"), function(scenario) {
  if(scenario@parentId==0){return(NA)}
  return(scenario@parentId)
})