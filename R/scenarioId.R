# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' The scenarioId of a scenario.
#'
#' The scenarioId of a Scenario.
#'
#' @param scenario Scenario.
#' @return integer id.
#' @export
setGeneric('scenarioId',function(scenario) standardGeneric('scenarioId'))
#' @describeIn scenarioId scenarioId of a Scenario.
setMethod('scenarioId', signature(scenario="Scenario"), function(scenario) {
  return(scenario@scenarioId)
})