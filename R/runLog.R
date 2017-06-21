# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' The runLog of a result Scenario.
#'
#' The runLog of a result Scenario
#'
#' @param scenario A Scenario object.
#' @return Character string of the run log.
#' @export
setGeneric('runLog',function(scenario) standardGeneric('runLog'))
setMethod('runLog', signature(scenario="Scenario"), function(scenario) {
  command("--list --runlog --help")
  tt=command(list(list=NULL,runlog=NULL,lib=.filepath(scenario),sid=.scenarioId(scenario)),.session(scenario))
  if(grepl("The scenario is not a result scenario",tt[1],fixed=T)){tt=tt[1];return(tt)}

  outString=paste(tt,collapse="\n") 
  writeLines(outString)
  return(outString)
})