# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' The run log of a result Scenario
#'
#' The run log of a result Scenario.
#'
#' @param scenario A Scenario object.
#' 
#' @return 
#' Returns a character string: the run log for a result scenario.
#' 
#' @export
setGeneric("runLog", function(scenario) standardGeneric("runLog"))

#' @rdname runLog
setMethod("runLog", signature(scenario = "character"), function(scenario) {
  return(SyncroSimNotFound(scenario))
})

#' @rdname runLog
setMethod("runLog", signature(scenario = "Scenario"), function(scenario) {
  tt <- command(list(list = NULL, runlog = NULL, lib = .filepath(scenario), sid = .scenarioId(scenario)), .session(scenario))
  if (grepl("The scenario is not a result scenario", tt[1], fixed = TRUE)) {
    tt <- tt[1]
    return(tt)
  }

  outString <- paste(tt, collapse = "\n")
  writeLines(outString)
  return(outString)
})
