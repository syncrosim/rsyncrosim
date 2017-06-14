# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' The path to a SyncroSim object on disk
#'
#' The path to a SyncroSim Session, SSimLibarary, Project or Scenario on disk.
#'
#' @param x An object containing a filepath.
#' @export
setGeneric('filepath',function(x) standardGeneric('filepath'))

# @describeIn filepath Path to the Syncrosim console in a Session.
setMethod('filepath', signature(x="Session"), function(x) x@filepath)

setMethod('filepath', signature(x="SsimObject"), function(x) x@filepath)
