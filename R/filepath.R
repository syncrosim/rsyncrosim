# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' The path to a SyncroSim object on disk
#'
#' The path to a SyncroSim Session, SSimLibarary, Project or Scenario on disk.
#'
#' @param ssimObject An object containing a filepath.
#' @export
setGeneric('filepath',function(ssimObject) standardGeneric('filepath'))
#' @describeIn filepath Path to the Syncrosim console in a Session.
setMethod('filepath', signature(ssimObject="Session"), function(ssimObject) ssimObject@filepath)
#' @describeIn filepath Path to the library of an SsimObject.
setMethod('filepath', signature(ssimObject="SsimObject"), function(ssimObject) ssimObject@filepath)
