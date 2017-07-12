# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' The path to a SyncroSim object on disk
#'
#' The path to a SyncroSim Session, SSimLibarary, Project or Scenario on disk.
#'
#' @param ssimObject An object containing a filepath.
#' @export
setGeneric('filepath',function(ssimObject) standardGeneric('filepath'))
#' @rdname filepath
setMethod('filepath', signature(ssimObject="Session"), function(ssimObject) ssimObject@filepath)
#' @rdname filepath
setMethod('filepath', signature(ssimObject="SsimObject"), function(ssimObject) ssimObject@filepath)
