# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' The SyncroSim version
#'
#' The version of a SyncroSim Session
#'
#' @param session Session.
#' @export
setGeneric('version',function(session=NULL) standardGeneric('version'))
setMethod('version', signature(session="missingOrNULL"), function(session) {return(version(session()))})
setMethod('version', signature(session="Session"), function(session) {return(command(list(version=NULL),session))})
