# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Apply updates.
#'
#' Apply updates to a SyncroSim Library,or a Project or Scenario associated with a Library.
#'
#' @param ssimObject  SsimLibrary/Project/Scenario
#' @return "saved" or a failure message from the console.
#' @export
setGeneric("ssimUpdate", function(ssimObject) standardGeneric("ssimUpdate"))

#' @rdname ssimUpdate
setMethod("ssimUpdate", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname ssimUpdate
setMethod("ssimUpdate", signature(ssimObject = "SsimObject"), function(ssimObject) {
  tt <- command(list(update = NULL, lib = .filepath(ssimObject)), .session(ssimObject))
  return(tt[1])
})
