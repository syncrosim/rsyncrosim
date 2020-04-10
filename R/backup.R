# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Backup an SsimLibrary.
#'
#' Backup an SsimLibrary.
#'
#' @param ssimObject SsimLibrary/Project/Scenario.
#' @export
setGeneric("backup", function(ssimObject) standardGeneric("backup"))

#' @rdname backup
setMethod("backup", signature(ssimObject = "character"), function(ssimObject) {
  return(SyncroSimNotFound(ssimObject))
})

#' @rdname backup
setMethod("backup", signature(ssimObject = "SsimObject"), function(ssimObject) {
  ds <- datasheet(ssimObject, name = "core_Backup")
  args <- list(lib = .filepath(ssimObject), backup = NULL)

  if (!is.na(ds$IncludeInput)) {
    if (ds$IncludeInput) {
      args <- c(args, list(input = NULL))
    }
  }

  if (!is.na(ds$IncludeOutput)) {
    if (ds$IncludeOutput) {
      args <- c(args, list(output = NULL))
    }
  }

  tt <- command(args = args, session = .session(ssimObject))
  return(tt)
})
