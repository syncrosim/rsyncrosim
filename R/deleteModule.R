# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Delete module or modules
#'
#' Delete module or modules from this version of SyncroSim.
#' Note that removing a module can be difficult to undo.
#' To restore the module the user will need to provide a .ssimpkg file or reinstall SyncroSim.
#' Thus, deleteModule() requires confirmation from the user.
#'
#' @param name Character string or vector of these. A module or vector of modules to remove. See modules() for options.
#' @param session Session.
#' @param force logical. If T, delete without requiring confirmation from user.
#' @return "saved" or error message.
#' @export
setGeneric('deleteModule',function(name,session=NULL,force=F) standardGeneric('deleteModule'))
#' @rdname deleteModule
setMethod('deleteModule', signature(session="missingOrNULLOrChar"), function(name,session,force) {
  .Deprecated("deletePackage")
  stop()
})
#' @rdname deleteModule
setMethod('deleteModule', signature(session="Session"), function(name,session,force) {
  .Deprecated("deletePackage")
  stop()
})
