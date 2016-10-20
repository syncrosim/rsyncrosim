setClassUnion("missingOrNULLOrChar", c("missing", "NULL","character"))
#' Path generic
#'
#' @param x An object containing a path.
setGeneric('path',function(x) standardGeneric('path'))

#' #' Info generic
#'
#' @param x An object containing info.
setGeneric('info',function(x) standardGeneric('info'))

#' Starts or gets a SyncroSim session.
#'
#' @param x=NULL A path to SyncroSim.Console.exe or an object containing a Session.
#'  If NULL the usual locations are searched.
#' @return An SyncroSim Session object containing a valid console path.
#' @examples
#' #Look for SyncroSim in the usual places
#' mySession = session()
#' path(mySession)
#'
#' #Specify a SyncroSim version
#' mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#'
#' #Get the session from a SyncroSim library object
#'
#' @export
setGeneric('session',function(x=NULL) standardGeneric('session'))
