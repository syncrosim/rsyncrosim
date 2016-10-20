# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
NULL
#' SyncroSim Session class
#'
#' A SyncroSim Session object contains a link to SyncroSim.
#' \code{SSimLibrary}, \code{Project} and \code{Scenario} objects contain a \code{Session} used to query and modify the object.
#'
#' @examples
#' #create or load a library using a non-default Session
#' mySession = session("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
#' myLib = ssimLibrary(name="st-sim",model="st-sim",cSession=mySession)
#' session(myLib)
#'
#' #create or load a library using a default Session
#' myLib = ssimLibrary(name="st-sim",model="st-sim")
#' session(myLib)
#' @slot path The path to SyncroSim.Console.exe.
#' @name Session-class
#' @rdname Session-class
#' @export Session
Session <- setClass("Session", representation(path="character"))
# @name Session
# @rdname Session-class
setMethod(f="initialize",signature="Session",definition=function(.Object,x){
  #Check validity of console path.
  path=x
  if(!is.null(path)){
    if(!file.exists(path)){
      stop(paste("SyncroSim console could not be found at:",path))
    }
  }else{
    #try which
    whichPath = Sys.which("SyncroSim.Console.exe")
    if (file.exists(whichPath[1])){consolePath=whichPath[1]}
    if(is.null(path)){
      #TO DO: what is best way to find console on all systems
      #Default installation locations?
      consolePathPossibilities = c("C:/Program Files/SyncroSim/1/SyncroSim.Console.exe")
      for(i in seq(length(consolePathPossibilities))){
        if(file.exists(consolePathPossibilities[i])){path=consolePathPossibilities[i];break}
      }
    }
  }
  if(is.null(path)){
    stop('SyncroSim.Console.exe not found. Please set consolePath to the location of the SyncroSim console.')
  }
  .Object@path=path
  return(.Object)
})
#' @describeIn session Create a SyncroSim Session from a path or get default Session.
setMethod('session', signature(x="missingOrNULLOrChar"), function(x) {
  return(new("Session",x))
})

#' @describeIn path Path to the Syncrosim console in a Session.
setMethod('path', signature(x="Session"), function(x) x@path)

