# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Display SyncroSim profile
#'
#' Displays the currently signed in SyncroSim profile information. To sign in
#' to SyncroSim use the \code{\link{signIn}} function.
#'
#' @param session \code{\link{Session-class}} object. If \code{NULL}(default), the 
#' default session will be used
#' @param ... other internal parameters
#' 
#' @examples
#' \dontrun{
#' # Retrieve profile information for a SyncroSim session
#' mySession <- session()
#' viewProfile(mySession)
#' }
#' 
#' @export
viewProfile <- function(session = NULL, ...) {
  
  get <- function(internal = FALSE) internal

  # if a SyncroSim session is not provided, make one
  if (is.null(session)) {
    session <- .session()
  }
  if (is(session, "character") && (is(session, SyncroSimNotFound(warn = FALSE)))) {
    return(SyncroSimNotFound())
  }
  
  p <- command("--profile", session = session, program = "SyncroSim.Console.exe")

  if (grepl("Username", p[1]) & !get(...)){
    
    cat(paste0(p[1], "\n"))
    cat(paste0(p[2], "\n"))
    cat(paste0(p[3], "\n"))
    cat(paste0(p[4], "\n"))
    
    return(invisible(p))
  }
  
  return(p)
}