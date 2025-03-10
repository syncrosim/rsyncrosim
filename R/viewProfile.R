# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Display SyncroSim profile
#'
#' Displays the currently signed in SyncroSim profile information. To sign in
#' to SyncroSim use the \code{\link{signin}} function.
#'
#' @param session \code{\link{Session}} object. If \code{NULL}(default), the 
#' default session will be used
#' 
#' @examples
#' \dontrun{
#' # Retrieve profile information for a SyncroSim session
#' mySession <- session()
#' viewProfile(mySession)
#' }
#' 
#' @export
viewProfile <- function(session = NULL) {
  
  # if a SyncroSim session is not provided, make one
  if (is.null(session)) {
    session <- .session()
  }
  if (is(session, "character") && (is(session, SyncroSimNotFound(warn = FALSE)))) {
    return(SyncroSimNotFound())
  }
  
  consoleName <- "SyncroSim.Console.exe"
  sessionPath <- filepath(session)
  
  p <- processx::process$new(file.path(sessionPath, consoleName),
                   args = c("--profile"),
                   stdin = "|", stdout = "|", stderr = "|")
  Sys.sleep(1)
  out <- p$read_output()
  cat(out)
}