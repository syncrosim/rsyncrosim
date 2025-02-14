# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Signs in to SyncroSim
#'
#' Signs in to syncrosim.com to authenticate user credentials.
#'
#' @param session \code{\link{Session}} object. If \code{NULL}(default), the 
#' default session will be used
#' 
#' @return 
#' Character string: whether sign in was successful or not.
#' 
#' @examples
#' \dontrun{
#' # Sign in to SyncroSim session
#' mySession <- session()
#' signin(mySession)
#' }
#' 
#' @export
signin <- function(session = NULL) {
  
  # if a SyncroSim session is not provided, make one
  if (is.null(session)) {
    session <- .session()
  }
  
  if (is(session, "character") && (is(session, SyncroSimNotFound(warn = FALSE)))) {
    return(SyncroSimNotFound())
  }
  
  consoleName <- "SyncroSim.Console.exe"
  sessionPath <- filepath(session)
  consolePath <- file.path(sessionPath, consoleName)
  
  profileInfo <- capture.output(.viewProfile(session))
  isSignedIn <- !grepl("You must sign in", profileInfo[1])
  
  if (isSignedIn){
    
    cat("You are already signed in to the following SyncroSim account:\n")
    cat(paste0(profileInfo[1], "\n"))
    cat(paste0(profileInfo[2], "\n"))
    cat(paste0(profileInfo[3], "\n"))
    cat(paste0(profileInfo[4], "\n"))
    cat("\nUse signout() to sign out of the current SyncroSim account.")
    
    return(invisible(TRUE))
  }
  
  p <- processx::process$new("cmd.exe", 
                   c("/k", paste0(consolePath, " --signin & pause")), 
                   stdin = "|", stdout = "|", stderr = "|",
                   cleanup = FALSE)
  Sys.sleep(1)
  profileInfo <- capture.output(.viewProfile(session))
  success <- grepl("Username", profileInfo[1])
  
  if (success){
    
    cat("Successfully signed into SyncroSim account.\n")
    cat(paste0(profileInfo[1], "\n"))
    cat(paste0(profileInfo[2], "\n"))
    cat(paste0(profileInfo[3], "\n"))
    cat(paste0(profileInfo[4], "\n"))
    
    return(invisible(TRUE))
    
  } else {
    
    cat("Sign in failed.")
    
    return(invisible(FALSE))
  }
}