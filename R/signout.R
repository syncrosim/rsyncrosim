# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Signs out of SyncroSim
#'
#' Signs out of syncrosim.com.
#'
#' @param session \code{\link{Session}} object. If \code{NULL}(default), the 
#' default session will be used
#' 
#' @return 
#' Character string: whether sign out was successful or not.
#' 
#' @examples
#' \dontrun{
#' # Sign out of SyncroSim session
#' mySession <- session()
#' signOut(mySession)
#' }
#' 
#' @export
signOut <- function(session = NULL) {
  
  # if a SyncroSim session is not provided, make one
  if (is.null(session)) {
    session <- .session()
  }
  
  if (is(session, "character") && (is(session, SyncroSimNotFound(warn = FALSE)))) {
    return(SyncroSimNotFound())
  }
  
  profileInfo <- .viewProfile(session, internal = TRUE)
  isSignedIn <- !grepl("You must sign in", profileInfo[1])
  
  if (!isSignedIn){
    
    cat("You are not currently signed in.")
    
    return(invisible(TRUE))
  }
  
  consoleName <- "SyncroSim.Console.exe"
  sessionPath <- filepath(session)
  consolePath <- file.path(sessionPath, consoleName)
  
  if (.Platform$OS.type == "windows") {
    p <- processx::process$new("cmd.exe", 
                               c("/k", paste0(consolePath, " --signout & pause")), 
                               stdin = "|", stdout = "|", stderr = "|",
                               cleanup = FALSE)
    
    counter <- 1
    counterMax <- 30
    success <- F
    
    while (p$is_alive() && counter < counterMax && success == FALSE){
      Sys.sleep(1)
      profileInfo <- .viewProfile(session, internal = TRUE)
      success <- grepl("You must sign in", profileInfo[1])
      counter <- counter + 1
    }
    
    p$kill()
    
    if (success){
      
      cat("Successfully signed out of SyncroSim account.\n")
      
      return(invisible(TRUE))
      
    } else if (counter == counterMax){
      
      cat("Sign out timed out.")
      
      return(invisible(FALSE))
      
    } else {
      
      cat("Sign out failed.")
      
      return(invisible(FALSE))
    }
  } else {
    # If linux, then will return a link to the SyncroSim browser
    tempCmd <- paste(c("mono", consolePath, "--signout --force"), collapse = " ")
    suppressWarnings(system(tempCmd, wait = FALSE))
  }
}