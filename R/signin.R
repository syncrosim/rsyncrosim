# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Signs in to SyncroSim
#'
#' Signs in to syncrosim.com to authenticate user credentials.
#'
#' @param session \code{\linkS4class{Session}} object. If \code{NULL}(default), the 
#' default session will be used
#' 
#' @return 
#' Character string: whether sign in was successful or not.
#' 
#' @examples
#' \dontrun{
#' # Sign in to SyncroSim session
#' mySession <- session()
#' signIn(mySession)
#' }
#' 
#' @export
signIn <- function(session = NULL) {
  
  # if a SyncroSim session is not provided, make one
  if (is.null(session)) {
    session <- .session()
  }
  
  if (is(session, "character") && (is(session, SyncroSimNotFound(warn = FALSE)))) {
    return(SyncroSimNotFound())
  }
  
  profileInfo <- .viewProfile(session, internal = TRUE)
  isSignedIn <- !grepl("You must sign in", profileInfo[1])
  
  if (isSignedIn){
    
    cat("You are already signed in to the following SyncroSim account:\n")
    cat(paste0(profileInfo[1], "\n"))
    cat(paste0(profileInfo[2], "\n"))
    cat(paste0(profileInfo[3], "\n"))
    cat(paste0(profileInfo[4], "\n"))
    cat("\nUse signOut() to sign out of the current SyncroSim account.")
    
    return(invisible(TRUE))
  }
  
  consoleName <- "SyncroSim.Console.exe"
  sessionPath <- filepath(session)
  consolePath <- file.path(sessionPath, consoleName)

  if (.Platform$OS.type == "windows") {
    p <- processx::process$new("cmd.exe", 
                     c("/k", paste0(consolePath, " --signin & pause")), 
                     stdin = "|", stdout = "|", stderr = "|",
                     cleanup = FALSE)
    
    counter <- 1
    counterMax <- 90
    success <- F
    
    while (p$is_alive() && counter < counterMax && success == FALSE){
      Sys.sleep(1)
      profileInfo <- .viewProfile(session, internal = TRUE)
      success <- grepl("Username", profileInfo[1])
      counter <- counter + 1
    }
    
    p$kill()
    
    if (success){
      
      cat("Successfully signed into SyncroSim account.\n")
      cat(paste0(profileInfo[1], "\n"))
      cat(paste0(profileInfo[2], "\n"))
      cat(paste0(profileInfo[3], "\n"))
      cat(paste0(profileInfo[4], "\n"))
      
      return(invisible(TRUE))
      
    } else if (counter == counterMax){
      
      cat("Sign in timed out.")
      
      return(invisible(FALSE))
      
    } else {
      
      cat("Sign in failed.")
      
      return(invisible(FALSE))
    }
    
  } else {
    # If linux, then will return a code and a link to the SyncroSim browser
    tempCmd <- paste(c("mono", consolePath, "--signin --force"), collapse = " ")
    suppressWarnings(system(tempCmd, wait = FALSE))
  }
}