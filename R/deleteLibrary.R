#' Delete Library
#'
#' Deletes a SyncroSim library. Note this is irreversable.
#'
#' @param ssimLibrary SsimLibrary or path to a library
#' @param force Logical. If FALSE (default) prompt to confirm that the library 
#'      should be deleted. This is irreversable.
#' @param removeBackup logical. If \code{TRUE}, will remove the backup folder when
#'     deleting a library. Default is FALSE.
#' @param removePublish logical. If TRUE, will remove the publish folder when
#'     deleting a library. Default is FALSE.
#' @param removeCustom logical. If TRUE and custom folders have been configured
#'     for a library, then will remove the custom publish and/or backup folders when 
#'     deleting a library. Note that the `removePublish` and `removeBackup` arguments 
#'     must also be set to TRUE to remove the respective custom folders. Default
#'     is FALSE.
#' @param session Session
#' @return "saved" or failure message.
#' 
#' #' @examples
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session and create SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#'  
#' # Delete library from SsimObject
#' deleteLibrary(myLibrary, force = TRUE, removeBackup = TRUE)
#' 
#' # Create another library
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' 
#' # Delete library from path
#' deleteLibrary(myLibraryName)
#' }
#' 
#' @export

setGeneric("deleteLibrary", 
           function(ssimLibrary, force = FALSE, removeBackup = FALSE,
                    removePublish = FALSE, removeCustom = FALSE,
                    session = NULL) standardGeneric("deleteLibrary"))

setMethod("deleteLibrary", signature(ssimLibrary = "SsimLibrary"), 
          function(ssimLibrary, force, removeBackup, removePublish, removeCustom) {
            
            return(deleteLibrary(.filepath(ssimLibrary), force, removeBackup, 
                                 removePublish, removeCustom, .session(ssimLibrary)))
          })

setMethod("deleteLibrary", signature(ssimLibrary = "character"), 
          function(ssimLibrary, force, removeBackup, removePublish, 
                   removeCustom, session) {
            
            if (!file.exists(ssimLibrary)) {
              stop(paste0("Library not found: ", ssimLibrary))
            }
            
            if (force) {
              answer <- "y"
            } else {
              answer <- readline(prompt = paste0("Do you really want to delete library ", ssimLibrary, "? (y/n): "))
            }
            
            if (answer == "y") {
              
              args <- list(delete = NULL, library = NULL, lib = ssimLibrary, force = NULL)
              
              if (removeBackup){
                args <- c(args, list(delrelback = NULL))
                if (removeCustom){
                  args <- c(args, list(delcustback = NULL))
                }
              }
              
              if (removePublish){
                args <- c(args, list(delrelpub = NULL))
                if (removeCustom){
                  args <- c(args, list(delcustpub = NULL))
                }
              }
              
              tt <- command(args = args, session = session)
              
              if (tt == "saved"){
                message(paste0("Library " ,ssimLibrary, " deleted"))
                return(invisible(TRUE))
              } else {
                message("Library deletion skipped")
                return(invisible(FALSE))
              }
            }
          })