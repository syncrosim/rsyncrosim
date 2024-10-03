#' Retrieves information about a library
#' 
#' Retrieves some basic metadata about a SsimLibrary: Name, Owner, Last Modified, 
#' Size, Read Only, Data files, Publish files, Temporary files, Backup files, 
#' and Use conda.
#' 
#' @param ssimLibrary \code{\link{SsimLibrary}} object
#' 
#' @return 
#' Returns a \code{data.frame} with information on the properties of the SsimLibrary 
#' object.
#' 
#' @examples 
#' \dontrun{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session and SsimLibrary
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' 
#' # Get information about SsimLibrary  
#' info(myLibrary)
#' }
#' 
#' @export
setGeneric("info", function(ssimLibrary) standardGeneric("info"))

#' @rdname info
setMethod("info", signature(ssimLibrary = "SsimLibrary"), function(ssimLibrary) {
  
  args <- list(list = NULL, library = NULL, csv = NULL, lib = .filepath(ssimLibrary))
  tt <- command(args, .session(ssimLibrary))
  out <- .dataframeFromSSim(tt, localNames = TRUE)
  
  return(out)
})
