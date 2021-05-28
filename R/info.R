#' Retrieves information about an library
#' 
#' Retrieves some basic metadata about a Library.
#' 
#' @param ssimLibrary An object of type \code{\link{SsimLibrary}}.
#' 
#' @return 
#' Returns a \code{data.frame} with information on the properties of the library 
#' object.
#' 
#' @examples 
#' \donttest{
#' temp_dir <- tempdir()
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = mySession)
#' 
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