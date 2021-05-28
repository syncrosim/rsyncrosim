#' Retrieves information about an library
#' 
#' Retrieves some basic metadata about a Library: Name, Owner, Last Modified, 
#' Size, Read Only, Package Name, Package Description, Current Package Version,
#' Minimum Package Version, External input files, External output files, 
#' Temporary files, Backup files.
#' 
#' @param ssimLibrary An object of class \code{\link{SsimLibrary}}.
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
