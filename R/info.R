#' Information about an library
#' 
#' Get basic information about a Library.
#' 
#' @param ssimLibrary A \code{SsimLibrary} object.
#' 
#' @return 
#' Returns a \code{data.frame} with information on the properties of the library object.
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