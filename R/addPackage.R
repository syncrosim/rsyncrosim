# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Add SyncroSim package(s)
#'
#' Adds package(s) to a \code{\link{SsimLibrary}}.
#'
#' @param ssimLibrary \code{\link{SsimLibrary}} object
#' @param name character string or vector of package name(s)
#' 
#' @return
#' Invisibly returns \code{TRUE} upon success (i.e.successful addition 
#' of the package) or \code{FALSE} upon failure.
#' 
#' @seealso 
#' \code{\link{packages}}
#' 
#' @examples
#' \donttest{
#' # Install "stsim" SyncroSim package
#' installPackage("stsim")
#' 
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession, 
#'                          package = "stsim")
#' 
#' # Add package
#' addPackage(myLibrary, c("stsimsf"))
#' packages(myLibrary)
#' 
#' # Remove package
#' removePackage(myLibrary, c("stsimsf"))
#' packages(myLibrary)
#' }
#' 
#' @export
setGeneric("addPackage", function(ssimLibrary, name) standardGeneric("addPackage"))

#' @rdname addPackage
setMethod("addPackage", signature(ssimLibrary = "character"), function(ssimLibrary, name) {
  return(SyncroSimNotFound(ssimLibrary))
})

#' @rdname addPackage
setMethod("addPackage", signature(ssimLibrary = "SsimLibrary"), function(ssimLibrary, name) {
  cAdds <- packages(ssimLibrary)
  retList <- list()
  for (i in seq(length.out = length(name))) {
    cVal <- name[i]
    if (!is.element(cVal, cAdds$name)) {
      print(paste0("Warning - ", cVal, " is not among the available packages: ", paste(cAdds$name, collapse = ",")))
      retList[[cVal]] <- FALSE
      next
    }

    tt <- command(list(add = NULL, package = NULL, lib = .filepath(ssimLibrary), pkg = cVal), .session(ssimLibrary))
    if (tt == "saved"){
      message(paste0("Package <", cVal, "> added"))
      retList[[cVal]] <- TRUE
    } else {
      message(tt)
      retList[[cVal]] <- FALSE
    }
  }

  return(invisible(retList))
})
