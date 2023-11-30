# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Removes SyncroSim package(s)
#'
#' Removes package(s) from a \code{\link{SsimLibrary}}.
#'
#' @param ssimLibrary \code{\link{SsimLibrary}} object
#' @param name character string or vector of package name(s)
#' 
#' @return
#' This function invisibly returns \code{TRUE} upon success (i.e.successful 
#' removal of the package) or \code{FALSE} upon failure.
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
setGeneric("removePackage", function(ssimLibrary, name) standardGeneric("removePackage"))

#' @rdname removePackage
setMethod("removePackage", signature(ssimLibrary = "character"), function(ssimLibrary, name) {
  return(SyncroSimNotFound(ssimLibrary))
})

#' @rdname removePackage
setMethod("removePackage", signature(ssimLibrary = "SsimLibrary"), function(ssimLibrary, name) {
  cAdds <- subset(addon(ssimLibrary))
  retList <- list()

  for (i in seq(length.out = length(name))) {
    cVal <- name[i]
    if (!is.element(cVal, cAdds$name)) {
      print(paste0("Warning - ", cVal, " is not among the available packages: ", paste(cAdds$name, collapse = ",")))
      retList[[cVal]] <- FALSE
      next
    }

    tt <- command(list(remove = NULL, package = NULL, force = NULL, lib = .filepath(ssimLibrary), pkg = cVal), .session(ssimLibrary))
    if (tt == "saved"){
      message(paste0("Package <", cVal, "> removed"))
      retList[[cVal]] <- TRUE
    } else {
      message(tt)
      retList[[cVal]] <- FALSE
    }
  }

  return(invisible(retList))
})
