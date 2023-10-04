# Copyright (c) 2023 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Enable addon package(s)
#'
#' Enable \code{\link{addon}} package(s) of a \code{\link{SsimLibrary}}.
#'
#' @param ssimLibrary \code{\link{SsimLibrary}} object
#' @param name character string or vector of addon name(s)
#' 
#' @return
#' Invisibly returns \code{TRUE} upon success (i.e.successful activation 
#' of the addon) or \code{FALSE} upon failure.
#' 
#' @seealso 
#' \code{\link{addon}}
#' 
#' @examples
#' \dontrun{
#' # Install "stsim" SyncroSim package
#' addPackage("stsim")
#' 
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession, 
#'                          package = "stsim")
#' 
#' # Enable add on package
#' enableAddon(myLibrary, c("stsimsf"))
#' addon(myLibrary)
#' 
#' # Disable add on package
#' disableAddon(myLibrary, c("stsimsf"))
#' addon(myLibrary)
#' }
#' 
#' @export
setGeneric("enableAddon", function(ssimLibrary, name) standardGeneric("enableAddon"))

#' @rdname enableAddon
setMethod("enableAddon", signature(ssimLibrary = "character"), function(ssimLibrary, name) {
  return(SyncroSimNotFound(ssimLibrary))
})

#' @rdname enableAddon
setMethod("enableAddon", signature(ssimLibrary = "SsimLibrary"), function(ssimLibrary, name) {
  enabled <- NULL
  cAdds <- addon(ssimLibrary)
  retList <- list()
  for (i in seq(length.out = length(name))) {
    cVal <- name[i]
    if (!is.element(cVal, cAdds$name)) {
      print(paste0("Warning - ", cVal, " is not among the available addons: ", paste(cAdds$name[cAdds$enabled == "No"], collapse = ",")))
      retList[[cVal]] <- FALSE
      next
    }
    cAddsLess <- subset(cAdds, enabled == FALSE)
    if (!is.element(cVal, cAddsLess$name)) {
      message(paste0(cVal, " is already enabled."))
      retList[[cVal]] <- FALSE
      next
    }

    tt <- command(list(create = NULL, addon = NULL, lib = .filepath(ssimLibrary), name = cVal), .session(ssimLibrary))
    if (tt == "saved"){
      message(paste0("Addon <", cVal, "> enabled"))
      retList[[cVal]] <- TRUE
    } else {
      message(tt)
      retList[[cVal]] <- FALSE
    }
  }

  return(invisible(retList))
})
