# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Disable addon package(s)
#'
#' Disable \code{\link{addon}} package(s) of a \code{\link{SsimLibrary}}.
#'
#' @param ssimLibrary \code{\link{SsimLibrary}} object
#' @param name character string or vector of addon name(s)
#' 
#' @return
#' This function invisibly returns \code{TRUE} upon success (i.e.successful deactivation
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
#' # Enable addon package
#' enableAddon(myLibrary, c("stsimsf"))
#' addon(myLibrary)
#' 
#' # Disable addon package
#' disableAddon(myLibrary, c("stsimsf"))
#' addon(myLibrary)
#' }
#' 
#' @export
setGeneric("disableAddon", function(ssimLibrary, name) standardGeneric("disableAddon"))

#' @rdname disableAddon
setMethod("disableAddon", signature(ssimLibrary = "character"), function(ssimLibrary, name) {
  return(SyncroSimNotFound(ssimLibrary))
})

#' @rdname disableAddon
setMethod("disableAddon", signature(ssimLibrary = "SsimLibrary"), function(ssimLibrary, name) {
  enabled <- NULL
  cAdds <- subset(addon(ssimLibrary))
  retList <- list()

  for (i in seq(length.out = length(name))) {
    cVal <- name[i]
    if (!is.element(cVal, cAdds$name)) {
      print(paste0("Warning - ", cVal, " is not among the available addons: ", paste(cAdds$name[cAdds$enabled == "No"], collapse = ",")))
      retList[[cVal]] <- FALSE
      next
    }
    cAddsLess <- subset(cAdds, enabled == TRUE)
    if (!is.element(cVal, cAddsLess$name)) {
      message(paste0(cVal, " is already disabled."))
      retList[[cVal]] <- FALSE
      next
    }

    tt <- command(list(delete = NULL, addon = NULL, force = NULL, lib = .filepath(ssimLibrary), name = cVal), .session(ssimLibrary))
    if (tt == "saved"){
      message(paste0("Addon <", cVal, "> disabled"))
      retList[[cVal]] <- TRUE
    } else {
      message(tt)
      retList[[cVal]] <- FALSE
    }
  }

  return(invisible(retList))
})
