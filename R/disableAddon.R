# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Disable addon or addons.
#'
#' Disable \code{\link{addon}} or addons of a \code{\link{SsimLibrary}}, 
#' \code{\link{Project}} or \code{\link{Scenario}} with an associated SsimLibrary.
#'
#' @param ssimLibrary An object of class SsimLibrary.
#' @param name Character string or vector of addon names.
#' 
#' @return
#' This function invisibly returns `TRUE` upon success (i.e.successful deactivation
#' of the addon) or `FALSE` upon failure.
#' 
#' @seealso 
#' \code{\link{addon}}
#' 
#' @examples
#' \donttest{
#' temp_dir <- tempdir()
#' myses <- session()
#' myLibrary <- ssimLibrary(name = file.path(temp_dir,"testlib"), session = myses)
#' 
#' enableAddon(myLibrary, c("stsimecodep"))
#' addon(myLibrary)
#' disableAddon(myLibrary, c("stsimecodep"))
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
