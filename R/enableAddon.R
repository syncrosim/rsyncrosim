# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Enable addon or addons.
#'
#' Enable addon or addons of an SsimLibrary.
#'
#' @param ssimLibrary SsimLibrary
#' @param name Character string or vector of addon names
#' @return Character "saved" in case of success or error message.
#' @examples
#' \dontrun{
#' myLibrary <- ssimLibrary()
#' enableAddon(myLibrary, c("stsim-ecological-departure"))
#' addon(myLibrary)
#' disableAddon(myLibrary, c("stsim-ecological-departure"))
#' addon(myLibrary)
#' }
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
      next
    }
    cAddsLess <- subset(cAdds, enabled == F)
    if (!is.element(cVal, cAddsLess$name)) {
      print(paste0(cVal, " is already enabled."))
      next
    }

    tt <- command(list(create = NULL, addon = NULL, lib = .filepath(ssimLibrary), name = cVal), .session(ssimLibrary))
    retList[[cVal]] <- tt
  }

  return(retList)
})
