# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Disable addon or addons.
#'
#' Disable addon or addons of an SsimLibrary, or Project/Scenario with an associated SsimLibrary.
#'
#' @param ssimLibrary SsimLibrary
#' @param name Character string or vector of these.
#' @return saved or error message.
#' @examples
#' #TODO - update examples
#' myLibrary = ssimLibrary()
#' enableAddon(myLibrary,c("stsim-ecological-departure"))
#' addon(myLibrary)
#' disableAddon(myLibrary,c("stsim-ecological-departure"))
#' addon(myLibrary)
#'
#' @export
setGeneric('disableAddon',function(ssimLibrary,name) standardGeneric('disableAddon'))
#' @rdname disableAddon
setMethod('disableAddon', signature(ssimLibrary="character"), function(ssimLibrary,name) {
  return(SyncroSimNotFound(ssimLibrary))
})
#' @rdname disableAddon
setMethod('disableAddon', signature(ssimLibrary="SsimLibrary"), function(ssimLibrary,name) {
  enabled=NULL
  cAdds = subset(addon(ssimLibrary))
  retList = list()
  for(i in seq(length.out=length(name))){
    cVal = name[i]
    if(!is.element(cVal,cAdds$name)){
      print(paste0("Warning - ",cVal," is not among the available addons: ",paste(cAdds$name[cAdds$enabled=="No"],collapse=",")))
      next
    }
    cAddsLess = subset(cAdds,enabled==T)
    if(!is.element(cVal,cAddsLess$name)){
      print(paste0(cVal," is already disabled."))
      next
    }
    
    tt=command(list(delete=NULL,addon=NULL,force=NULL,lib=.filepath(ssimLibrary),name=cVal),.session(ssimLibrary))
    retList[[cVal]]=tt
  }
  
  return (retList)
}
)
