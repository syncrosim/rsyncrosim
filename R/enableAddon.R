# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Enable addon or addons.
#'
#' Enable addon or addons of an SsimLibrary.
#'
#' @param ssimLibrary SsimLibrary
#' @param name Character string or vector of these.
#' @return saved or error message for each addon.
#' @examples
#' TODO - update examples
#' myLibrary = ssimLibrary()
#' enableAddon(myLibrary,c("stsim-ecological-departure", "stsim-stock-flow"))
#' addon(myLibrary)
#' @export
setGeneric('enableAddon',function(ssimLibrary,name) standardGeneric('enableAddon'))
#' @rdname enableAddon
setMethod('enableAddon', signature(ssimLibrary="SsimLibrary"), function(ssimLibrary,name) {
  enabled=NULL
  cAdds = addon(ssimLibrary,all=T)
  name=gsub(":add-on-transformer","",name,fixed=T)
  retList=list()
  for(i in seq(length.out=length(name))){
    #i=1
    cVal = name[i]
    if(!is.element(cVal,cAdds$name)){
      print(paste0("Warning - ",cVal," is not among the available addons: ",paste(cAdds$name[cAdds$enabled=="No"],collapse=",")))
      next
    }
    cAddsLess = subset(cAdds,enabled=="No")
    if(!is.element(cVal,cAddsLess$name)){
      print(paste0(cVal," is already enabled."))
      next
    }
    
    tt=command(list(create=NULL,addon=NULL,lib=.filepath(ssimLibrary),name=cVal),.session(ssimLibrary))
    retList[[cVal]]=tt
  }
  
  return (retList)
}
)
