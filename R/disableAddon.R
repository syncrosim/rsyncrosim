# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
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
#' TODO - update examples
#' myLibrary = ssimLibrary()
#' enableAddon(myLibrary,c("stsim-ecological-departure"))
#' addons(myLibrary)
#' disableAddon(myLibrary,c("stsim-ecological-departure"))
#' addons(myLibrary)
#'
#' @export
setGeneric('disableAddon',function(ssimLibrary,name) standardGeneric('disableAddon'))
setMethod('disableAddon', signature(ssimLibrary="SsimLibrary"), function(ssimLibrary,name) {
  #x=myLibrary
  #value = c("stsim-ecological-departure", "stsim-stock-flow")
  cAdds = addons(ssimLibrary,all=T)
  name=gsub(":add-on-transformer","",name,fixed=T)
  retList = list()
  for(i in seq(length.out=length(name))){
    #i=1
    cVal = name[i]
    if(!is.element(cVal,cAdds$shortName)){
      print(paste0("Warning - ",cVal," is not among the available addons: ",paste(cAdds$shortName[cAdds$enabled=="No"],collapse=",")))
      next
    }
    cAddsLess = subset(cAdds,enabled=="Yes")
    if(!is.element(cVal,cAddsLess$shortName)){
      print(paste0(cVal," is already disabled."))
      next
    }
    
    tt=command(list(delete=NULL,addon=NULL,force=NULL,lib=.filepath(ssimLibrary),name=paste0(cVal,":add-on-transformer")),.session(ssimLibrary))
    retList[[cVal]]=tt
  }
  
  #ssimLibrary@datasheetNames = .datasheets(ssimLibrary,scope="all",refresh=T)
  return (retList)
}
)
