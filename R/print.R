# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#Overwrite default print methods to improve display of SsimObjects. 
#Inspired by print methods for raster objects in raster package.

setMethod ('print', 'SsimObject', 
  function(x, ...) {
    show(x)
  }
)
setMethod ('print', 'Session', 
           function(x, ...) {
             show(x)
           }
)

setMethod ('show' , 'SsimObject', 
  function(object) {
    .printSsim(object)
  }
)

setMethod ('show' , 'Session', 
           function(object) {
             .printSsim(object)
           }
)

.printSsim <- function(x, ...) {
  #x = myScenario
  cat('class:' , class(x), '\n')

  cNames = slotNames(x)
  for(i in seq(length.out=length(cNames))){
    #i = 7
    cName = cNames[i]
    cSlot = slot(x,cName)
    outString = paste0(cName," [",class(cSlot),"]") 
    if(is.element(class(cSlot),c("numeric","character","logical"))){
      outString = paste(outString,paste(cSlot,collapse=","))
    }

    if(is.element(class(cSlot),"Session")){
      outString=paste0(outString," ",.filepath(cSlot),", printCmd=",printCmd(cSlot),", defaultModel=",defaultModel(cSlot))
    }
    if(is.element(class(cSlot),"data.frame")){
      outString=paste0(outString," ",paste(names(cSlot),collapse=","))
    }
    
    outString=paste0(outString,'\n')
    cat(outString)
        
  }
  
}