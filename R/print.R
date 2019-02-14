# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
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

  outStrings = list()
  
  outStrings[['class']]= class(x)

  cNames = slotNames(x)
  for(i in seq(length.out=length(cNames))){
    cName = cNames[i]
    cSlot = slot(x,cName)
    outName = paste0(cName," [",class(cSlot),"]") 
    
    outString=""
    if(is.element(class(cSlot),c("numeric","character","logical"))){
      outString = paste0(outString,paste(cSlot,collapse=","))
    }

    if(is.element(class(cSlot),"Session")){
      outString=paste0(outString,"",.filepath(cSlot),", printCmd=",printCmd(cSlot))
    }
    if(is.element(class(cSlot),"data.frame")){
      outString=paste0(outString,"",paste(names(cSlot),collapse=","))
    }
    outStrings[[outName]]=outString     
  }
  
  #now pad names with spaces to get allignment.
  maxLength = max(nchar(names(outStrings)))
  for (i in seq(length.out=length(names(outStrings)))){
    names(outStrings)[[i]]=paste0(names(outStrings)[[i]],paste(rep(" ",maxLength-nchar(names(outStrings)[[i]])),collapse=""))
    names(outStrings)[[i]]=paste0(names(outStrings)[[i]],": ")
    
    cat(paste0(names(outStrings)[[i]],outStrings[[i]],"\n"))
  }    
}
