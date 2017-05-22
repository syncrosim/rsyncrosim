#get scnSet
getScnSet<-function(ssimObject){
  #get current scenario info
  tt = command(list(list=NULL,scenarios=NULL,csv=NULL,lib=.filepath(ssimObject)),.session(ssimObject))
  scnSet=.dataframeFromSSim(tt,localNames=T)
  names(scnSet)[names(scnSet)=="scenarioID"]="id"
  names(scnSet)[names(scnSet)=="projectID"]="pid"
  if(nrow(scnSet)==0){
    scnSet=merge(scnSet,data.frame(id=NA,exists=NA),all=T)
    scnSet=subset(scnSet,!is.na(id))
  }else{
    scnSet$exists=T
  }
  return(scnSet)
}

#get projectSet
getProjectSet<-function(ssimObject){
  tt = command(list(list=NULL,projects=NULL,csv=NULL,lib=.filepath(ssimObject)),.session(ssimObject))
  if(identical(tt,"saved")){
    projectSet = data.frame(id=NA,name=NA,exists=NA)
    projectSet=subset(projectSet,!is.na(id))
  }else{
    projectSet=.dataframeFromSSim(tt)
    names(projectSet)[names(projectSet)=="iD"]="id"
    projectSet$exists = T
  }
  return(projectSet)
}

#make first character of string lower case
camel <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}

#http://stackoverflow.com/questions/26083625/how-do-you-include-data-frame-output-inside-warnings-and-errors
#' @export
printAndCapture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}

#Get name of parent scenario from result scenario name.
.getParentName<-function(x){
  out = strsplit(x," ([",fixed=T)[[1]][1]
  return(out)
}

