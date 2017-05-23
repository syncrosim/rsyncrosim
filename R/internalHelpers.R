getIdsFromListOfObjects<-function(ssimObject,expecting,scenario=NULL,project=NULL){
  if(class(ssimObject[[1]])!=expecting){
    if(expecting=="Scenario"){
      stop("ssimObject should be a SsimLibrary/Project/Scenario, path to a library, or list of Scenarios.")
    }
    if(expecting=="Project"){
      stop("ssimObject should be a SsimLibrary/Project/Scenario, path to a library, or list of Projects.")
    }
  }
  cLib=.ssimLibrary(ssimObject[[1]])
  if(!is.null(scenario)){
    warning("scenario argument is ignored when ssimObject is a list of Scenarios.")
  }
  if(!is.null(project)){
    warning("project argument is ignored when ssimObject is a list of Projects or Scenarios.")
  }
  objs=c()
  for(i in seq(length.out=length(ssimObject))){
    cObj = ssimObject[[i]]
    if(class(cObj)!=expecting){
      if(expecting=="Scenario"){
        stop("ssimObject should be a SsimLibrary/Project/Scenario, path to a library, or list of Scenarios.")
      }
      if(expecting=="Project"){
        stop("ssimObject should be a SsimLibrary/Project/Scenario, path to a library, or list of Projects.")
      }
    }
    if(.filepath(cObj)!=.filepath(cLib)){
      stop("All elements of ssimObject must belong to the same library.")
    }
    if(expecting=="Scenario"){
      objs=c(objs,.scenarioId(cObj))
    }
    if(expecting=="Project"){
      objs=c(objs,.projectId(cObj))
    }
  }
  ssimObject = cLib
  return(list(ssimObject=ssimObject,objs=objs))
}
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

