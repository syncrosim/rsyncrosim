deleteDatasheet<-function(datasheet,datasheets,cProj=NULL,cScn=NULL,cProjName=NULL,cScnName=NULL,out=list(),force){
  for(j in seq(length.out(datasheet))){
    cName = datasheet[j]
    cSheet = subset(datasheets,name==cName)
    if(nrow(cSheet)==0){
      stop("datasheet ",cName," not found in object identified by ssimObject/project/scenario arguments.")
    }
    targs = list(delete=NULL,data=NULL,lib=.filepath(ssimObject),sheet=cName,force=NULL)
    outName=cName
    if(cSheet$scope=="project"){
      targs[["pid"]]=cProj
      outName = paste0(outName," pid",cProj)
      addPrompt = paste0(" from project ",name,"(",cProj,")")
    }
    if(cSheet$scope=="scenario"){
      targs[["sid"]]=cScn
      outName = paste0(outName," sid",cScn)
      addPrompt = paste0(" from scenario ",name,"(",cScn,")")
    }
    
    if(force){
      answer="y"
    }else{
      promptString = paste0("Do you really want to delete datasheet ",cName,addPrompt,"? (y/n): ")
      answer <- readline(prompt=promptString)
    }
    if(!is.element(outName,names(out))){#don't try something again that has already been tried
      if(answer=="y"){
        outBit = command(targs,.session(ssimObject))
      }else{
        outBit = "skipped"
      }
      out[[outName]]=outBit
    }
  }
}

getIdsFromListOfObjects<-function(ssimObject,expecting=NULL,scenario=NULL,project=NULL){
  if(is.null(expecting)){
    expecting=class(ssimObject[[1]])
  }
  if(class(ssimObject[[1]])!=expecting){
    if(expecting=="character"){
      stop("Expecting a list of library paths.")
    }else{
      stop("Expecting a list of ",expecting,"s.")
    }
  }
  cLib=.ssimLibrary(ssimObject[[1]],create=F)
  if(!is.null(scenario)){
    warning("scenario argument is ignored when ssimObject is a list.")
  }
  if(!is.null(project)){
    warning("project argument is ignored when ssimObject is a list.")
  }
  objs=c()
  for(i in seq(length.out=length(ssimObject))){
    cObj = ssimObject[[i]]
    if(expecting=="character"){
      cObj = .ssimLibrary(cObj,create=F)
    }
    
    if(class(cObj)!=expecting){
      stop("All elements of ssimObject should be of the same type.")
    }
    if((expecting!="character")&&(.filepath(cObj)!=.filepath(cLib))){
      stop("All elements of ssimObject must belong to the same library.")
    }
    if(expecting=="Scenario"){
      objs=c(objs,.scenarioId(cObj))
    }
    if(expecting=="Project"){
      objs=c(objs,.projectId(cObj))
    }
    if(is.element(expecting,c("character","SsimLibrary"))){
      objs=c(objs,cObj)
      
    }
  }
  ssimObject = cLib
  if(expecting=="character"){expecting="SsimLibrary"}
  return(list(ssimObject=ssimObject,objs=objs,expecting=expecting))
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

