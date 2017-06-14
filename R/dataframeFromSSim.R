# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
# Dataframe from SyncroSim output
#
# \code{dataframeFromSSim} converts output from SyncroSim to a dataframe.
#
# @param x Output from \code{\link{command()}}
# @param colNames A vector of column names.
# @param csv If T assume comma separation. Otherwise, assume undefined white space separation.
# @param localNames If T, remove spaces from column names and make camelCase.
# @return A data frame of output from the SyncroSim console.
# @examples
# # Use a default session to create a new library
# myArgs = list(list=NULL,columns=NULL,lib="C:/Temp/NewLibrary.ssim",sheet="STSim_Stratum",pid=1)
# myOutput = command(args=myArgs,mySsim)
# myDataframe = dataframeFromSSim(myOutput)
# myDataframe
# @export
.dataframeFromSSim<-function(x,colNames=NULL,csv=T,localNames=T,convertToLogical=NULL){
  #colNames=c("name","description","version");
  #x=c("Property,Value","Size:,\"35,526 KB\"");csv=T;colNames=NULL;localNames=T
  #x=tt;localNames=T;colNames=NULL;csv=T;convertToLogical=NULL
  if(is.null(colNames)){
    header=T
  }else{
    header=F
  }
  if(csv){
    con = textConnection(x)
    out = read.csv(con,stringsAsFactors=F,header=header)
    close(con)
  }else{
    #for(i in seq(length.out=length(x))){
    #  stop("here") 
    #}
    
    if(1){
    #Do the old wierd thing if not csv
    while(max(grepl("   ",x,fixed=T))){
      x = gsub("   ","  ",x,fixed=T)
    }
    x = gsub("  ",",",x,fixed=T)
    con = textConnection(x)
    out = read.csv(con,stringsAsFactors=F,header=header,sep=",")
    if(!is.null(colNames)){
      lastName = names(out)[length(names(out))]
      if((ncol(out)>length(colNames))&(sum(!is.na(out[[lastName]]))==0)){
        out[[lastName]]=NULL
      }
      names(out)=colNames
    }
    }
    close(con)
  }
  if(localNames){
    names(out)=gsub(" ","",names(out))
    names(out)=gsub(".","",names(out),fixed=T)
    names(out)=sapply(names(out),camel)
  }
  if(!is.null(convertToLogical)){
    for(i in seq(length.out=length(convertToLogical))){
      cName = convertToLogical[[i]]
      if(is.element(cName,names(out))){
        out[[cName]][out[[cName]]=="No"]=F
        out[[cName]][out[[cName]]=="Yes"]=T
        out[[cName]]=as.logical(out[[cName]])
      }
    }
  }
  return(out)
}
