# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' Dataframe from SyncroSim output
#'
#' \code{dataframeFromSSim} converts output from SyncroSim to a dataframe.
#'
#' @param x Output from \code{\link{command()}}
#' @param colNames A vector of column names.
#' @return A data frame of output from the SyncroSim console.
#' @examples
#' # Use a default session to create a new library
#' myArgs = list(list=NULL,columns=NULL,lib="C:/Temp/NewLibrary.ssim",sheet="STSim_Stratum",pid=1)
#' myOutput = command(args=myArgs,mySsim,printCmd=T)
#' myDataframe = dataframeFromSSim(myOutput)
#' myDataframe
#' @export
.dataframeFromSSim<-function(x,colNames=NULL){
  #colNames=c("name","description","version");x=tt
  if(is.null(colNames)){
    if(length(x)<2){
      stop("Input does not have a header row and cannot be converted to a table.")
    }
  }
  while(max(grepl("   ",x))){
    x = gsub("   ","  ",x)
  }
  rows = strsplit(x,"  ")
  out=data.frame(temp=c(NA))
  if(is.null(colNames)){
    colNames = rows[[1]]
  }
  for(i in seq(length.out=length(colNames))){
    #i = 1
    out[[colNames[i]]]=c(NA)
  }
  out=subset(out,!is.na(temp))
  out$temp=NULL

  if(identical(colNames,rows[[1]])){
    skip=2
  }else{skip=0}
  for(i in seq(length.out=length(rows))){
    if(i<=skip){next}
    if(length(rows[[i]])!=ncol(out)){
      stop(paste("Need the same number of columns in each row.",rows[[1]],";",rows[[i]]))
    }
    for(j in seq(length.out=length(rows[[i]]))){
      out[i-skip,j]=rows[[i]][j]
    }
  }
  return(out)
}
