# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' Dataframe from SyncroSim output
#'
#' \code{dataframeFromSSim} converts output from SyncroSim to a dataframe.
#'
#' @param x Output from \code{\link{command()}}
#' @return A data frame of output from the SyncroSim console.
#' @examples
#' #Use a default session to creat a new library
#' myArgs = list(list=NULL,columns=NULL,lib="C:/Temp/NewLibrary.ssim",sheet="STSim_Stratum",pid=1)
#' myOutput = command(args=myArgs,mySsim,printCmd=T)
#' myDataframe = dataframeFromSSim(myOutput)
#' myDataframe
# @export
.dataframeFromSSim<-function(x){
  if(length(x)<2){
    stop("Input does not have a header row and cannot be converted to a table.")
  }
  while(max(grepl("   ",x))){
    x = gsub("   ","  ",x)
  }
  rows = strsplit(x,"  ")
  out=data.frame(temp=c(NA))
  for(i in seq(length(rows[[1]]))){
    #i = 1
    out[[rows[[1]][i]]]=c(NA)
  }
  out=subset(out,!is.na(temp))
  out$temp=NULL

  for(i in seq(length(rows))){
    if(i<=2){next}
    if(length(rows[[i]])!=ncol(out)){
      stop(paste("Need the same number of columns in each row.",rows[[1]],";",rows[[i]]))
    }
    for(j in seq(length(rows[[i]]))){
      out[i-2,j]=rows[[i]][j]
    }
  }
  return(out)
}
