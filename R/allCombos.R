# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' Get all combinations of factor levels
#'
#' Given an empty data frame, get all possible combinations of factor levels
#'
#' @param x An empty data frame with factors.
#' @param onlyFactors If T, only the factor columns are returned.
#' @return A data frame containing all possible combinations of factor levels.
#' @export
allCombos<-function(x,onlyFactors=F){
  #dat = mySheet
  if(nrow(x)>0){
    stop("Please provide an empty data frame.")
  }
  p= list()
  for(i in seq(length.out=ncol(x))){
    #i = 1
    cName = colnames(x)[i]
    cCol = x[[cName]]
    if(!is.factor(cCol)){
      next
    }else{
      if(length(levels(cCol))==0){
        p[[cName]] = as.factor("")
      }else{
        p[[cName]]=as.factor(levels(cCol))
      }
    }
  }
  if(length(p)==0){return(x)}
  out=expand.grid(p, KEEP.OUT.ATTRS = F)
  if(onlyFactors){return(out)}
  for(i in seq(length.out=ncol(x))){
    #i = 1
    cName = colnames(x)[i]
    cCol = x[[cName]]
    if(is.factor(cCol)){
      next
    }else{
      out[[cName]]=NA
      if(is.character(x[[cName]])){out[[cName]]=as.character(out[[cName]])}
      if(is.numeric(x[[cName]])){out[[cName]]=as.numeric(out[[cName]])}
    }
  }
  return(out)
}
