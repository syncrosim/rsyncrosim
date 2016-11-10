# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' Add a row to a dataframe.
#'
#' Adds a row to a dataframe.
#'
#' @param x A dataframe.
#' @param values A list of values for a row.
#' @return A dataframe .
#' @export
setRows<-function(x,values){
  #x=mySheet
  x$IsIn = T
  for(i in seq(length(conditions))){
    x$IsIn[x[[names(conditions[i])]]!=conditions[i]]=F
  }
  for(i in seq(length(values))){
    x[[names(values)[i]]][x$IsIn]=values[[i]]
  }
  x$IsIn=NULL
  return(x)
}
