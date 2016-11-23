# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' Add rows to a datasheet.
#'
#' Adds rows to a dataframe.
#' Preserves the types and factor levels of x.
#' Fills missing values if possible using factor levels.
#'
#' @param x A dataframe.
#' @param value A dataframe. Columns in value should be a subset of columns in x.
#' @return A dataframe with new rows.
#' @export
setGeneric('addRows<-',function(x,value) standardGeneric('addRows<-'))
setReplaceMethod(
  f='addRows',
  signature="data.frame",
  definition=function(x,value){
    #x=mySheet;value=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",TransitionTypeID="Fire",Probability=0.01)

    if(length(setdiff(names(value),names(x)))>0){
      stop("Column names not recognized: ",paste(setdiff(names(value),names(x))),collapse=",")
    }
    for(i in seq(length.out=ncol(value))){
      #i=4
      cName = names(value)[i]
      if(is.factor(x[[cName]])){
        notAllowed = setdiff(value[[cName]],levels(x[[cName]]))
        if(length(notAllowed)>0){
          stop("Invalid values for ",cName," : ",paste(notAllowed,collapse=","))
        }
      }else{
        if(is.factor(value[[cName]])){
          value[[cName]]=as.character(value[[cName]])
        }
        class(value[[cName]])=class(x[[cName]])
      }
    }

    #Note - will not add row if that exact row already exists.
    out=merge(x,value,all=T)

    #Now fill in missing factor values if possible
    for(i in seq(length.out=ncol(out))){
      #i=5
      cName =names(out)[i]
      if(is.factor(x[[cName]])){
        if(length(levels(x[[cName]]))==1){
          out[[cName]] = levels(x[[cName]])[1]
        }
        out[[cName]]=factor(out[[cName]],levels=levels(x[[cName]]))
      }
    }

    out=subset(out,select=names(x))
    return(out)
  }
)
