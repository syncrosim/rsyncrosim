# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' Add a row to a datasheet.
#'
#' Adds a row to a dataframe.
#' Missing values are filled if possible using factor levels.
#'
#' @param x A dataframe.
#' @param value A dataframe. Columns in should be a subset of columns in x.
#' @return A dataframe with a new row.
#' @export
setGeneric('addRow<-',function(x,value) standardGeneric('addRow<-'))
setReplaceMethod(
  f="addRow",
  signature="data.frame",
  definition=function(x,value){
    #x=mySheet;value=data.frame(StateClassIDSource="Coniferous:All",StateClassIDDest="Deciduous:All",TransitionTypeID="Fire",Probability=0.01)
    for(i in seq(length.out=ncol(value))){
      if(class(value[[i]])=="factor"){value[[i]]=as.character(value[[i]])}
    }
    if(length(setdiff(colnames(value),colnames(x)))>0){
      stop("Columns in values should be a subset of columns in x.")
    }
    xTemp =x
    if(nrow(xTemp)>0){
      xTemp$temp = 1
      xTemp = subset(xTemp,temp==2)
    }
    combos = allCombos(xTemp,onlyFactors=T)


    if(nrow(combos)>0){
      findRow = merge(value,combos)
      if(nrow(findRow)>1){
        stop("More info needed to identify a unique combination of factors:\n", printAndCapture(findRow))
      }
      if(nrow(findRow)==0){
        stop("Invalid combination of factors")
      }
    }else{
      findRow = value
    }

    #now fill in missing values
    missingCols = setdiff(colnames(x),colnames(findRow))
    for (i in seq(length.out=length(missingCols))){
      findRow[[missingCols[i]]]=NA
    }
    out = rbind(x,findRow)

    for(i in seq(length.out=ncol(out))){
      #i=4
      cCol = colnames(out)[i]
      if(is.element(cCol,colnames(combos))){
        levels(out[[cCol]])=levels(combos[[cCol]])
      }else{
        if(is.character(x[[cCol]])){
          out[[cCol]]=as.character(out[[cCol]])
        }
        if(is.numeric(x[[cCol]])){
          out[[cCol]]=as.numeric(as.character(out[[cCol]]))
        }
      }
    }
    out=subset(out,select=colnames(x))
    return(out)
  }
)
