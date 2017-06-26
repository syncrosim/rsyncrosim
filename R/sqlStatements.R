# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Get SELECT and GROUP BY Statements
#'
#' Creates SELECT and GROUP BY SQL Satements.
#' Variables are column names.
#' Variables not included in groupBy or aggregate will be dropped from the table.
#'
#' @param groupBy Vector of variables to GROUP BY.
#' @param aggregate Vector of variables to aggregate using aggregateFunction
#' @param aggregateFunction An SQL aggregate function (e.g. SUM, COUNT)
#' @param where A list of subset variables.
#' @return A list of SQL SELECT and GROUP BY statements.
#' @export
sqlStatements<-function(groupBy=NULL,aggregate=NULL,aggregateFunction="SUM",where=NULL){
  #groupBy=NULL;aggregate=NULL;aggregateFunction="SUM";where=list(Timestep=c(0,1,2),Iteration=c(3,4))
  if(is.null(groupBy)){selectSQL="SELECT *"
  }else{selectSQL = paste0("SELECT ",paste(groupBy,collapse=","))}
  if(!is.null(aggregate)){
    selectSQL=paste0(selectSQL,",",paste(paste0(aggregateFunction,"(",aggregate,") AS ",aggregate),collapse=","))
  }
  if(!is.null(aggregate)){
    groupBySQL = paste0("GROUP BY ",paste(groupBy,collapse=","))
  }else{
    groupBySQL = ""
  }
  if(!is.null(where)){
    whereSQL = "WHERE "
    for(i in 1:length(where)){
      whereSQL=paste0(whereSQL,"(",names(where)[i]," IN (",paste(where[[i]],collapse=","),"))")
      if(i!=length(where)){
        whereSQL =paste0(whereSQL," AND ")
      }
    }
    return(list(select=selectSQL,groupBy=groupBySQL,where=whereSQL))
  }else{
    return(list(select=selectSQL,groupBy=groupBySQL))
  }
  
}
