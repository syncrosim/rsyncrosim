# Author: Josie Hughes
# Date : November 2016
# Version 0.1
# Licence GPL v3
#' Get SELECT and GROUP BY Statements
#'
#' Creates SELECT and GROUP BY SQL Satements.
#' Variables are column names.
#' Variables not included in groupBy or aggregate will be dropped from the table.
#'
#' @param groupBy Vector of variables to GROUP BY.
#' @param aggregate Vector of variables to aggregate using aggregateFunction
#' @param aggregateFunction An SQL aggregate function (e.g. SUM, COUNT)
#' @return A list of SQL SELECT and GROUP BY statements.
#' @export
sqlStatements<-function(groupBy=NULL,aggregate=NULL,aggregateFunction="SUM"){
  selectSQL = paste0("SELECT ",paste(groupBy,collapse=","))
  if(!is.null(aggregate)){
    selectSQL=paste0(selectSQL,",",paste(paste0(aggregateFunction,"(",aggregate,") AS ",aggregate),collapse=","))
  }
  if(!is.null(aggregate)){
    groupBySQL = paste0("GROUP BY ",paste(groupBy,collapse=","))
  }else{
    groupBySQL = ""
  }
  return(list(select=selectSQL,groupBy=groupBySQL))
}
