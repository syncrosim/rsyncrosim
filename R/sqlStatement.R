# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Construct an SQLite query
#'
#' Creates SELECT, GROUP BY and WHERE SQL statements.
#' The resulting list of SQL statements will be converted to an SQLite database query by the \code{\link{datasheet}} function.
#'
#' @details
#' Variables are column names of the datasheet. See column names using datasheet(,empty=TRUE).
#' Variables not included in groupBy, aggregate or where will be dropped from the table.
#' Note that it is not possible to construct a complete SQL query at this stage,
#' because the \code{\link{datasheet}} function may add ScenarioID and/or ProjectID to the query.
#'
#' @param groupBy Character string or vector of these. Vector of variables (column names) to GROUP BY.
#' @param aggregate Character string of vector of these. Vector of variables (column names) to aggregate using aggregateFunction.
#' @param aggregateFunction Character string. An SQL aggregate function (e.g. SUM, COUNT).
#' @param where Named list. A list of subset variables. Names are column names, and elements are the values to be selected from each column.
#' 
#' @return 
#' Returns a list of SELECT, GROUP BY and WHERE SQL statements used by the \code{\link{datasheet}} function to construct an SQLite database query.
#'
#' @examples
#' \donttest{
#' # Query the total Amount for each combination of ScenarioID, Iteration, Timestep and StateLabelXID,
#' # including only Timesteps 0,1 and 2, and Iterations 3 and 4.
#' mySQL <- sqlStatement(
#'   groupBy = c("ScenarioID", "Iteration", "Timestep", "StateLabelXID"),
#'   aggregate = c("Amount"), where = list(Timestep = c(0, 1, 2), Iteration = c(3, 4))
#' )
#' mySQL
#' }
#' 
#' @export
sqlStatement <- function(groupBy = NULL, aggregate = NULL, aggregateFunction = "SUM", where = NULL) {
  if (is.null(groupBy)) {
    selectSQL <- "SELECT *"
  } else {
    selectSQL <- paste0("SELECT ", paste(groupBy, collapse = ","))
  }
  if (!is.null(aggregate)) {
    selectSQL <- paste0(selectSQL, ",", paste(paste0(aggregateFunction, "(", aggregate, ") AS ", aggregate), collapse = ","))
  }
  if (!is.null(aggregate)) {
    groupBySQL <- paste0("GROUP BY ", paste(groupBy, collapse = ","))
  } else {
    groupBySQL <- ""
  }
  if (!is.null(where)) {
    whereSQL <- "WHERE "
    for (i in 1:length(where)) {
      whereSQL <- paste0(whereSQL, "(", names(where)[i], " IN (", paste(where[[i]], collapse = ","), "))")
      if (i != length(where)) {
        whereSQL <- paste0(whereSQL, " AND ")
      }
    }
    return(list(select = selectSQL, groupBy = groupBySQL, where = whereSQL))
  } else {
    return(list(select = selectSQL, groupBy = groupBySQL))
  }
}
