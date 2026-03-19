# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Construct an SQLite query
#'
#' Creates \code{SELECT}, \code{GROUP BY} and \code{WHERE} SQL statements.
#' The resulting list of SQL statements will be converted to an SQLite database 
#' query by the \code{\link{datasheet}} function.
#'
#' @param groupBy character string or vector of these. Vector of variables 
#'     (column names) to \code{GROUP BY} (optional)
#' @param aggregate character string of vector of these. Vector of variables 
#'     (column names) to aggregate using \code{aggregateFunction} (optional)
#' @param aggregateFunction character string. An SQL aggregate function 
#'     (e.g. \code{SUM}, \code{COUNT}). Default is \code{SUM}
#' @param where named list. A list of subset variables. Names are column names, 
#'     and elements are the values to be selected from each column (optional)
#'
#' @details
#' Variables are column names of the Datasheet. See column names using \code{datasheet(,empty=TRUE)}
#' Variables not included in \code{groupBy}, \code{aggregate} or \code{where} will be dropped from the table.
#' Note that it is not possible to construct a complete SQL query at this stage,
#' because the \code{\link{datasheet}} function may add ScenarioId and/or ProjectId to the query.
#'   
#' @return 
#' Returns a list of \code{SELECT}, \code{GROUP BY} and \code{WHERE} SQL statements used by the 
#' \code{\link{datasheet}} function to construct an SQLite database query.
#'
#' @examples
#' \dontrun{
#' # Query total Amount for each combination of ScenarioId, Iteration, Timestep and StateLabelXID,
#' # including only Timesteps 0,1 and 2, and Iterations 3 and 4.
#' mySQL <- sqlStatement(
#'   groupBy = c("ScenarioId", "Iteration", "Timestep"),
#'   aggregate = c("yCum"),
#'   aggregateFunction = "SUM",
#'   where = list(Timestep = c(0, 1, 2), Iteration = c(3, 4))
#' )
#' mySQL
#' }
#' \dontrun{
#' # The SQL statement can then be used in the datasheet function
#' 
#' # Set the file path and name of an existing SsimLibrary
#' myLibraryName <- file.path("MyLibrary.ssim")
#' 
#' # Set the SyncroSim Session, SsimLibrary, Project, and Scenario
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName,
#'                          session = mySession)
#' myProject <- project(myLibrary, project = "Definitions")
#' myScenario <- scenario(myProject, scenario = "My Scenario")
#' 
#' # Run Scenario to generate results
#' resultScenario <- run(myScenario)
#' 
#' # Use the SQL statement when loading the Datasheet
#' myAggregatedDataFrame <- datasheet(resultScenario, 
#'                                    name = "helloworldSpatial_OutputDatasheet",
#'                                    sqlStatement = mySQL)
#'                                    
#' # View aggregated DataFrame
#' myAggregatedDataFrame
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
