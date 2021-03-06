% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sqlStatement.R
\name{sqlStatement}
\alias{sqlStatement}
\title{Construct an SQLite query}
\usage{
sqlStatement(
  groupBy = NULL,
  aggregate = NULL,
  aggregateFunction = "SUM",
  where = NULL
)
}
\arguments{
\item{groupBy}{Character string or vector of these. Vector of variables (column names) to GROUP BY.}

\item{aggregate}{Character string of vector of these. Vector of variables (column names) to aggregate using aggregateFunction.}

\item{aggregateFunction}{Character string. An SQL aggregate function (e.g. SUM, COUNT).}

\item{where}{Named list. A list of subset variables. Names are column names, and elements are the values to be selected from each column.}
}
\value{
Returns a list of SELECT, GROUP BY and WHERE SQL statements used by the \code{\link{datasheet}} function to construct an SQLite database query.
}
\description{
Creates SELECT, GROUP BY and WHERE SQL statements.
The resulting list of SQL statements will be converted to an SQLite database query by the \code{\link{datasheet}} function.
}
\details{
Variables are column names of the datasheet. See column names using datasheet(,empty=TRUE).
Variables not included in groupBy, aggregate or where will be dropped from the table.
Note that it is not possible to construct a complete SQL query at this stage,
because the \code{\link{datasheet}} function may add ScenarioID and/or ProjectID to the query.
}
\examples{
\donttest{
# Query the total Amount for each combination of ScenarioID, Iteration, Timestep and StateLabelXID,
# including only Timesteps 0,1 and 2, and Iterations 3 and 4.
mySQL <- sqlStatement(
  groupBy = c("ScenarioID", "Iteration", "Timestep", "StateLabelXID"),
  aggregate = c("Amount"), where = list(Timestep = c(0, 1, 2), Iteration = c(3, 4))
)
mySQL
}

}
