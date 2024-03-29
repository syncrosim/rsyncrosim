% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addRow.R
\name{addRow}
\alias{addRow}
\alias{addRow,data.frame-method}
\title{Add row(s) to a data.frame}
\usage{
addRow(targetDataframe, value)

\S4method{addRow}{data.frame}(targetDataframe, value)
}
\arguments{
\item{targetDataframe}{data.frame}

\item{value}{data.frame, character string, vector, or list. Columns or elements
in value should be a subset of columns in targetDataframe}
}
\value{
A dataframe with new rows.
}
\description{
This function is mostly used internally to add rows to data.frames
associated with SyncroSim Datasheets retrieved from the command line.
}
\details{
Preserves the types and factor levels of the targetDataframe.
Fills missing values if possible using factor levels.
If value is a named vector or list, it will be converted to a single row
data.frame. If value is an unnamed vector or list, the number of elements
should equal the number of columns in the targetDataframe; elements are
assumed to be in same order as data.frame columns.
}
\examples{
# Create an example data.frame
oldDataframe <- as.data.frame(mtcars)

# Add a single row to the example data.frame
newDataframe <- addRow(oldDataframe, list(mpg = 100, wt = 10))

# Create an example data.frame with more than one row of data
multipleRows <- data.frame(mpg = c(40, 50, 75), wt = c(4, 7, 6))

# Add the old example data.frame to the new example data.frame
newDataframe <- addRow(oldDataframe, multipleRows)

}
