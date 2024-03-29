% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssimEnvironment.R
\name{runtimeTempFolder}
\alias{runtimeTempFolder}
\title{SyncroSim Temporary Folder}
\usage{
runtimeTempFolder(folderName)
}
\arguments{
\item{folderName}{character. The folder name}
}
\value{
Returns a temporary folder name.
}
\description{
This function is part of a set of functions designed to facilitate the
development of R-based Syncrosim Packages. This function creates and returns
a SyncroSim Temporary Folder.
}
\examples{
\dontrun{
tempFolder <- runtimeTempFolder()
}

}
