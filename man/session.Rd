% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/session.R
\name{session}
\alias{session}
\alias{session,missingOrNULLOrChar-method}
\alias{session,SsimObject-method}
\alias{session,Folder-method}
\alias{session<-}
\alias{session<-,NULLOrChar-method}
\alias{session<-,SsimObject-method}
\title{Create or return SyncroSim Session}
\usage{
session(x = NULL, silent = TRUE, printCmd = FALSE)

\S4method{session}{missingOrNULLOrChar}(x = NULL, silent = TRUE, printCmd = FALSE)

\S4method{session}{SsimObject}(x = NULL, silent = TRUE, printCmd = FALSE)

\S4method{session}{Folder}(x = NULL, silent = TRUE, printCmd = FALSE)

session(ssimObject) <- value

\S4method{session}{NULLOrChar}(ssimObject) <- value

\S4method{session}{SsimObject}(ssimObject) <- value
}
\arguments{
\item{x}{character or SsimObject. Path to SyncroSim installation. If \code{NULL}
(default), then default path is used}

\item{silent}{logical. Applies only if x is a path or \code{NULL} If \code{TRUE}, warnings
from the console are ignored. Otherwise they are printed. Default is \code{FALSE}}

\item{printCmd}{logical. Applies only if x is a path or \code{NULL} If \code{TRUE},
arguments passed to the SyncroSim console are also printed. Helpful for
debugging. Default is \code{FALSE}}

\item{ssimObject}{\code{\link{Project}} or \code{\link{Scenario}} object}

\item{value}{\code{\link{Session}} object}
}
\value{
A SyncroSim \code{\link{Session}} object.
}
\description{
Methods to create or return a SyncroSim \code{\link{Session}}.
}
\details{
In order to avoid problems with SyncroSim version compatibility and SsimLibrary
updating, the new Session must have the same filepath as the Session of the
SsimObject
e.g. \code{filepath(value)==filepath(session(ssimObject))}.
Therefore, the only time when you will need to set a new SyncroSim Session is if you
have updated the SyncroSim software and want to update an existing SsimObject
to use the new software.
}
\examples{
\donttest{
# Specify file path and name of new SsimLibrary
myLibraryName <- file.path(tempdir(), "testlib")

# Set up a SyncroSim Session, SsimLibrary, and Project
mySession <- session()
myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
myProject <- project(myLibrary, project = "Definitions")

# Lists the folder location of SyncroSim Session
filepath(mySession)

# Lists the version of SyncroSim Session
version(mySession)

# Data frame of the packages installed with this version of SyncroSim
package(mySession) 

# Data frame of the base packages installed with this version of SyncroSim
package(mySession, installed = "BASE") 

# Set a new SyncroSim Session for the SyncroSim Project
session(myProject) <- session(x = filepath(session(myProject)))
}

}
