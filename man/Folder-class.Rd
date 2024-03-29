% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AAAClassDefinitions.R
\docType{class}
\name{Folder-class}
\alias{Folder-class}
\alias{Folder}
\title{SyncroSim Folder class}
\description{
\code{Folder} object representing a SyncroSim Folder. A Folder is used to
organize SyncroSim Scenarios within a \code{\link{Project}}, and can be
nested within other Folders at the project-level. These are used mostly in
the SyncroSim User Interface.
}
\section{Slots}{

\describe{
\item{\code{session}}{\code{\link{Session}} object. The Session associated with the
Folder's SsimLibrary}

\item{\code{filepath}}{character string. The path to the Folder's SsimLibrary on disk}

\item{\code{folderId}}{integer. The Folder id}

\item{\code{parentId}}{integer. The parent Folder id (if the folder is nested)}

\item{\code{projectId}}{integer. The Project id}
}}

\seealso{
See \code{\link{folder}} for options when creating or loading a
SyncroSim Folder
}
