# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License

setClassUnion("missingOrNULL", c("missing", "NULL"))
setClassUnion("missingOrNULLOrChar", c("missing", "NULL", "character"))
setClassUnion("NULLOrChar", c("NULL", "character"))

# NOTE: Constructors for each class are defined in the R file bearing the name of the
# class (lower case). e.g. session.R, ssimLibrary.R, etc.

#' SyncroSim Session class
#'
#' A SyncroSim Session object contains a link to a SyncroSim installation. 
#' \code{SsimLibrary}, \code{Project} and \code{Scenario} objects contain a 
#' \code{Session} used to query and modify the object.
#'
#' @seealso See \code{\link{session}} for options when creating a Session.
#' @slot filepath The path to the SyncroSim installation
#' @slot silent If \code{FALSE}, all SyncroSim output with non-zero exit status is 
#'     printed. Helpful for debugging. Default is \code{TRUE}
#' @slot printCmd If \code{TRUE}, arguments passed to the SyncroSim console are also 
#'     printed. Helpful for debugging. Default is \code{FALSE}
#' @slot condaFilepath The path to the Conda installation. Default is \code{"default"}
#' @name Session-class
#' @rdname Session-class
#' @export Session
Session <- setClass("Session", representation(filepath = "character", 
                                              silent = "logical", 
                                              printCmd = "logical", 
                                              condaFilepath = "NULLOrChar"))

# SyncroSim Object class (nor exported)
# SsimLibrary, Project and Scenario all inherit from this abstract class.
#
# @slot session The SyncroSim \code{\link{Session}}.
# @slot filepath The path to the Library on disk.
# @slot datasheetNames The names and scope of all datasheets in the Library. 
# Used to speed calculations.
SsimObject <- setClass("SsimObject", 
                       representation(session = "Session", 
                                      filepath = "character", 
                                      datasheetNames = "data.frame"))

#' SyncroSim Library class
#'
#' \code{SsimLibrary} object representing a SyncroSim Library. A SsimLibrary is the
#' highest level of organization in the SyncroSim workflow and contains at 
#' least one \code{\link{Project}}.
#'
#' @seealso See \code{\link{ssimLibrary}} for options when creating or loading a 
#'     SyncroSim SsimLibrary.
#' @slot session \code{\link{Session}} object
#' @slot filepath character string. The path to the SsimLibrary on disk
#' @slot datasheetNames character string. The name and scope of all Datasheets
#'  in the SsimLibrary.
#' @name SsimLibrary-class
#' @rdname SsimLibrary-class
#' @export SsimLibrary
SsimLibrary <- setClass("SsimLibrary", contains = "SsimObject", 
                        representation())

#' SyncroSim Scenario class
#'
#' \code{Scenario} object representing a SyncroSim Scenario. A Scenario is the 
#' lowest level of organization in the SyncroSim workflow, and is often used 
#' to isolate information on a single Datasheet. 
#'
#' @seealso See \code{\link{scenario}} for options when creating or loading a 
#'     SyncroSim Scenario.
#' @slot session \code{\link{Session}} object. The Session associated with the 
#' Scenario
#' @slot filepath character string. The path to the Scenario's SsimLibrary on disk
#' @slot datasheetNames character string. Names and scope of all Datasheets in
#'  Scenario's SsimLibrary
#' @slot projectId integer. The Project id
#' @slot scenarioId integer. The Scenario id
#' @slot parentId integer. For a result Scenario, this is the id of the parent 
#' Scenario. 0 indicates this is not a result Scenario
#' @slot folderId integer. The folder in which the Scenario exists. If the Scenario
#' exists at the root of the project, then this value is NULL.
#' @slot breakpoints list of Breakpoint objects (optional)
#' @name Scenario-class
#' @rdname Scenario-class
#' @export Scenario
Scenario <- setClass("Scenario", contains = "SsimObject", 
                     representation(projectId = "numeric", 
                                    scenarioId = "numeric", 
                                    parentId = "numeric", 
                                    folderId = "numeric", 
                                    breakpoints = "list"))

#' SyncroSim Project class
#'
#' \code{Project} object representing a SyncroSim Project. A Project is the intermediate
#' level of organization in the SyncroSim workflow, between the \code{\link{ssimLibrary}} 
#' and the \code{\link{scenario}}. It contains information relevant to a group 
#' of Scenarios.
#'
#' @seealso See \code{\link{project}} for options when creating or loading a 
#'     SyncroSim Project.
#' @slot session \code{\link{Session}} object. The Session associated with the 
#' Project's SsimLibrary
#' @slot filepath character string. The path to the Project's SsimLibrary on disk
#' @slot datasheetNames Names and scopes of datasheets in the Project's Library
#' @slot projectId integer. The Project id
#' @name Project-class
#' @rdname Project-class
#' @export Project
Project <- setClass("Project", contains = "SsimObject", 
                    representation(projectId = "numeric"))

#' SyncroSim Folder class
#'
#' \code{Folder} object representing a SyncroSim Folder. A Folder is used to 
#' organize SyncroSim Scenarios within a \code{\link{Project}}, and can be 
#' nested within other Folders at the project-level. These are used mostly in 
#' the SyncroSim User Interface.
#'
#' @seealso See \code{\link{folder}} for options when creating or loading a 
#' SyncroSim Folder
#' @slot session \code{\link{Session}} object. The Session associated with the 
#' Folder's SsimLibrary
#' @slot filepath character string. The path to the Folder's SsimLibrary on disk
#' @slot folderId integer. The Folder id
#' @slot parentId integer. The parent Folder id (if the folder is nested)
#' @slot projectId integer. The Project id
#' @name Folder-class
#' @rdname Folder-class
#' @export Folder
Folder <- setClass("Folder", contains = "SsimObject",
                   representation(session = "Session", 
                                  filepath = "character", 
                                  folderId = "numeric", 
                                  parentId = "numeric", 
                                  projectId = "numeric"))

#' SyncroSim Chart class
#'
#' \code{Chart} object representing a SyncroSim Chart object. A Chart object
#' is used to create line or column charts from tabular output data in the 
#' and can be viewed using the SyncroSim User Interface.
#'
#' @seealso See \code{\link{chart}} for options when creating or loading a 
#' SyncroSim Chart
#' @slot session \code{\link{Session}} object. The Session associated with the 
#' Chart's SsimLibrary
#' @slot filepath character string. The path to the Chart's SsimLibrary on disk
#' @slot chartId integer. The Chart id
#' @slot projectId integer. The Project id
#' @name Chart-class
#' @rdname Chart-class
#' @export Chart
Chart <- setClass("Chart", contains = "SsimObject", 
                  representation(session = "Session", 
                                 filepath = "character", 
                                 chartId = "numeric",
                                 projectId = "numeric"))


