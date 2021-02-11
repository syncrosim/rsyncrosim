# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License

setClassUnion("missingOrNULL", c("missing", "NULL"))
setClassUnion("missingOrNULLOrChar", c("missing", "NULL", "character"))

# NOTE: Constructors for each class are defined in the R file bearing the name of the
# class (lower case). e.g. session.R, ssimLibrary.R, etc.

#' SyncroSim Session class
#'
#' A SyncroSim Session object contains a link to a SyncroSim installation.  \code{SsimLibrary}, \code{Project}
#' and \code{Scenario} objects contain a \code{Session} used to query and modify the object.
#'
#' @seealso See \code{\link{session}} for options when creating a Session.
#' @slot filepath The path to the SyncroSim installation.
#' @slot silent If FALSE, all SyncroSim output with non-zero exit status is printed. Helpful for debugging. Default=TRUE.
#' @slot printCmd If TRUE, arguments passed to the SyncroSim console are also printed. Helpful for debugging. Default=FALSE.
#' @name Session-class
#' @rdname Session-class
#' @export Session
Session <- setClass("Session", representation(filepath = "character", silent = "logical", printCmd = "logical"))

# SyncroSim Object class
# SsimLibrary, Project and Scenario all inherit from this abstract class.
#
# @slot session The SyncroSim Session.
# @slot filepath The path to the Library on disk.
# @slot datasheetNames The names and scope of all datasheets in the Library. Used to speed calculations.
SsimObject <- setClass("SsimObject", representation(session = "Session", filepath = "character", datasheetNames = "data.frame"))

#' SyncroSim Library class
#'
#' \code{SsimLibrary} object representing a SyncroSim Library.
#'
#' @seealso See \code{\link{ssimLibrary}} for options when creating or loading a SyncroSim Library.
#' @slot session The SyncroSim Session.
#' @slot filepath The path to the Library on disk.
#' @slot datasheetNames The name and scope of all datasheets in the Library.
#' @name SsimLibrary-class
#' @rdname SsimLibrary-class
#' @export SsimLibrary
SsimLibrary <- setClass("SsimLibrary", contains = "SsimObject", representation())

#' SyncroSim Scenario class
#'
#' \code{Scenario} object representing a SyncroSim Scenario.
#'
#' @seealso See \code{\link{scenario}} for options when creating or loading a SyncroSim Scenario.
#' @slot session The Session associated with the Scenario.
#' @slot filepath The path to the Scenario's Library on disk.
#' @slot datasheetNames Names and scope of all datasheets in Scenario's Library.
#' @slot projectId The project id.
#' @slot scenarioId The scenario id.
#' @slot parentId For a result scenario, this is the id of the parent scenario. 0 indicates this is not a result scenario.
#' @slot breakpoints An (optional) list of Breakpoint objects.
#' @name Scenario-class
#' @rdname Scenario-class
#' @export Scenario
Scenario <- setClass("Scenario", contains = "SsimObject", representation(projectId = "numeric", scenarioId = "numeric", parentId = "numeric", breakpoints = "list"))

#' SyncroSim Project class
#'
#' \code{Project} object representing a SyncroSim Project.
#'
#' @seealso See \code{\link{project}} for options when creating or loading a SyncroSim Project.
#' @slot session The Session associated with the Project's Library.
#' @slot filepath The path to the Project's Library on disk.
#' @slot datasheetNames Names and scopes of datasheets in the Project's Library.
#' @slot projectId The Project id.
#' @name Project-class
#' @rdname Project-class
#' @export Project
Project <- setClass("Project", contains = "SsimObject", representation(projectId = "numeric"))
