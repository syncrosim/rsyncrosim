# Copyright © 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#Internal wrappers for functions that are overwritten internally by arguments of the same name.
#Export for developmet and debugging.
#' @include session.R
#' @include ssimLibrary.R
#' @include project.R
#' @include scenario.R
#' @include sqlStatements.R
#' @include projectId.R
#' @include scenarioId.R
#' @include filepath.R
#' @include addons.R
#' @include datasheet.R
#' @include datasheets.R
#' @include name.R
NULL
# @export
.projectId = projectId
# @export
.scenarioId = scenarioId
# @export
.filepath=filepath
# @export
.session=session
# @export
.project = project
# @export
.scenario = scenario
# @export
.addons=addons
# @export
.datasheet=datasheet
# @export
.name=name
# @export
.datasheets=datasheets
# @export
.sqlStatements=sqlStatements
# @export
#.breakpoints=breakpoints
# @export
#.breakpoint=breakpoint


