# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#Internal wrappers for functions that are overwritten internally by arguments of the same name.
#Export for developmet and debugging.
#' @include session.R
#' @include ssimLibrary.R
#' @include project.R
#' @include scenario.R
#' @include sqlStatement.R
#' @include projectId.R
#' @include scenarioId.R
#' @include filepath.R
#' @include addon.R
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
.tempfilepath=tempfilepath
# @export
.session=session
# @export
.project = project
# @export
.scenario = scenario
# @export
.addon=addon
# @export
.datasheet=datasheet
# @export
.name=name
# @export
.datasheets=datasheets
# @export
.subset=subset
# @export
.sqlStatement=sqlStatement


