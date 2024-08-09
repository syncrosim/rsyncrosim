# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

# Internal wrappers for functions that are overwritten internally by arguments of the same name.
# Export for development and debugging.
#' @include session.R
#' @include ssimLibrary.R
#' @include project.R
#' @include scenario.R
#' @include folder.R
#' @include chart.R
#' @include sqlStatement.R
#' @include projectId.R
#' @include scenarioId.R
#' @include folderId.R
#' @include chartId.R
#' @include filepath.R
#' @include datasheet.R
#' @include internalHelpers.R
#' @include name.R
#' @include packages.R
NULL
# @export
.projectId <- projectId
# @export
.scenarioId <- scenarioId
# @export
.folderId <- folderId
# @export
.chartId <- chartId
# @export
.filepath <- filepath
# @export
.tempfilepath <- tempfilepath
# @export
.session <- session
# @export
.project <- project
# @export 
.folder <- folder
# @export
.chart <- chart
# @export
.scenario <- scenario
# @export
.datasheet <- datasheet
# @export
.name <- name
# @export
.datasheets <- datasheets
# @export
.subset <- subset
# @export
.sqlStatement <- sqlStatement
# @export
.packages <- packages
