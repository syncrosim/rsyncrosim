# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Delete SsimLibrary, Project, Scenario, Folder, Chart or Datasheet
#'
#' Deletes one or more items. Note that this is irreversible.
#'
#' @param ssimObject \code{\link{SsimLibrary}}, \code{\link{Project}},
#'     \code{\link{Scenario}}, \code{\link{Folder}}, or \code{\link{Chart}} 
#'     object, or character (i.e. path to a SsimLibrary)
#' @param project character string, numeric, or vector of these. One or more 
#'     \code{\link{Project}} names or ids. Note that project argument is ignored 
#'     if ssimObject is a list. Note that integer ids are slightly faster (optional)
#' @param scenario character string, numeric, or vector of these. One or more 
#'     \code{\link{Scenario}} names or ids. Note that scenario argument is 
#'     ignored if ssimObject is a list. Note that integer ids are slightly faster 
#'     (optional)
#' @param folder character string, numeric, or vector of these. One or more 
#'     \code{\link{Folder}} names or ids. Note that folder argument is 
#'     ignored if ssimObject is a list. Note that integer ids are slightly faster 
#'     (optional)
#' @param chart character string, numeric, or vector of these. One or more 
#'     \code{\link{Chart}} names or ids. Note that chart argument is 
#'     ignored if SsimObject is a list. Note that integer ids are slightly faster 
#'     (optional)
#' @param datasheet character string or vector of these. One or more datasheet 
#' names (optional)
#' @param force logical. If \code{FALSE} (default), user will be prompted to approve 
#'     removal of each item
#' @param removeBackup logical. If \code{TRUE}, will remove the backup folder when
#'     deleting a library. Default is \code{FALSE}
#' @param removePublish logical. If \code{TRUE}, will remove the publish folder when
#'     deleting a library. Default is \code{FALSE}
#' @param removeCustom logical. If \code{TRUE} and custom folders have been configured
#'     for a library, then will remove the custom publish and/or backup folders when 
#'     deleting a library. Note that the `removePublish` and `removeBackup` arguments 
#'     must also be set to \code{TRUE} to remove the respective custom folders. Default
#'     is \code{FALSE}
#' @param session \code{\link{Session}} object. If \code{NULL} (default), session()
#'     will be used. Only applicable when `ssimObject` argument is a character
#' 
#' @return 
#' Invisibly returns a list of boolean values corresponding to each
#' input: \code{TRUE} upon success (i.e.successful deletion) and \code{FALSE} upon failure.
#' 
#' @examples
#' \donttest{
#' # Specify file path and name of new SsimLibrary
#' myLibraryName <- file.path(tempdir(), "testlib")
#' 
#' # Set up a SyncroSim Session, SsimLibrary, and Project
#' mySession <- session()
#' myLibrary <- ssimLibrary(name = myLibraryName, session = mySession)
#' myProject <- project(myLibrary, project = "a project")
#' 
#' # Check the Projects associated with this SsimLibrary
#' project(myLibrary)
#' 
#' # Delete Project
#' delete(myLibrary, project = "a project", force = TRUE)
#' 
#' # Check that Project was successfully deleted from SsimLibrary
#' project(myLibrary)
#' }
#' 
#' @export
# Note delete supports character paths because sometimes we want to delete a 
# library without updating it.
# Note delete supports project/scenario/folder/chart arguments because sometimes 
# we want to delete objects without creating them.
setGeneric("delete", 
           function(ssimObject, project = NULL, scenario = NULL, 
                    folder = NULL, chart = NULL, datasheet = NULL, 
                    force = FALSE, removeBackup = FALSE, removePublish = FALSE,
                    removeCustom = FALSE, session = NULL) standardGeneric("delete"))

#' @rdname delete
setMethod("delete", signature(ssimObject = "character"), 
          function(ssimObject, project, scenario, folder, chart, datasheet, 
                   force, removeBackup, removePublish, removeCustom, session) {
  browser()        
  if (is.null(datasheet) && is.null(project) && is.null(scenario) && 
      is.null(folder) && is.null(chart)) {
    
    return(deleteLibrary(ssimObject, force, removeBackup, removePublish, 
                         removeCustom, session))
  } else {
    
    if (ssimObject == SyncroSimNotFound(warn = FALSE)) {
      
      return(SyncroSimNotFound())
    }
    
    ssimObject <- .ssimLibrary(ssimObject)
    return(delete(ssimObject, project, scenario, folder, chart, datasheet, 
                  force, session))
  }
})

#' @rdname delete
setMethod("delete", signature(ssimObject = "SsimObject"), 
          function(ssimObject, project, scenario, folder, chart, datasheet, 
                   force, session) {
  browser()
  ScenarioId <- NULL
  
  xProjScn <- .getFromXProjScn(ssimObject, project = project, 
                               scenario = scenario, folder = folder,
                               chart = chart, returnIds = TRUE, 
                               convertObject = FALSE, complainIfMissing = TRUE)
  
  if (is(xProjScn, "Folder")) {
    
    if (is.null(folder)) {
      folderId <- .folderId(ssimObject)
      folderName <- .name(ssimobject)
    } else {
      allFolders <- getFolderData(ssimObject)
      
      if (is.character(folder)) {
        folderId <- subset(allFolders, Name %in% folder)$FolderId
        folderName <- subset(allFolders, Name %in% folder)$Name
      } else if (is.numeric(folder)){
        folderId <- subset(allFolders, FolderId %in% folder)$FolderId
        folderName <- subset(allFolders, FolderId %in% folder)$Name
      }
    }
    
    if (length(folderId) == 0){
      stop(paste0("The specified folder(s) does not exist: ", 
                  paste0(folder, collapse=",")))
    }
    
    out <- deleteFolder(xProjScn, folderId, folderName, out = list(), force)
    
    return(invisible(out))
  }
  
  if (is(xProjScn, "Chart")) {
    
    if (is.null(chart)) {
      chartId <- .chartId(ssimObject)
      chartName <- .name(ssimObject)
    } else {
      allCharts <- getChartData(ssimObject)
      
      if (is.character(chart)) {
        chartId <- subset(allCharts, Name %in% chart)$ChartId
        chartName <- subset(allCharts, Name %in% chart)$Name
      } else if (is.numeric(chart)){
        chartId <- subset(allCharts, ChartId %in% chart)$ChartId
        chartName <- subset(allCharts, ChartId %in% chart)$Name
      }
    }
    
    if (length(chartId) == 0){
      stop(paste0("The specified chart(s) does not exist: ", 
                  paste0(chart, collapse=",")))
    }
    
    out <- deleteChart(xProjScn, chartId, chartName, out = list(), force)
    
    return(invisible(out))
  }
  
  # expect to have a vector of valid project or scenario ids - checking already done
  x <- xProjScn$ssimObject
  project <- xProjScn$project
  scenario <- xProjScn$scenario
  goal <- xProjScn$goal
  
  if (goal == "library") {
    stop(paste0("Error in delete: SyncroSim libraries must be deleted using ", 
         "the deleteLibrary() function."))
  }
  
  if (goal == "project") {
    
    if (!is.numeric(project)) {
      stop("Error in delete: project ids are not numeric.")
    }
    
    allProjects <- xProjScn$projectSet
    if (!is.null(datasheet)) {
      out <- deleteProjectDatasheet(x, datasheet, project, allProjects, 
                                    out = list(), force)
    } else {
      out <- deleteProject(x, project, allProjects, out = list(), force)
    }
    
    return(invisible(out))
  }
  
  if (goal == "scenario") {
    
    if (!is.numeric(scenario)) {
      stop("Error in delete: expect to have valid scenario ids.")
    }
    
    if (!is.null(datasheet)){
      allScenarios <- xProjScn$scenarioSet
      out <- deleteScenarioDatasheet(x, datasheet, scenario, allScenarios, 
                                     out = list(), force)
    } else {
      allScenarios <- xProjScn$scenarioSet
      out <- deleteScenario(x, scenario, allScenarios, out = list(), force)
    }
    
    return(invisible(out))
  }
  

  
  stop("Error in delete().")
})
