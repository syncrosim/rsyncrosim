setClassUnion("missingOrNULLOrChar", c("missing", "NULL","character"))
#' The name of a SyncroSim project or scenario.
#'
#' The name of a SyncroSim Project/Scenario.
#'
#' @param ssimObject Project/Scenario.
#' @export
setGeneric('name',function(ssimObject) standardGeneric('name'))

#' Set the project or scenario names.
#'
#' Set the name of a SyncroSim Project or Scenario.
#'
#' @param ssimObject Scenario/Project
#' @param value The new name.
#' @export
setGeneric('name<-',function(ssimObject,value) standardGeneric('name<-'))


#' The projectId of a SyncroSim project or scenario.
#'
#' The projectId of a SyncroSim Project or Scenario.
#'
#' @param ssimObject Project/Scenario.
#' @export
setGeneric('projectId',function(ssimObject) standardGeneric('projectId'))

#' The scenarioId of a scenario.
#'
#' The scenarioId of a Scenario.
#'
#' @param scenario Scenario.
#' @export
setGeneric('scenarioId',function(scenario) standardGeneric('scenarioId'))


#' The path to a SyncroSim object on disk
#'
#' The path to a SyncroSim Session, SSimLibarary, Project or Scenario on disk.
#'
#' @param x An object containing a filepath.
#' @export
setGeneric('filepath',function(x) standardGeneric('filepath'))

#' Information about an object
#'
#' Get basic information about a SyncroSim Session, SSimLibarary, Project or Scenario
#'
#' @param x An object containing info.
#' @export
setGeneric('info',function(x) standardGeneric('info'))


# datasheets
#
# Gets datasheet summary info from an SsimLibrary, Project or Scenario.
#
# @details
# See \code{\link{datasheet}} for discussion of optional/empty/sheetName/lookupsAsFactors arguments.
# \itemize{
#   \item {If x/project/scenario identify a scenario: }{Returns library, project, and scenario scope datasheets.}
#   \item {If x/project/scenario identify a project (but not a scenario): }{Returns library and project scope datasheets.}
#   \item {If x/project/scenario identify a library (but not a project or scenario): }{Returns library scope datasheets.}
# }
#
# @param x An SsimLibrary, Project or Scenario object. Or a path to a SyncroSim library on disk.
# @param project Project name or id. Ignored if x is a Project.
# @param scenario Scenario name or id. Ignored if x is a Scenario.
# @param scope "scenario","project", "library", "all", or NULL.
# @param refresh If FALSE (default) names are retrieved from x@datasheetNames. If TRUE names are retrieved using a console call (slower).
# @return A dataframe of datasheet names.
# @examples
#
# @export
#Note: this function is now internal. Should now only be called from datasheet.
setGeneric('datasheets',function(x,project=NULL,scenario=NULL,scope=NULL,refresh=F) standardGeneric('datasheets'))

#Handles case where x is a path to an SyncroSim library on disk.
setMethod('datasheets', signature(x="character"), function(x,project,scenario,scope,refresh) {
  x = .getFromXProjScn(x,project,scenario)
  out = .datasheets(x,project,scenario,scope,refresh)
  return(out)
})

#' Get a datasheet
#'
#' Gets Syncrosim datasheet.
#'
#' @details
#' If summary=T or summary and name=NULL, a dataframe listing datasheet names and other info is returned. All other arguments are ignored.
#' 
#' Otherwise, for each element in name a datasheet is returned as follows:
#' \itemize{
#'   \item {If lookupsAsFactors=T (default): }{Each column is given the correct data type, and dependencies returned as factors with allowed values (levels). A warning is issued if the lookup has not yet been set.}
#'   \item {If empty=T: }{Each column is given the correct data type. Fast (1 less console command)}
#'   \item {If empty=F and lookupsAsFactors=F: }{Column types are not checked, and the optional argument is ignored. Fast (1 less console command).}
#'   \item {If ssimObject is a list of Scenario or Project objects (output from run(), scenario() or project()): }{Adds ScenarioID/ProjectID column if appropriate.}
#'   \item {If scenario/project is a vector: }{Adds ScenarioID/ProjectID column as necessary.}
#'   \item {If requested datasheet has scenario scope and contains info from more than one scenario: }{ScenarioID/ScenarioName/ScenarioParent columns identify the scenario by name, id, and parent (if a result scenario)}
#'   \item {If requested datasheet has project scope and contains info from more than one project: }{ProjectID/ProjectName columns identify the project by name and id.}
#' }
#'
#' @param ssimObject SsimLibrary/Project/Scenario, list of objects, or path to a library. Note that all objects in a list must be of the same type, and belong to the same library.
#' @param name Character or vector of these. Sheet name(s). If NULL, all datasheets in the ssimObject will be returned. Note that setting summary=F and name=NULL pulls all datasheets, which is timeconsuming and not generally recommended.
#' @param project Character, numeric, or vector of these. One or more Project names, id or objects.
#' @param scenario Character, numeric, or vector of these. One or more Scenario names, id or objects.
#' @param summary Logical. If TRUE returns a dataframe of sheet names and other info. If FALSE returns dataframe or list of dataframes. 
#' @param optional Logical. If TRUE returns all of the datasheet's columns, including the optional columns; otherwise returns only those columns that are mandatory and/or contain data. Ignored if summary=T, or if empty=F and lookupsAsFactors=F.
#' @param empty Logical. If TRUE returns empty dataframes for each datasheet. Ignored if summary=TRUE.
#' @param lookupsAsFactors Logical. If TRUE (default) dependencies returned as factors with allowed values (levels). Set FALSE to speed calculations. Ignored if summary=TRUE.
#' @param sqlStatements List returned by sqlStatements(). SELECT and GROUP BY SQL statements passed to SQLite database. Ignored if summary=TRUE.
#' @param includeKey Logical. If TRUE include primary key in output table.
#' @param forceElements Logical. If FALSE and name has a single element returns a dataframe; otherwise a dataframe. Ignored if summary=TRUE.
#' @return If summary=T returns a dataframe of datasheet names and other info, other wise returns dataframe or list of dataframes representing SyncroSim datasheets.
#' @examples
#'
#' @export
#' @import RSQLite
setGeneric('datasheet',function(ssimObject,name=NULL,project=NULL,scenario=NULL,summary=NULL,optional=F,empty=F,lookupsAsFactors=T,sqlStatements=list(select="SELECT *",groupBy=""),includeKey=F,forceElements=F) standardGeneric('datasheet'))
#Handles case where ssimObject is a path to an SyncroSim library on disk.
setMethod('datasheet', signature(ssimObject="character"), function(ssimObject,name,project,scenario,summary,optional,empty,lookupsAsFactors,sqlStatements,includeKey,forceElements) {
  ssimObject = .getFromXProjScn(ssimObject,project,scenario)
  out = .datasheet(ssimObject,name,project,scenario,summary,optional,empty,lookupsAsFactors,sqlStatements,includeKey,forceElements)
  return(out)
})

#Handles case where ssimObject is list of Scenario or Project objects
setMethod('datasheet', signature(ssimObject="list"), function(ssimObject,name,project,scenario,summary,optional,empty,lookupsAsFactors,sqlStatements,includeKey,forceElements) {
  #x=myResults;name="STSim_OutputStratumState";lookupsAsFactors=T;project=NULL;scenario=NULL;optional=F;empty=F

  cScn = ssimObject[[1]]
  if(!is.element(class(cScn),c("Scenario","Project"))){
    stop("ssimObject must be an SsimLibrary, Project or Scenario object. Or a list of Project/Scenario objects. Or the path to a library on disk.")
  }
  
  if(!is.null(project)|!is.null(scenario)){
    warning("project and scenario arguments are ignored when ssimObject is a Project/Scenario or list of these.")
  }
  project=c();scenario=c()


  for(i in seq(length.out=length(ssimObject))){
    cScn = ssimObject[[1]]
    if(class(cScn)!=class(ssimObject[[1]])){
      stop("Elements of ssimObject must all be of the same type (Project or Scenario).")
    }
    if(.filepath(cScn)!=.filepath(ssimObject[[1]])){
      stop("Elements of ssimObject must all be contained in the same library.")
    }

    #get list of scenario/projects and pass to datasheet again
    if(class(cScn)=="Scenario"){
      project=NULL
      scenario=c(scenario,scenarioId(cScn))
    }
    
    if(class(cScn)=="Project"){
      project=c(project,projectId(cScn))
      scenario=NULL
    }
    
  }
  
  
    
  out = .datasheet(.ssimLibrary(cScn),name=name,project=project,scenario=scenario,summary=summary, optional=optional,empty=empty,lookupsAsFactors=lookupsAsFactors,sqlStatements=sqlStatements,includeKey=includeKey,forceElements=forceElements)
  
  return(out)
})

#' Set datasheets
#'
#' Loads datasheets into the SyncroSim library.
#'
#' @param x An SsimLibrary, Project or Scenario object. Or the path to a library on disk.
#' @param data A dataframe or named list of dataframes to load.
#' @param name The sheet name - required if data is a dataframe, ignored otherwise.
#' @param project Project name or id.
#' @param scenario Scenario name or id.
#' @param breakpoint Set to TRUE when modifying datasheets in a breakpoint function.
#' @return A named list of success or failure reports.
#' @examples
#'
#' @export
setGeneric('loadDatasheets',function(x,data,name,project=NULL,scenario=NULL,breakpoint=F) standardGeneric('loadDatasheets'))
#Handles case where x is a path to an SyncroSim library on disk.
setMethod('loadDatasheets', signature(x="character"), function(x,data,name,project,scenario,breakpoint) {
  x = .getFromXProjScn(x,project,scenario)
  out = loadDatasheets(x,data,name,project,scenario,breakpoint)
  return(out)
})

#' Set spatial data
#'
#' Loads spatial data into the SyncroSim library.
#'
#' @details
#'
#' Metadata should be a dataframe that can be appended to datasheet(x,metadata$SheetName[1]), containing 1 row for each layer of data.
#' "SheetName" and at least one "FileName" columns are expected in metadata.
#' A "RasterLayerName" column in metadata is optional. If present, it should contain the name of each layer in the data raster stack.
#' If "RasterLayerName" is not included in metadata, names(data) should correspond to the FileNames in metadata.
#' 
#' Note: Spatial data will be appended if non-FileName columns (e.g. Iteration, Timestep, etc) differ between metadata and datasheet(x,metadata$SheetName[1]). Otherwise, the new spatial data will overwrite the old spatial data.
#'
#' @param x An SsimLibrary, Project or Scenario object. Or the path to a library on disk.
#' @param data A RasterLayer or RasterStack to load.
#' @param metadata A dataframe that can be appended to datasheet(x,metadata$SheetName[1]) - see details. 
#' @param project Project name or id.
#' @param scenario Scenario name or id.
#' @param breakpoint Set to TRUE when setting spatial data in a breakpoint function.
#' @param check Default is TRUE. Set FALSE to speed calculations my assuming metadata is valid.
#' @return A named list of success or failure reports.
#' @examples
#'
#' @export
setGeneric('loadSpatialData',function(x,data,metadata,project=NULL,scenario=NULL,breakpoint=F,check=T) standardGeneric('loadSpatialData'))
#Handles case where x is a path to an SyncroSim library on disk.
setMethod('loadSpatialData', signature(x="character"), function(x,data,metadata,project,scenario,breakpoint,check) {
  x = .getFromXProjScn(x,project,scenario)
  out = loadSpatialData(x,data,metadata,project,scenario,breakpoint,check)
  return(out)
})


#' Run scenarios
#'
#' Run one or more SyncroSim scenarios
#'
#' @param ssimObject SsimLibrary/Project/Scenario or a list of Scenarios. Or the path to a library on disk.
#' @param scenario character, integer, or vector of these. Scenario names or ids. Or NULL.
#' @param summary Logical. If FALSE (default) result Scenario objects are returned. If TRUE (faster) result scenario ids are returned.
#' @param jobs The number of jobs to run. Passed to SyncroSim where multithreading is handled.
#' @param forceElements Logical. If TRUE then returns a single result scenario as a named list; otherwise returns a single result scenario as a Scenario object. Applies only when summary=FALSE.
#' @return If summary=F a result Scenario object or a named list of result Scenarios. If summary=F a scenario id or named vector of ids. The name is the parent scenario for each result.
#' @examples
#'
#' @export
setGeneric('run',function(ssimObject,scenario=NULL,summary=F,jobs=1,forceElements=F) standardGeneric('run'))
#Handles case where ssimObject is a path to an SyncroSim library on disk.
setMethod('run', signature(ssimObject="character"), function(ssimObject,scenario,summary,jobs,forceElements) {
  ssimObject = .ssimLibrary(ssimObject)
  out = run(ssimObject,scenario,summary,jobs,forceElements)
  return(out)
})
#Handles case where ssimObject is a list of Scenarios.
setMethod('run', signature(ssimObject="list"), function(ssimObject,scenario,summary,jobs,forceElements) {
  if(!is.null(scenario)){
    warning("scenario argument is ignored when ssimObject is a list of scenarios.")
  }
  cLib = .filepath(ssimObject[[1]])
  ids = c()
  for(i in seq(length.out=length(ssimObject))){
    cScn = ssimObject[[i]]
    if(class(cScn)!="Scenario"){
      stop("ssimObject must be a SsimLibrary/Project/Scenario or a list of Scenarios. Or the path to a library on disk.")
    }
    if(.filepath(cScn)!=cLib){
      stop("Scenarios in ssimObject must all belong to the same library.")
    }
    ids = c(ids,.scenarioId(cScn))
  }
  out=run(.ssimLibrary(cScn),scenario=ids,summary=summary,jobs=jobs,forceElements=forceElements)
  return(out)
})

#' Get spatial inputs or outputs from a SyncroSim scenario.
#'
#' Get spatial inputs or outputs from a SyncroSim scenario.
#' @details
#'
#' The Color column of a rat table should have one of these formats:
#' \itemize{
#'   \item {R,G,B,alpha: } {4 numbers representing red, green, blue and alpha, separated by commas, and scaled between 0 and 255. See rgb() for details.}
#'   \item {R colour names: } {See colors() for options.}
#'   \item {hexadecimal colors: } {As returned by R functions such as rainbow(), heat.colors(), terrain.colors(), topo.colors(), gray(), etc.}
#' }
#'
#' The names() of the returned raster stack contain metadata.
#' For datasheets without Filename this is: paste0(<datasheet name>,".Scn",<scenario id>,".",<tif name>)
#' For datasheets containing Filename this is: paste0(<datasheet name>,".Scn",<scenario id>,".It",<iteration>,".Ts",<timestep>)
#'
#' @param x A SyncroSim results Scenario or list of SyncroSim result Scenarios.
#' @param sheet The name of a spatial datasheet.
#' @param iterations A vector of iterations. If NULL(default) all available iterations will be included
#' @param nameFilters A vector of strings. Only layer name that include these terms will be returned.
#' @param timesteps A vector of timesteps. If NULL(default) all available timesteps will be included.
#' @param rat An (optional) raster attribute table. This is dataframe with ID, (optional) Color, and other columns. See raster::ratify() for details.
#' @return A RasterStack or RasterBrick object. See raster package documentation for details.
#' @export
setGeneric('spatialData',function(x,sheet,iterations=NULL,timesteps=NULL,nameFilters=NULL,rat=NULL) standardGeneric('spatialData'))
setMethod('spatialData', signature(x="list"), function(x,sheet,iterations,timesteps,nameFilters,rat) {
  # x= myResult; sheet="STSim_InitialConditionsSpatial";iterations=NULL;timesteps = NULL;rat=NULL
  if(class(x[[1]])!="Scenario"){
    stop("Expecting a Scenario object or list of scenario objects.")
  }
  for(i in 1:length(x)){
    #i=1
    cScn = x[[i]]
    cOut = spatialData(cScn,sheet,iterations,timesteps,nameFilters,rat)

    if(i == 1){out=cOut}else{out=raster::stack(out,cOut)}
  }
  return(out)
})

#' Modify the grouping of spatial layers.
#'
#' Modify the grouping of spatial output layers in a SyncroSim results scenario.
#'
#' @examples
#' # Update an old scenario to allow rsyncrosim to access spatial output
#' multiband(myResultScenario,action="rebuild")
#'
#' # Combine spatial outputs into multi-band rasters containing a layer for each timetep.
#' multiband(myResultScenario,action="apply",grouping="Timestep")
#'
#' # Combine spatial outputs into multi-band rasters containing a layer for each iteration.
#' multiband(myResultScenario,action="apply",grouping="Iteration")
#'
#' # Combine spatial outputs into multi-band rasters containing a layer for each timestep and iteration.
#' multiband(myResultScenario,action="apply",grouping="All")
#'
#' # Remove multi-banding
#' multiband(myResultScenario,action="remove")
#'
#' @param x A SyncroSim results Scenario or list of SyncroSim result Scenarios.
#' @param action Options are: apply, remove, rebuild
#' @param grouping Only used if action=apply. If NULL use datasheet(myLibrary,name="STime_Options"). Options are: Iteration,Timestep,All
#' @return "saved" or an error message from SyncroSim.
#' @export
setGeneric('multiband',function(x,action,grouping=NULL) standardGeneric('multiband'))
setMethod('multiband', signature(x="list"), function(x,action,grouping) {
  #x=myResult;action="rebuild";grouping=NULL

  if(class(x[[1]])!="Scenario"){
    stop("Expecting a Scenario object or list of scenario objects.")
  }
  out=list()
  for(i in 1:length(x)){
    #i=1
    cScn = x[[i]]
    cOut = multiband(cScn,action,grouping)
    out[[as.character(.scenarioId(cScn))]]=cOut
  }
  return(out)
})

# Get or set a socket connection.
#
# @param x An ipAddress or BreakpointSession object. If NULL a default ip will be used.
# @param port For new connections only - a port number.
# @export
setGeneric('connection',function(x,...) standardGeneric('connection'))
# @describeIn connection Get a new connection.
setMethod('connection',signature(x="missingOrNULLOrChar"),
          function(x='127.0.0.1',port=13000) {
            #port=13000;ipAddress='127.0.0.1'
            ipAddress = x
            con = socketConnection(host = ipAddress, port=port,open="r+",encoding="UTF-8",blocking=T,server=F,timeout=4)
            ## S3 method for class 'connection'
            #open(con, open = "r", blocking = TRUE, ...)
            ## S3 method for class 'connection'
            #close(con, type = "rw", ...)
            #flush(con)
            #isOpen(con, rw = "")
            #isIncomplete(con)
            if(!isOpen(con)){
              stop(paste0('Problem connecting to the SyncroSim server. IP:',ipAddress," Port:",port))
            }
            return(con)
          })

