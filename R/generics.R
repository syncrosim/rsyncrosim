setClassUnion("missingOrNULLOrChar", c("missing", "NULL","character"))
#' The name of a SyncroSim project or scenario.
#'
#' The name of a SyncroSim Project or Scenario.
#'
#' @param x An object with a name.
#' @export
setGeneric('name',function(x) standardGeneric('name'))

#' Set the project or scenario names.
#'
#' Set the name of a SyncroSim Project or Scenario.
#'
#' @param x A SyncroSim \code{\link{Project}} or \code{\link{Scenario}} object.
#' @param value The new name.
#' @export
setGeneric('name<-',function(x,value) standardGeneric('name<-'))


#' The id of a SyncroSim project or scenario.
#'
#' The id of a SyncroSim Project or Scenario.
#'
#' @param x An object with an id.
#' @export
setGeneric('id',function(x) standardGeneric('id'))

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


#' datasheets
#'
#' Gets datasheets from an SsimLibrary, Project or Scenario.
#'
#' @details
#' See \code{\link{datasheet}} for discussion of optional/empty/sheetName/lookupsAsFactors arguments.
#' \itemize{
#'   \item {If x/project/scenario identify a scenario: }{Returns library, project, and scenario scope datasheets.}
#'   \item {If x/project/scenario identify a project (but not a scenario): }{Returns library and project scope datasheets.}
#'   \item {If x/project/scenario identify a library (but not a project or scenario): }{Returns library scope datasheets.}
#' }
#'
#' @param x An SsimLibrary, Project or Scenario object. Or a path to a SyncroSim library on disk.
#' @param project Project name or id. Ignored if x is a Project.
#' @param scenario Scenario name or id. Ignored if x is a Scenario.
#' @param names If TRUE (default) returns dataframe of sheet names, ignoring remaining arguments. If FALSE returns a named list of dataframes representing each datasheet.
#' @param scope "scenario","project", "library", "all", or NULL.
#' @param optional If FALSE (default) returns only required columns. If TRUE returns optional columns also. Ignored if empty=F and lookupsAsFactors=F.
#' @param empty If FALSE (default) returns data (if any). If TRUE returns empty dataframe.
#' @param lookupsAsFactors If TRUE (default) lookups are returned as factors with allowed values (levels). Set FALSE to speed calculations.
#' @param refresh If FALSE (default) names are retrieved from x@datasheetNames. If TRUE names are retrieved using a console call (slower).
#' @return A dataframe of datasheet names, or list of datasheets represented by dataframes.
#' @examples
#'
#' @export
setGeneric('datasheets',function(x,project=NULL,scenario=NULL,names=T,scope=NULL,optional=F,empty=F,lookupsAsFactors=T,refresh=F) standardGeneric('datasheets'))

#Handles case where x is a path to an SyncroSim library on disk.
setMethod('datasheets', signature(x="character"), function(x,project,scenario,names,scope,optional,empty,lookupsAsFactors,refresh) {
  x = .getFromXProjScn(x,project,scenario)
  out = .datasheets(x,project,scenario,names,scope,optional,empty,lookupsAsFactors,refresh)
  return(out)
})

#' Get a datasheet
#'
#' Gets Syncrosim datasheet.
#'
#' @details
#' \itemize{
#'   \item {If lookupsAsFactors=T (default): }{Each column is given the correct data type, and dependencies returned as factors with allowed values (levels). A warning is issued if the lookup has not yet been set.}
#'   \item {If empty=T: }{Each column is given the correct data type. Fast (1 less console command)}
#'   \item {If empty=F and lookupsAsFactors=F: }{Column types are not checked, and the optional argument is ignored. Fast (1 less console command).}
#'   \item {If x is a list of Scenario or Project objects (output from run(), scenarios() or project()): }{Adds ScenarioID/ProjectID column if appropriate.}
#'   \item {If length(scenario)>1: }{Adds ScenarioID/ProjectID column if appropriate.}
#'   \item {If requested datasheet has scenario scope and contains info from more than one scenario: }{ScenarioID/ScenarioName/ScenarioParent columns identify the scenario by name, id, and parent (if a result scenario)}
#'   \item {If requested datasheet has project scope and contains info from more than one project: }{ProjectID/ProjectName columns identify the project by name and id.}
#' }
#'
#' @param x An SsimLibrary, Project or Scenario object. Or the path to a library on disk. Or a list of Scenario or Project objects.
#' @param name The sheet name
#' @param project One or more Project names, id or objects.
#' @param scenario One or more Scenario names, id or objects.
#' @param optional If FALSE (default) returns only required columns. If TRUE returns optional columns also. Ignored if empty=F and lookupsAsFactors=F.
#' @param empty If FALSE (default) returns data (if any). If TRUE returns empty dataframe.
#' @param lookupsAsFactors If TRUE (default) dependencies returned as factors with allowed values (levels). Set FALSE to speed calculations.
#' @param sqlStatements SELECT and GROUP BY SQL statements passed to SQLite database.
#' @param includeKey If TRUE include primary key in output table.
#' @return A dataframe representing a SyncroSim datasheet.
#' @examples
#'
#' @export
#' @import RSQLite
setGeneric('datasheet',function(x,name,project=NULL,scenario=NULL,optional=F,empty=F,lookupsAsFactors=T,sqlStatements=list(select="SELECT *",groupBy=""),includeKey=F) standardGeneric('datasheet'))
#Handles case where x is a path to an SyncroSim library on disk.
setMethod('datasheet', signature(x="character"), function(x,name,project,scenario,optional,empty,lookupsAsFactors,sqlStatements,includeKey) {
  x = .getFromXProjScn(x,project,scenario)
  return(out)
})

#Handles case where x is list of Scenario or Project objects
setMethod('datasheet', signature(x="list"), function(x,name,project,scenario,optional,empty,lookupsAsFactors,sqlStatements,includeKey) {
  #x=myResults;name="STSim_OutputStratumState";lookupsAsFactors=T;project=NULL;scenario=NULL;optional=F;empty=F

  cScn = x[[1]]
  if(!is.element(class(cScn),c("Scenario","Project"))){
    stop("x must be an SsimLibrary, Project, Scenario object. Or a list of Scenario objects. Or the path to a library on disk.")
  }

  cName = name
  sheetInfo = subset(.datasheets(cScn,scope="all"),name==cName)
  if(nrow(sheetInfo)==0){
    sheetInfo = subset(.datasheets(cScn,scope="all",refresh=T),name==cName)
    if(nrow(sheetInfo)==0){
      stop("Datasheet ",name, " not found in library.")
    }
  }
  if((sheetInfo$dataScope=="library")&(sqlStatements$select=="SELECT *")){
     out = .datasheet(cScn,name,project=NULL,scenario=NULL,optional=optional,empty=empty,lookupsAsFactors=lookupsAsFactors,sqlStatements=sqlStatements)
     return(out)
  }

  forceConsole=!lookupsAsFactors&(sheetInfo$isOutput)&(sqlStatements$select=="SELECT *")&(length(x)<=4)
  project = c();scenario=c()
  for(i in seq(length.out=length(x))){
    #i=1
    cScn = x[[i]]
    if(class(cScn)=="Scenario"){
      cPid = .pid(cScn)
      project=c(project,cPid)
      cSid = .id(cScn)
      scenarioParent = strsplit(.name(cScn)," ([",fixed=T)[[1]][1]
      mergeInfo = data.frame(ScenarioID=cSid,ProjectID=cPid,ScenarioName=.name(cScn),ScenarioParent=scenarioParent,stringsAsFactors=F)
      scenario=c(scenario,cSid)
    }
    if(class(cScn)=="Project"){
      cPid= .id(cScn)
      mergeInfo =data.frame(ProjectID = cPid,ProjectName=.name(cScn),stringsAsFactors=F)
      project=c(project,cPid)
    }

    if(forceConsole){
       outBit = .datasheet(cScn,name,project=NULL,scenario=NULL,optional=optional,empty=empty,lookupsAsFactors=lookupsAsFactors,sqlStatements=sqlStatements,includeKey=includeKey)
       if(nrow(outBit)>0){
         if(sheetInfo$dataScope=="project"){
           outBit$ProjectID = cPid
           outBit = merge(mergeInfo,outBit)
         }
         if(sheetInfo$dataScope=="scenario"){
           outBit$ScenarioID = cSid
           outBit = merge(mergeInfo,outBit)
         }
       }
       if(i==1){
         out=outBit
       }else{
         out=rbind(out,outBit)
       }
    }
  }
  if(!forceConsole){
    out = .datasheet(.ssimLibrary(cScn),name,project,scenario,optional,empty,lookupsAsFactors,sqlStatements,includeKey)
  }else{
    out=unique(out)
  }
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
#' @param x One or more SsimLibrary, Projects or Scenario objects. Or the path to a library on disk.
#' @param scenario One or more scenario objects, names or ids.
#' @param onlyIds If FALSE (default) result Scenario objects are returned. If TRUE (faster) result scenario ids are returned.
#' @param jobs The number of jobs to run. Passed to SyncroSim where multithreading is handled.
#' @return A named list of result Scenario objects or ids. The name is the parent scenario for each result.
#' @examples
#'
#' @export
setGeneric('run',function(x,scenario=NULL,onlyIds=F,jobs=1) standardGeneric('run'))
#Handles case where x is a path to an SyncroSim library on disk.
setMethod('run', signature(x="character"), function(x,scenario,onlyIds,jobs) {
  x = library(x)
  out = run(x,scenario,onlyIds,jobs)
  return(out)
})
#Handles case where x is a list of objects.
setMethod('run', signature(x="list"), function(x,scenario,onlyIds,jobs) {
  out=list()
  for(i in seq(length.out=length(x))){
    out[[i]]=run(x[[i]],scenario,onlyIds,jobs)
  }
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
#' @param sheet The name of a spatial datasheet. See subset(datasheets(myResultScenario),isSpatial)$name for options.
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
    out[[as.character(.id(cScn))]]=cOut
  }
  return(out)
})

#' Get or set a socket connection.
#'
#' @param x An ipAddress or BreakpointSession object. If NULL a default ip will be used.
#' @param port For new connections only - a port number.
# @export
setGeneric('connection',function(x,...) standardGeneric('connection'))
#' @describeIn connection Get a new connection.
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

