# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Modify the grouping of spatial layers.
#'
#' Modify the grouping of spatial output layers in a SyncroSim results scenario.
#'
#' @param ssimObject Result Scenario.
#' @param action Character. Options are: apply, remove, rebuild
#' @param grouping Character. Only used if action=apply. If NULL use datasheet(myLibrary,name="STime_Options"). Options are: Iteration,Timestep,All
#' @return "saved" or an error message from SyncroSim.
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
#' # Combine spatial outputs into multi-band rasters containing a layer 
#' #for each timestep and iteration.
#' multiband(myResultScenario,action="apply",grouping="All")
#'
#' # Remove multi-banding
#' multiband(myResultScenario,action="remove")
#'
#' @export
setGeneric('multiband',function(ssimObject,action,grouping=NULL) standardGeneric('multiband'))
#' @rdname multiband
setMethod('multiband', signature(ssimObject="Scenario"), function(ssimObject,action,grouping) {
  #x=myResult;action="rebuild";grouping=NULL
  if(is.na(parentId(ssimObject))){
    stop("Need a result Scenario.")
  }
  
  #command(c("help"),program="SyncroSim.MultiBand.exe")
  args = list(lib=.filepath(ssimObject),sid=.scenarioId(ssimObject))
  args[[action]]=NA
  if(action=="apply"){
    if(!is.null(grouping)){
      args$grp = grouping
    }
  }
  tt = command(args,.session(ssimObject),program="SyncroSim.MultiBand.exe")
  return(tt[1])
})

if(0){#Note lists of objects are only supported when there is a reason to need combined output
  setMethod('multiband', signature(ssimObject="list"), function(ssimObject,action,grouping) {
    #x=myResult;action="rebuild";grouping=NULL
    
    if(class(ssimObject[[1]])!="Scenario"){
      stop("Expecting a Scenario object or list of scenario objects.")
    }
    out=list()
    for(i in 1:length(ssimObject)){
      #i=1
      cScn = ssimObject[[i]]
      cOut = multiband(cScn,action,grouping)
      out[[as.character(.scenarioId(cScn))]]=cOut
    }
    return(out)
  })
}
