# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Delete module or modules
#'
#' Delete module or modules from this version of SyncroSim.
#' Note that removing a module can be difficult to undo.
#' To restore the module the user will need to provide a .ssimpkg file or reinstall SyncroSim.
#' Thus, deleteModule() requires confirmation from the user.
#'
#' @param name Character string or vector of these. A module or vector of modules to remove. See modules() for options.
#' @param session Session.
#' @return "saved" or error message.
#' @export
setGeneric('deleteModule',function(name,session=NULL) standardGeneric('deleteModule'))
#' @rdname deleteModule
setMethod('deleteModule', signature(session="missingOrNULLOrChar"), function(name,session) {
  session=.session(session)
  return(deleteModule(name,session))
})
#' @rdname deleteModule
setMethod('deleteModule', signature(session="Session"), function(name,session) {
  #name = "sample-basic-dotnet";session=session()
  installedModules=modules(session)
  retList = list()
  for(i in seq(length.out=length(name))){
    #i = 1
    cVal = name[i]
    if(!is.element(cVal,installedModules$name)){
      print(paste0("Module ",cVal," is not installed, so cannot be removed."))
      next
    }
    
    answer <- readline(prompt=paste0("To restore ",cVal," after removing it you will need to provide a .ssimpkg file or reinstall SyncroSim.\nDo you really want to remove the module? (y/n): "))
    if(answer=="y"){
      tt = command(args=list(removemodule=cVal),session,program="SyncroSim.ModuleManager.exe")
      
      installedModules = modules(session)
      if(is.element(cVal,installedModules$name)){
        stop(paste0('Error: failed to remove module ',cVal))
      }
      retList[[cVal]]=tt
    }else{
      retList[[cVal]]="skipped"
    }
  }
  return (retList)
})
