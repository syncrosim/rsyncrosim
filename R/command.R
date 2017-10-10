# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' SyncroSim console command
#'
#' Issues a command to the SyncroSim console and returns the output.
#'
#' @details 
#' Example args, and the resulting character string passed to the SyncroSim console:
#' \itemize{
#'    \item Character string e.g. "--create --help": "--create --help" 
#'    \item Named list or named vector e.g. list(name1=NULL,name2=value2): "--name1 --name2=value2"
#'    \item Unnamed list or unnamed vector e.g. c("create","help"): "--create --help"
#' }
#' @param args Character string, named list, named vector, unnamed list, or unnamed vector. Arguments for the SyncroSim console. See details.
#' @param session Session. If NULL, a default session will be used.
#' @param program Character. The name of the target SyncroSim executable. Options include SyncroSim.Console.exe (default), SyncroSim.Server.exe, SyncroSim.ModuleManager.exe and SyncroSim.Multiband.exe.
#' @param wait Logical. If TRUE (default) R will wait for the command to finish before proceeding. Note that silent(session) is ignored if wait=F.
#' @return Output from the SyncroSim program.
#' @examples
#' # Use a default session to creat a new library in the current working directory.
#' args = list(create=NULL,library=NULL,name=paste0(getwd(),"/temp.ssim"),
#'   model="stsim")
#' output = command(args,session=session(printCmd=T))
#' output
#' 
#' #Three different ways to provide args to command
#' command(c("create","help"))
#' command("--create --help")
#' command(list(create=NULL,help=NULL))
#' @export
command<-function(args,session=NULL,program="SyncroSim.Console.exe",wait=T) {

  #if a syncrosim session is not provided, make one
  if(is.null(session)){
    session = .session()
  }
  if((class(session)=="character")&&(session==SyncroSimNotFound(warn=F))){return(SyncroSimNotFound())}
  
  if((class(args)=="list")&is.null(names(args))){
    args = as.character(args)
  }
  
  if(class(args)=="list"){
    #catch invalid library paths - note this only works for args with names
    if(is.element("lib",names(args))){
      if(!file.exists(args$lib)){
        stop(paste0("Library does not exist on disk: ",args$lib))
      }
    }
    sysArgs = c()
    for(i in seq(length.out=length(args))){
      cArg = paste0("--",names(args)[i])
      sysArgs =c(sysArgs,cArg)
      if(is.null(args[[i]])){next}
      if(is.na(args[[i]])){next}
      if(args[[i]]==""){next}
      sysArgs[i] = paste0(sysArgs[i],'="',args[[i]],'"')
    }
  }else{
    args=gsub(" --","---",args,fixed=T)
    fixPaths = grepl(" ",args)
    args[fixPaths] = gsub('=','="',args[fixPaths],fixed=T)
    args[fixPaths] = paste0(args[fixPaths],'"')
    args = gsub("---"," --",args,fixed=T)
    
    if(sum(grepl("--",args,fixed=T))==0){
      args=paste0('--',args)
    }
    sysArgs=args
  }
  if(printCmd(session)){
    outCmd = gsub("\"","",paste(sysArgs,collapse=" "),fixed=T)
    print(outCmd)
  }

  tempCmd = NULL
  progName = paste0('\"', .filepath(session), "/", program, '\"')

  if (.Platform$OS.type == "windows") {
    tempCmd = paste(c(progName, sysArgs), collapse = " ")
  } else {
    tempCmd = paste(c("mono", progName, sysArgs), collapse = " ")
  }

  if(wait){
    out=suppressWarnings(system(tempCmd,intern=T))
    if(!silent(session)){
      if(is.element("status",names(attributes(out)))){
        if(attributes(out)$status>0){
          print(paste0("SyncroSim error: ",out[1]))
        }
      }
    }else{
      if(is.element("status",names(attributes(out)))){
        if(attributes(out)$status>0){
          out=out[1]
        }
      }
    }
  }else{
    #Special case used for breakpoints
    out=suppressWarnings(system(tempCmd,wait=F))
  }

  if(identical(out,character(0))){
    out="saved"
  }
  return(out)
}

