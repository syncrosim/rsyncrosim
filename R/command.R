# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' SyncroSim console command
#'
#' \code{command} issues a command to the SyncroSim console and returns the output.
#'
#' @param args A list of arguments to the SyncroSim console.
#' @param session A SyncroSim session object. If NULL, a default session will be used.
#' @param printCmd If T, the command string is printed.
#' @param silent If NULL (default) use session@silent. If T suppress warnings from console.
#' @param wait If TRUE (default) R will wait for the command to finish before proceeding.
#' @return Output from the SyncroSim console.
#' @examples
#' # Use a default session to creat a new library
#' args = list(create=NULL,ssimLibrary=NULL,name=paste0(getwd(),"/temp.ssim",model="stsim:model-transformer")
#' output = command(args)
#' output
#' @export
command<-function(args,session=NULL,printCmd=F,program="/SyncroSim.Console.exe",silent=NULL,wait=T) {
  # args=args;session=bugSession;printCmd=T;program="/SyncroSim.Console.exe"
  # TO DO: check validity of args
  if(!is.null(silent)){
    session@silent =silent
  }

  #if a syncrosim session is not provided, make one
  if(is.null(session)){
    session = .session()
  }

  if(class(args)=="list"){
    sysArgs = c()
    for(i in seq(length.out=length(args))){
      #i=1
      cArg = paste0("--",names(args)[i])
      sysArgs =c(sysArgs,cArg)
      if(is.null(args[[i]])){next}
      if(is.na(args[[i]])){next}
      if(args[[i]]==""){next}
      sysArgs[i] = paste0(sysArgs[i],'="',args[[i]],'"')
    }
  }else{
    fixPaths = grepl(" ",args)
    args[fixPaths] = gsub('=','="',args[fixPaths],fixed=T)
    args[fixPaths] = paste0(args[fixPaths],'"')
    sysArgs=paste0('--',args)
  }
  if(printCmd){
    outCmd = gsub("\"","",paste(sysArgs,collapse=" "),fixed=T)
    print(outCmd)
  }

  tempCmd = paste(c(paste0('\"\"',.filepath(session),program,'\"'),sysArgs,'\"'),collapse=" ")

  if(wait){
    if(silent(session)){
      out=suppressWarnings(shell(tempCmd,intern=T))
      #out = suppressWarnings(system2(paste0(.filepath(session),program), args=sysArgs,stdout=TRUE))
    }else{
      #out = system2(paste0(.filepath(session),program), args=sysArgs,stdout=TRUE)
      out = shell(tempCmd,intern=T)
    }
  }else{
    #Special case used for breakpoints
    out=suppressWarnings(shell(tempCmd,wait=F))
  }
  #if(grepl("0x80131515",out,fixed=T)){
    # handle a problem with permission to run
    #https://blogs.msdn.microsoft.com/brada/2009/12/11/visual-studio-project-sample-loading-error-assembly-could-not-be-loaded-and-will-be-ignored-could-not-load-file-or-assembly-or-one-of-its-dependencies-operation-is-not-supported-exception-from/
    #shell would probably also work
    #out = system2("cmd",args=c('/c',paste0('\"\"',.filepath(session),program,'\"'),sysArgs,'\"'))
  #}
  if(identical(out,character(0))){
    out="Success!"
  }else{
    if(!is.null(attr(out,"status"))){
    }
  }
  return(out)
}

