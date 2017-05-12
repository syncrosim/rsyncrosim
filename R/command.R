# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' SyncroSim console command
#'
#' \code{command} issues a command to the SyncroSim console and returns the output.
#'
#' @details 
#' Example args, and the resulting character string passed to the SyncroSim console:
#' \itemize{
#'    \item Character string e.g. "--create --help": "--create --help" 
#'    \item Named list or named vector e.g. list(name1=NULL,name2=value2): "--name1 --name2=value2"
#'    \item Unnamed list or unnamed vector e.g. c("create","help"): "--create --help"
#' }
#' @param args Character string, named list, named vector, unnamed list, or unnamed vector. Arguments for the SyncroSim console. See details.
#' @param session A SyncroSim session object. If NULL, a default session will be used.
#' @param program Character. The name of the target SyncroSim executable. Options include SyncroSim.Console.exe (default), SyncroSim.Server.exe, SyncroSim.ModuleManager.exe and SyncroSim.Multiband.exe.
#' @param wait Logical. If TRUE (default) R will wait for the command to finish before proceeding. Note that silent(session) is ignored if wait=F.
#' @return Output from the SyncroSim program.
#' @examples
#' # Use a default session to creat a new library in the current working directory.
#' args = list(create=NULL,library=NULL,name=paste0(getwd(),"/temp.ssim"),model="stsim:model-transformer")
#' output = command(args,session=session(printCmd=T))
#' output
#' 
#' #Three different ways to provide args to command
#' command(c("create","help"))
#' command("--create --help")
#' command(list(create=NULL,help=NULL))
#' @export
command<-function(args,session=NULL,program="SyncroSim.Console.exe",wait=T) {
  # args=args;session=session(printCmd=T,silent=F);program="SyncroSim.Console.exe";wait=T
  # TO DO: check validity of args
 
  #if a syncrosim session is not provided, make one
  if(is.null(session)){
    session = .session()
  }
  if((class(args)=="list")&is.null(names(args))){
    args = as.character(args)
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
  sysArgs
  if(printCmd(session)){
    outCmd = gsub("\"","",paste(sysArgs,collapse=" "),fixed=T)
    print(outCmd)
  }

  tempCmd = paste(c(paste0('\"\"',.filepath(session),"/",program,'\"'),sysArgs,'\"'),collapse=" ")

  #print(filepath(session))
  if(Sys.info()['sysname'][[1]]!="Windows"){
    warning("Handle this case. May need to add mono for Server. See session.py for detail")
  }
  if(wait){
    tryCatch(out<-shell(tempCmd,intern=T),warning=function(w){
      if(!silent(session)){
        print(paste0("SyncroSim Error: ",as.character(out)))
      }
    },error = function(e) stop(e))
    #errs = suppressWarnings(system2(paste0(.filepath(session),"/",program), args=sysArgs,stdout=TRUE,stderr=TRUE))
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
    out="saved"
  }
  return(out)
}

