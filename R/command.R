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
#' @return Output from the SyncroSim console.
#' @examples
#' # Use a default session to creat a new library
#' args = list(create=NULL,ssimLibrary=NULL,name=paste0(getwd(),"/temp.ssim",model="stsim:model-transformer")
#' output = command(args)
#' output
#' @export
command<-function(args,session=NULL,printCmd=F,program="/SyncroSim.Console.exe") {
  # args=myArgs;session=mySsim;printCmd=F
  # TO DO: check validity of args

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
    sysArgs=paste0("--",args)
  }
  if(printCmd){
    outCmd = gsub("\"","",paste(sysArgs,collapse=" "),fixed=T)
    print(outCmd)
  }
  if(silent(session)){
    out = suppressWarnings(system2(paste0(.filepath(session),program), args=sysArgs,stdout=TRUE))
  }else{
    out = system2(paste0(.filepath(session),program), args=sysArgs,stdout=TRUE)
  }
  if(identical(out,character(0))){
    out="Success!"
  }else{
    if(!is.null(attr(out,"status"))){
    }
  }
  return(out)
}

