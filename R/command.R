# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' SyncroSim console command
#'
#' \code{command} issues a command to the SyncroSim console and returns the output.
#'
#' @param args A list of arguments to the SyncroSim console.
#' @param cSession A session object. If NULL, a default session will be used.
#' @param printCmd If T, the command string is printed.
#' @return Output from the SyncroSim console.
#' @examples
#' # Use a default session to creat a new library
#' args = list(create=NULL,library=NULL,name=paste0(getwd(),"/temp.ssim",model="stsim:model-transformer")
#' output = command(args)
#' output
#' @export
command<-function(args,cSession=NULL,printCmd=F) {
  # args=myArgs;cSession=mySsim;printCmd=F
  # TO DO: check validity of args

  #if a syncrosim session is not provided, make one
  if(is.null(cSession)){
    cSession = session()
  }

  sysArgs = c()
  for(i in seq(length(args))){
    #i=1
    cArg = paste0("--",names(args)[i])
    sysArgs =c(sysArgs,cArg)
    if(is.null(args[[i]])){next}
    if(is.na(args[[i]])){next}
    if(args[[i]]==""){next}
    sysArgs[i] = paste0(sysArgs[i],"=",args[[i]])
  }
  if(printCmd){
    print(paste(sysArgs,collapse=" "))
  }
  if(silent(cSession)){stderr=F}else{stderr=""}
  cOutput = system2(filepath(cSession), args=sysArgs,stdout=TRUE,stderr=stderr)
  if(identical(cOutput,character(0))){
    cOutput="Success!"
  }#else{
  #  if(!is.null(attr(cOutput,"status"))){
  #    if(attr(cOutput,"status")!=0){stop(cOutput)}
  #  }
  #}
  #TO DO: how to use status to check for failure?
  return(cOutput)
}

