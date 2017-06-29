# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Installed models
#'
#' Models installed with this version of SyncroSim
#'
#' @param ssimObject Session or SsimLibrary.
#' @return A dataframe of models (for Session) or named vector of character strings (for SsimLibrary)
#' @export
setGeneric('model',function(ssimObject=NULL) standardGeneric('model'))
#' @rdname model
setMethod('model', signature(ssimObject="missingOrNULL"), function(ssimObject) {
  ssimOject=session()
  tt=command(c("list","models","csv"),ssimObject)
  out=.dataframeFromSSim(tt,localNames=T)
  return(out)
})
#' @rdname model
setMethod('model', signature(ssimObject="Session"), function(ssimObject) {
  #x=session()
  tt=command(c("list","models","csv"),ssimObject)
  out=.dataframeFromSSim(tt,localNames=T)
  return(out)
})
#' @rdname model
setMethod('model', signature(ssimObject="SsimLibrary"), function(ssimObject) {
  #ssimObject=myLibrary
  oInf = info(ssimObject)
  property=NULL
  out =data.frame(name=subset(oInf,property=="Model Name:")$value)
  out$description = subset(oInf,property=="Model Description:")$value
  out$version = subset(oInf,property=="Model Current Version:")$value
  out$minVersion = subset(oInf,property=="Model Minimum Version:")$value
  return(out)
})
