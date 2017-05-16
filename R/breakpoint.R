# Author: Josie Hughes
# Date : December 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
#' @include breakpointSession.R
NULL
# Breakpoint class
#
# @slot arguments Timesteps or iterations e.g. "1,2"
# @slot breakpointName Breakpoint name
# @slot name Name
# @slot transformerName 'stsim:core-transformer' or?
# @slot callback The function to apply. See STSimBreakpointsTutorial.R for details.
# @name Breakpoint-class
# @rdname Breakpoint-class
# @export Breakpoint
Breakpoint <- setClass("Breakpoint",representation(arguments="character",breakpointName="character",name="character",transformerName="character",callback="function"))
setMethod(f='initialize',signature="Breakpoint",
          definition=function(.Object,breakpointName,transformerName,arguments,callback,name="Main"){

            .Object@breakpointName = breakpointName
            .Object@transformerName = transformerName
            .Object@arguments = paste(arguments,collapse=",")
            .Object@callback = callback
            .Object@name = name
            return(.Object)
          })
# @export
breakpoint<-function(breakpointName,transformerName,arguments,callback,name="Main"){
  return(new("Breakpoint",breakpointName,transformerName,arguments,callback,name))
}
