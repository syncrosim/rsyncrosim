# Author: Josie Hughes
# Date : October 2016
# Version 0.1
# Licence GPL v3
#' @include generics.R
NULL
#' ExternalClient class
#'
#' @name ExternalClient-class
#' @rdname ExternalClient-class
#' @export ExternalClient
ExternalClient <- setClass("ExternalClient",contains="ServerController")
setMethod(f="initialize",signature="ExternalClient",
          definition=function(.Object,connect=T,...){
            if(connect){
              .Object@connection=connection(...)
            }
            return(.Object)
          })
externalClient <- function(connect=T,...) new("ExternalClient",connect,...)

