# Copyright (c) 2017 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Get printCmd of a Session.
#'
#' Get printCmd setting of a Session object.
#'
#' @param session Session or character. A Session object or path to a session. If NULL, the default session will be used.
#' @return logical.
#' @export
setGeneric('printCmd',function(session=NULL) standardGeneric('printCmd'))
#' @rdname printCmd
setMethod('printCmd', signature(session="Session"), function(session) session@printCmd)
#' @rdname printCmd
setMethod('printCmd', signature(session="missingOrNULLOrChar"), function(session) {
  if(class(session)=="character"){
    session = .session(session)
  }else{
    session=.session()
  }
  return(printCmd(session))
})
