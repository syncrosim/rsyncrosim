#Internal wrappers for functions that are overwritten internally by arguments of the same name.
#Export for developmet and debugging.
#' @include generics.R
#' @include session.R
#' @include ssimLibrary.R
#' @include project.R
#' @include scenario.R
NULL
#' @export
.ssimLibrary=ssimLibrary
#' @export
.id=id
#' @export
.filepath=filepath
#' @export
.session=session
#' @export
.project = project
#' @export
.scenario = scenario
#' @export
.addons=addons


