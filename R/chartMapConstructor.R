#' Testing out plotting construction code
#' Below taken and modified from ggplot plot-construction.R
#'
#' Create function for adding together SyncroSim charting and mapping objects
#' @param e1 An object of class [ssimChart()].
#' @param e2 A chart or map component, as described below.
#' @export
#' @method + Chart
#' @rdname chart-add
"+.Chart" <- function(e1, e2) {
  if (missing(e2)) {
    cli::cli_abort(c(
      "Cannot use {.code +} with a single argument.",
      "i" = "Did you accidentally put {.code +} on a new line?"
    ))
  }
  
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))
  
  if (is.Chart(e1)) add_ssimChart(e1, e2, e2name)
  else if (is.Map(e2)) add_ssimMap(e1, e2, e2name) 
}

# Overload the "+"
#' @rdname chart-add
#' @export
`+.Chart` <- function(e1, e2){
  UseMethod("ssimChart_add")
}

#' Customize the chart
#'
#' This generic allows you to add your own methods for adding custom objects to
#' a chart with [+.Chart].
#'
#' @param object An object to add to the chart
#' @param plot The chart object to add `object` to
#' @param object_name The name of the object to add
#'
#' @return A modified chart object
#'
#' @keywords internal
#' @export
add_ssimChart <- function(c, object, objectname) {
  if (is.null(object)) return(c)
  
  c <- ssimChart_add(object, c, objectname)
  c
}

#' @export
as.Chart <- function(x){
  if(!inherits(x, "Chart")) class(x) <- c("Chart", class(x))
  x
}

#' @export
is.Chart <- function(x){
  inherits(x, "Chart")
}

#' @export
ssimChart_add <- function(object, chart, object_name) {
  UseMethod("ssimChart_add")
}


# Below we specify what happens when an object of a certain type is added
# to a chart object (e.g., legend object, format object, title object, etc.)

# Adds an object of type "Criteria" to the chart and returns a chart object
#' @export
ssimChart_add.Criteria <- function(object, chart, object_name){
  browser()
  chart$type <- "line"
  chart
}

#' @export
ssimChart_add.line_chart <- function(object, chart, object_name){
  chart$type <- "line"
  chart
}

#' @export
ssimChart_add.column_chart <- function(object, chart, object_name){
  chart$type <- "column"
  chart
}

#' @export
ssimChart_add.y <- function(object, chart, object_name){
  chart$yVariable <- object
  chart
}

add_ssimMap <- function(m, object, object_name){
  m
}

