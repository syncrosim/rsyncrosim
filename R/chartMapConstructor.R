# Testing out plotting construction code
# Below taken and modified from ggplot plot-construction.R

# Create function for adding together SyncroSim charting and mapping objects
"+.ssim" <- function(e1, e2) {
  if (missing(e2)) {
    cli::cli_abort(c(
      "Cannot use {.code +} with a single argument.",
      "i" = "Did you accidentally put {.code +} on a new line?"
    ))
  }
  
  # Get the name of what was passed in as e2, and pass along so that it
  # can be displayed in error messages
  e2name <- deparse(substitute(e2))
  
  if (is.ssimChart(e1)) add_ssimChart(e1, e2, e2name)
  else if (is.ssimMap(e1)) add_ssimMap(e1, e2, e2name) 
}

# Overload the "+"
#' @rdname ssim-add
#' @export
"%+%" <- `+.ssim`

add_ssimChart <- function(c, object, objectname) {
  if (is.null(object)) return(c)
  
  c <- ssimChart_add(object, c, objectname)
  c
}

ssimChart_add <- function(object, chart, object_name) {
  UseMethod("ssimChart_add")
}

ssimChart_add.y <- function(object, chart, object_name){
  chart$yVariable <- object
  chart
}

add_ssimMap <- function(m, object, object_name){
  m
}

