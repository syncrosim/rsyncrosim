#make first character of string lower case
camel <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}

#http://stackoverflow.com/questions/26083625/how-do-you-include-data-frame-output-inside-warnings-and-errors
#' @export
printAndCapture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}

