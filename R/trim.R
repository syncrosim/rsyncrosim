# Borrowed from the raster package
# Author: Robert J. Hijmans
# Date : December 2009
# Version 1.0
# Licence GPL v3

if (!isGeneric("trim")) {
  setGeneric("trim", function(x, ...)
    standardGeneric("trim"))
}


setMethod('trim', signature(x='character'),
          function(x, ...) {
            gsub("^\\s+|\\s+$", "", x)
          }
)

setMethod('trim', signature(x='data.frame'),
          function(x, ...) {
            for (i in 1:ncol(x)) {
              if (class(x[,i]) == 'character') {
                x[,i] <- trim(x[,i])
              } else if (class(x[,i]) == 'factor') {
                x[,i] <- as.factor(trim(as.character(x[,i])))
              }
            }
            return(x)
          }
)

setMethod('trim', signature(x='matrix'),
          function(x, ...) {
            if (is.character(x)) {
              x[] = trim(as.vector(x))
            } else {
              rows <- rowSums(is.na(x))
              cols <- colSums(is.na(x))
              rows <- which(rows != ncol(x))
              cols <- which(cols != nrow(x))
              if (length(rows)==0) {
                x <- matrix(ncol=0, nrow=0)
              } else {
                x <- x[min(rows):max(rows), min(cols):max(cols), drop=FALSE]
              }
            }
            return(x)
          }
)
