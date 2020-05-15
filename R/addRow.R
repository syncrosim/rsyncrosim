# Copyright (c) 2019 Apex Resource Management Solution Ltd. (ApexRMS). All rights reserved.
# GPL v.3 License
#' @include AAAClassDefinitions.R
NULL

#' Add row(s) to a dataframe.
#'
#' Adds row(s) to a dataframe.
#'
#' @details
#' Preserves the types and factor levels of the targetDataframe.
#' Fills missing values if possible using factor levels.
#' If value is a named vector or list, it will be converted to a single row dataframe.
#' If value is an unnamed vector or list, the number of elements should equal the number of columns in the targetDataframe; elements are assumed to be in same order as dataframe columns.
#'
#' @param targetDataframe Dataframe.
#' @param value Dataframe, character string vector, or list. Columns in value should be a subset of columns in targetDataframe.
#' 
#' @return 
#' A dataframe with new rows.
#' 
#' @export
setGeneric("addRow", function(targetDataframe, value) standardGeneric("addRow"))

#' @rdname addRow
setMethod("addRow",
  signature = "data.frame",
  definition = function(targetDataframe, value) {
    inNames <- names(value)
    if (class(value) == "character") {
      value <- as.data.frame(t(value), stringsAsFactors = F)
    }

    # if value is list
    if (class(value) == "list") {
      value <- as.data.frame(value, stringsAsFactors = F)

      if (nrow(value) != 1) {
        stop("Can't convert value to a single row data frame.")
      }
    }

    if (length(setdiff(names(value), names(targetDataframe))) > 0) {
      if (is.null(inNames)) {
        if (ncol(value) == ncol(targetDataframe)) {
          names(value) <- names(targetDataframe)
        } else {
          stop("The number of elements in value does not equal the number of columns in the targetDataframe. Provide names, or ensure the correct number of elements in value.")
        }
      } else {
        stop("Column names not recognized: ", paste(setdiff(names(value), names(targetDataframe))), collapse = ",")
      }
    }
    for (i in seq(length.out = ncol(value))) {
      cName <- names(value)[i]
      if (is.factor(targetDataframe[[cName]])) {
        notAllowed <- setdiff(value[[cName]], levels(targetDataframe[[cName]]))
        if (length(notAllowed) > 0) {
          stop("Invalid values for ", cName, " : ", paste(notAllowed, collapse = ","))
        }
      } else {
        if (is.factor(value[[cName]])) {
          value[[cName]] <- as.character(value[[cName]])
        }
        class(value[[cName]]) <- class(targetDataframe[[cName]])
      }
    }

    # Note - will not add row if that exact row already exists.
    out <- merge(targetDataframe, value, all = T)

    # Now fill in missing factor values if possible
    for (i in seq(length.out = ncol(out))) {
      cName <- names(out)[i]
      if (is.factor(targetDataframe[[cName]])) {
        if (length(levels(targetDataframe[[cName]])) == 1) {
          out[[cName]] <- levels(targetDataframe[[cName]])[1]
        }
        out[[cName]] <- factor(out[[cName]], levels = levels(targetDataframe[[cName]]))
      }
    }

    out <- subset(out, select = names(targetDataframe))
    return(out)
  }
)
