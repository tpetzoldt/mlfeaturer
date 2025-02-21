#' @title Transform Data
#'
#' @description Transforms the data using the provided functions.
#'
#' @param object A `preproc_data` object or a data frame.
#' @param params A `preproc_params` object with transformation and scaling parameters
#' @param funs A list of data transformation functions .
#' @param ... Additional arguments (currently not used).
#'
#' @return A `preproc_data` object or a `data.frame` with the transformed data.
#'
#' @include aaa_classes.R
#'
#' @export
setGeneric("transform_data", function(object, params, funs,...) standardGeneric("transform_data"))


#' @describeIn transform_data Method for transforming data in a `preproc_data` object.
setMethod("transform_data", signature = c(object = "preproc_data"),
          function(object, ...) {
            df <- object@data
            funs <- object@params@fun_transform
            for (col in intersect(names(df), names(funs))) {
              df <- df |>
                mutate(across(all_of(col), .fns = funs[[col]]))
            }
            object@params@transformed <- TRUE
            object@data <- df
            return(object)
          })

#' @describeIn transform_data Method for transforming data in a `preproc_data` object and user-defined functions (deprecated).
setMethod("transform_data", signature = c(object = "preproc_data", funs = "list", params = "missing"),
          function(object, funs, ...) {
            df <- object@data
            for (col in intersect(names(df), names(funs))) {
              df <- df |>
                mutate(across(all_of(col), .fns = funs[[col]]))
            }
            object@params@transformed <- TRUE
            object@data <- df
            return(object)
          })

#' @describeIn transform_data Method for a `data frame`.
setMethod("transform_data", signature = c(object = "data.frame", funs="missing", params="preproc_params"),
          function(object, params, ...) {
            df <- object
            funs <- params@fun_transform
            for (col in intersect(names(df), names(funs))) {
              df <- df |>
                mutate(across(all_of(col), .fns = funs[[col]]))
            }
            return(df)
          })

#' @describeIn transform_data Method for a `data frame`.
setMethod("transform_data", signature = c(object = "data.frame", funs="list", params="missing"),
          function(object, funs, ...) {
            df <- object
            for (col in intersect(names(df), names(funs))) {
              df <- df |>
                mutate(across(all_of(col), .fns = funs[[col]]))
            }
            return(df)
          })
