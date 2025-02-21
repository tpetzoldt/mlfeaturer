#' @title Inverse Transform Data
#'
#' @description Applies the inverse transformation to the data.
#'
#' @param object A `preproc_data` object.
#' @param params A `preproc_params` object with transformation and scaling parameters
#' @param funs A list of inverse data transformation functions.
#' @return A `preproc_data` object or a `data.frame` with the inverse transformed data.
#' @param ... Additional arguments (currently not used).
#'
#' @export
setGeneric("inverse_transform", function(object, params, funs, ...) standardGeneric("inverse_transform"))


#' @describeIn inverse_transform Method for inverse transforming data in a `preproc_data` object.
setMethod("inverse_transform", signature = c(object = "preproc_data"),
          function(object, ...) {
            if (object@params@transformed) {
              df <- object@data
              funs <- object@params@fun_inverse
              if (is.null(funs)) warning("Inverse transformations not available")
              for (col in intersect(names(df), names(funs))) {
                df <- df |>
                  mutate(across(all_of(col), .fns = funs[[col]]))
              }
              object@data <- df
              object@params@transformed <- FALSE
            } else {
              warning("No inverse transformation. Object was not transformed.")
            }
            # mark inverse transformed object with NA to avoid further transformations
            object@params@transformed <- NA
            return(object)
          })


#' @describeIn inverse_transform Method for transforming data in a `preproc_data` object and user-defined functions (deprecated).
setMethod("inverse_transform", signature = c(object = "preproc_data", funs = "list", params = "missing"),
          function(object, funs, ...) {
            df <- object@data
            if (is.null(funs)) warning("Inverse transformations not available")
            for (col in intersect(names(df), names(funs))) {
              df <- df |>
                mutate(across(all_of(col), .fns = funs[[col]]))
            }
            object@params@transformed <- TRUE
            object@data <- df
            return(object)
          })

#' @describeIn inverse_transform Method for a `data frame`.
setMethod("inverse_transform", signature = c(object = "data.frame", funs="missing", params="preproc_params"),
          function(object, params, ...) {
            df <- object
            funs <- params@fun_inverse
            if (is.null(funs)) warning("Inverse transformations not available")
            for (col in intersect(names(df), names(funs))) {
              df <- df |>
                mutate(across(all_of(col), .fns = funs[[col]]))
            }
            return(df)
          })
