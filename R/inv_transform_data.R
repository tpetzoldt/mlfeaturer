#' @title Inverse Transform Data
#'
#' @description Applies the inverse transformation to the data.
#'
#' @param object A `feature_data` object.
#' @param params A `feature_params` object with transformation and scaling parameters
#' @param funs A list of inverse data transformation functions.
#' @return A `feature_data` object or a `data.frame` with the inverse transformed data.
#' @param ... Additional arguments (currently not used).
#'
#' @export
setGeneric("inv_transform_data", function(object, params, funs, ...) standardGeneric("inv_transform_data"))


#' @describeIn inv_transform_data Method for inverse transforming data in a `feature_data` object.
setMethod("inv_transform_data", signature = c(object = "feature_data"),
          function(object, ...) {
            if (object@params@transformed) {
              df <- object@data
              funs <- object@params@fun_inverse
              #if (is.null(funs)) warning("Inverse transformations not available")
              for (col in intersect(names(df), names(funs))) {
                df <- df |>
                  mutate(across(all_of(col), .fns = funs[[col]]))
              }
              object@data <- df
              object@params@transformed <- FALSE
            } else {
              #warning("No inverse transformation. Object was not transformed.")
            }
            # mark inverse transformed object with NA to avoid further transformations
            object@params@transformed <- NA
            return(object)
          })


#' @describeIn inv_transform_data Method for transforming data in a `feature_data` object and user-defined functions (deprecated).
setMethod("inv_transform_data", signature = c(object = "feature_data", funs = "list", params = "missing"),
          function(object, funs, ...) {
            df <- object@data
            #if (is.null(funs)) warning("Inverse transformations not available")
            for (col in intersect(names(df), names(funs))) {
              df <- df |>
                mutate(across(all_of(col), .fns = funs[[col]]))
            }
            object@params@transformed <- TRUE
            object@data <- df
            return(object)
          })

#' @describeIn inv_transform_data Method for a `data frame`.
setMethod("inv_transform_data", signature = c(object = "data.frame", funs="missing", params="feature_params"),
          function(object, params, ...) {
            df <- object
            funs <- params@fun_inverse
            #if (is.null(funs)) warning("Inverse transformations not available")
            for (col in intersect(names(df), names(funs))) {
              df <- df |>
                mutate(across(all_of(col), .fns = funs[[col]]))
            }
            return(df)
          })
