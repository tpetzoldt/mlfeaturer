#' Model Predictions with `feature_data` Objects
#'
#' Makes predictions from a machine learning model with a `feature_data` object.
#'
#' @param object A `feature_data` object.
#' @param model A fitted model object.
#' @param subset Subset of the `preproc` data set.
#' @param xprep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
#'  original raw data ("none") are used for x.
#' @param yprep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
#'  original raw data ("none") are used for y.
#' @param ... Additional arguments (currently not used).
#'
#' @return A matrix or vector with the predictions.
#'
#' @details Currently only certain Keras models can be used as fitted models.
#'
#'
#' @export
setMethod("predict", signature(object = "feature_data"),
          function(object, model,
                   subset = c("all", "test", "train"),
                   xprep = c("both", "scale", "transform", "none"),
                   yprep = c("both", "scale", "transform", "none"),
                   to_original_scale = FALSE,
                   as_matrix = TRUE, ...) {

            subset <- match.arg(subset)
            xprep <- match.arg(xprep)
            yprep <- match.arg(yprep)
            params <- object@params

            x <- switch(subset,
                        all =   get_x_all(object, prep = xprep),
                        train = get_x_train(object, prep = xprep),
                        test =  get_x_test(object, prep = xprep)
            )

            # todo: create wrapper for compatibility between model types
            ret <- predict(model, x) |>
              as.data.frame()
            colnames(ret) <- params@target_col

            if (to_original_scale) {
              ret <- ret |>
                (\(.) if (yprep %in% c("both")) inv_scale_data(., params, transformed = TRUE) else .)() |>
                (\(.) if (yprep %in% c("scale")) inv_scale_data(., params) else .)() |>
                (\(.) if (yprep %in% c("both", "transform")) inv_transform_data(., params) else .)()
            }

            if (as_matrix) {
              return(as.matrix(ret))
            } else {
              return(ret)
            }
          })
