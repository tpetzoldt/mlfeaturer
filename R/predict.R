#' Model Predictions with `feature_data` Objects
#'
#' Makes predictions from a machine learning model with a `feature_data` object.
#'
#' @param object A `feature_data` object.
#' @param model A fitted model object.
#' @param subset Subset of the `preproc` data set.
#' @param prep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
#'  original raw data ("none") will be returned.
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
                   to_original_scale = FALSE, ...) {

                        subset <- match.arg(subset)
            xprep <- match.arg(xprep)
            yprep <- match.arg(yprep)
            x <- switch(subset,
                        all = get_x_all(object, prep = xprep),
                        train = get_x_train(object, prep = xprep),
                        test = get_x_test(object, prep = xprep)
            )

            # todo: create wrapper for compatibility between model types
            ret <- predict(model, x)

            if (to_original_scale) {

              params <- object@params
              ret <- as.data.frame(ret)
              colnames(ret) <- params@target_col

              ret <- ret |>
                (\(.) if (yprep %in% c("both")) inverse_scaling(., params, transformed = TRUE) else .)() |>
                (\(.) if (yprep %in% c("scale")) inverse_scaling(., params) else .)() |>
                (\(.) if (yprep %in% c("both", "transform")) inverse_transform(., params) else .)()
            }
            return(ret)
          })
