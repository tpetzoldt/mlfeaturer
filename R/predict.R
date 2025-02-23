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
            prep = c("both", "scale", "transform", "none"), ...) {
            subset <- match.arg(subset)
			prep <- match.arg(prep)
            x <- switch(subset,
              all = get_x_all(object, prep = prep),
              train = get_x_train(object, prep = prep),
              test = get_x_test(object, prep = prep)
            )
            predict(model, x)
          })
