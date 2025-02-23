#' Extract Model Residuals Using `feature_data` Objects
#'
#' Extract residuals from a machine learning model compared to a `feature_data` object.
#'
#' @param object A `feature_data` object.
#' @param model A fitted model object.
#' @param subset Subset of the `preproc` data set.
#' @param prep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
#'  original raw data ("none") will be returned.
#' @param ... Additional arguments (currently not used).
#'
#' @return A matrix or vector with the residuals.
#'
#' @details Currently only certain Keras models can be used as fitted models.
#'
#'
#' @export
setMethod("residuals", signature(object = "feature_data"),
          function(object, model,
		    subset = c("all", "test", "train"),
			prep = c("both", "scale", "transform", "none"),	...) {

		    subset <- match.arg(subset)
			prep <- match.arg(prep)

			x <- switch(subset,
              all = get_x_all(object),
              train = get_x_train(object),
              test = get_x_test(object)
            )
            y <- switch(subset,
                        all = get_y_all(object, prep = prep),
                        train = get_y_train(object, prep = prep),
                        test = get_y_test(object, prep = prep)
            )
            y - predict(model, x)
          })
