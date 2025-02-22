#' Nonlinear Coefficient of Determination Using `preproc_data` Objects
#'
#' Estimates the nonlinear R2 from a machine learning model compared to a
#'   `preproc_data` object.
#'
#' @param object A `preproc_data` object.
#' @param model A fitted model object.
#' @param subset Subset of the `preproc` data set.
#' @param prep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
#'  original raw data ("none") will be returned.
#' @param ... Additional arguments (currently not used).
#'
#' @return The coefficient of determination.
#'
#' @details Currently only certain Keras models can be used as fitted models.
#'
#'
#' @export
setGeneric("rsquared", function(object, ...) standardGeneric("rsquared"))

#' @describeIn rsquared Method for estimating the coefficient of determination of
#'   a model for a `preproc_data` object.
setMethod("rsquared", signature(object = "preproc_data"),
          function(object, model,
		    subset = c("all", "test", "train"),
		    prep = c("both", "scale", "transform", "none"), ...) {

			subset <- match.arg(subset)
            x <- switch(subset,
              all = get_x_all(object),
              train = get_x_train(object),
              test = get_x_test(object)
            )
            y <- switch(subset,
                        all = get_y_all(object),
                        train = get_y_train(object),
                        test = get_y_test(object)
            )
            residuals <- y - predict(model, x)
            as.vector(1 - var(residuals) / var(y))
          })
