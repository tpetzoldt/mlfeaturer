#' Extract Model Residuals Using `feature_data` Objects
#'
#' Extract residuals from a machine learning model compared to a `feature_data` object.
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
#' @param to_original_scale Logical value to force re-transformation of
#'   predictions to the original scale.
#' @param as_matrix logical TRUE if the function should return matrix,
#'   or a data frame or tibble otherwise.
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
                   xprep = c("both", "scale", "transform", "none"),
                   yprep = c("both", "scale", "transform", "none"),
                   to_original_scale = FALSE,
                   as_matrix = TRUE,...) {

            subset <- match.arg(subset)
            xprep <- match.arg(xprep)
            yprep <- match.arg(yprep)

            x <- switch(subset,
                        all = get_x_all(object, prep = xprep),
                        train = get_x_train(object, prep = xprep),
                        test = get_x_test(object, prep = xprep)
            )
            y <- switch(subset,
                        all = get_y_all(object, prep = yprep),
                        train = get_y_train(object, prep = yprep),
                        test = get_y_test(object, prep = yprep)
            )
            y - predict(model, x, xprep = xprep, yprep = yprep,
                        to_original_scale = to_original_scale)
          })
