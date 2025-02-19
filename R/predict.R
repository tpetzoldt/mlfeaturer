#' Model Predictions with `preproc_data` Objects
#'
#' Makes predictions from a machine learning model with a `preproc_data` object.
#'
#' @param object A `preproc_data` object.
#' @param model A fitted model object.
#' @param type Subset of the `preproc` data set.
#'
#' @return A matrix or vector with the predictions.
#'
#' @details Currently only certain Keras models can be used as fitted models.
#'
#'
#' @export
setMethod("predict", signature(object = "preproc_data"),
          function(object, model, type = c("all", "test", "train"), ...) {
            type <- match.arg(type)
            x <- switch(type,
              all = get_x_all(object),
              train = get_x_train(object),
              test = get_x_test(object)
            )
            predict(model, x)
          })
