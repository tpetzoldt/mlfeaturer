#' Extract Model Residuals Using `preproc_data` Objects
#'
#' Extract residuals from a machine learning model compared to a `preproc_data` object.
#'
#' @param object A `preproc_data` object.
#' @param model A fitted model object.
#' @param type Subset of the `preproc` data set.
#'
#' @return A matrix or vector with the residuals.
#'
#' @details Currently only certain Keras models can be used as fitted models.
#'
#'
#' @export
setMethod("residuals", signature(object = "preproc_data"),
          function(object, model, type = c("all", "test", "train"), ...) {
            type <- match.arg(type)
            x <- switch(type,
              all = get_x_all(object),
              train = get_x_train(object),
              test = get_x_test(object)
            )
            y <- switch(type,
                        all = get_y_all(object),
                        train = get_y_train(object),
                        test = get_y_test(object)
            )
            y - predict(model, x)
          })
