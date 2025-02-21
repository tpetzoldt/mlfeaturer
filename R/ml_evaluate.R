#' Evaluates model performance across different data subsets.
#'
#' This function calculates various model evaluation metrics, including R-squared,
#' Mean Squared Error (MSE), Root Mean Squared Error (RMSE), and Mean Absolute
#' Error (MAE), for different subsets of data (all, train, and test).
#'
#' @param object An object of class `preproc_data` containing the data subsets.
#' @param model The fitted model to be evaluated.
#' @param xtype Character argument if transformed input data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
#'  original raw x data ("none") are used.
#' @param ytype Character argument if transformed target values ("transform") or
#'  original raw y-values ("none") are used.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A data frame containing the evaluation metrics for each data subset.
#'   Rows represent the metrics (R2, MSE, RMSE, MAE), and columns represent the
#'   data subsets (all, train, test).
#'
#' @details The `xtype` must be exactly the same that was used as input for the
#'   fitted model, while `ytype` may be "transform" (the scale used for model
#'   fitting) or "none", i.e. the original scale.
#'
#' @rdname evaluate
#' @export
setGeneric("ml_evaluate", function(object, model, ...) standardGeneric("ml_evaluate"))

#' @describeIn evaluate Method for estimating a set of model evaluation criteria
#' for a model and a `preproc_data` object.
setMethod("ml_evaluate", signature(object = "preproc_data"),
          function(object, model,
                   xtype = c("both", "scale", "transform", "none"),
                   ytype = c("transform", "none"), ...) {

            xtype <- match.arg(xtype)
            ytype <- match.arg(ytype)

            subsets <- c("all", "train", "test")

            results_list <- lapply(subsets, function(current_subset) {
              x <- switch(current_subset,
                          all = get_x_all(object),
                          train = get_x_train(object),
                          test = get_x_test(object)
              )
              y <- switch(current_subset,
                          all = get_y_all(object, ytype),
                          train = get_y_train(object, ytype),
                          test = get_y_test(object, ytype)
              )

              y_hat <- predict(model, x)
              residuals <- y - y_hat

              R2 <- 1 - var(residuals) / var(y)
              MSE <- mean(residuals^2)
              RMSE <- sqrt(MSE)
              MAE <- mean(abs(residuals))
              BIAS <- mean(y_hat - y) # Note the order: y_hat - y

              tibble(
                R2 = R2,
                MSE = MSE,
                RMSE = RMSE,
                MAE = MAE,
                BIAS =BIAS
              )
            })

            results_df <- t(do.call(rbind, results_list))
            colnames(results_df) <- subsets
            rownames(results_df) <- c("R2", "MSE", "RMSE", "MAE", "BIAS")
            results_df <- as_tibble(results_df, rownames = "metric") |> # Corrected: rownames = "metric"
              relocate(metric, .before = 1)

            return(results_df)
          })
