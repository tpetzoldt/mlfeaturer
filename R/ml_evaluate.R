#' Evaluates model performance across different data subsets.
#'
#' This function calculates various model evaluation metrics, including R-squared,
#' Mean Squared Error (MSE), Root Mean Squared Error (RMSE), and Mean Absolute
#' Error (MAE), for different subsets of data (all, train, and test).
#'
#' @param object An object of class `feature_data` containing the data subsets.
#' @param model The fitted model to be evaluated.
#' @param xprep Character argument if transformed input data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
#'  original raw x data ("none") are used.
#' @param yprep Character argument if transformed target values ("transform") or
#'  original raw y-values ("none") are used.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A data frame containing the evaluation metrics for each data subset.
#'   Rows represent the metrics (R2, MSE, RMSE, MAE), and columns represent the
#'   data subsets (all, train, test).
#'
#' @details The parameters `xprep` and `yprep` must be exactly the same
#'   that were used as input and output for the fitted model,
#'   Rescaling of `yprep` will be implemented in future versions of the package.
#'
#' @rdname evaluate
#' @export
setGeneric("ml_evaluate", function(object, model, ...) standardGeneric("ml_evaluate"))

#' @describeIn evaluate Method for estimating a set of model evaluation criteria
#' for a model and a `feature_data` object.
setMethod("ml_evaluate", signature(object = "feature_data"),
          function(object, model,
                   xprep = c("both", "scale", "transform", "none"),
                   yprep = c("both", "scale", "transform", "none"), ...) {

            xprep <- match.arg(xprep)
            yprep <- match.arg(yprep)

            calc_metrics <- function(x, y) {
              y_hat <- predict(model, x)

              ## todo: implement inverse transformation
              residuals <- y - y_hat

              R2   <- 1 - var(residuals) / var(y)
              MSE  <- mean(residuals^2)
              RMSE <- sqrt(MSE)
              MAE  <- mean(abs(residuals))
              BIAS <- mean(y_hat - y) # Note the order: y_hat - y

              c(R2 = R2, MSE = MSE, RMSE = RMSE, MAE = MAE, BIAS = BIAS)
            }

            metrics <- c("R2", "MSE", "RMSE", "MAE", "BIAS")
            m_all   <- calc_metrics(get_x_all(object, xprep),
                                    get_y_all(object, yprep))
            m_train <- calc_metrics(get_x_train(object, xprep),
                                    get_y_train(object, yprep))
            m_test  <- calc_metrics(get_x_test(object, xprep),
                                    get_y_test(object, yprep))

            results <-
              cbind(metrics = metrics,
                    train = m_train, test = m_test, all = m_all) |>
              as_tibble()

            return(results)
          })
