#' Convert feature_data to a list
#'
#' Coerces a `feature_data` object to a `list` object.
#'
#' @param x A `feature_data` object.
#'
#' @return A `list` object.
#'
#' @export
setMethod("as.list", signature = "feature_data",
          function(x) {
            L <- as(x, "feature_data_list") # Convert to feature_data_list first
            # Now convert the feature_data_list object to a regular list
            list(
              x_train = L@x_train,
              x_test = L@x_test,
              x_all = L@x_all,
              y_train = L@y_train,
              y_test = L@y_test,
              y_all = L@y_all,
              x_train_scaled = L@x_train_scaled,
              x_test_scaled = L@x_test_scaled,
              x_all_scaled = L@x_all_scaled
            )
          })
