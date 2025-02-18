#' @title Convert preproc_data to preproc_data_list
#'
#' @description Coerces a `preproc_data` object to a `preproc_data_list` object.
#'
#' @param from A `preproc_data` object.
#'
#' @return A `preproc_data_list` object.
#' @name as.list
#' @export
setAs("preproc_data", "preproc_data_list",
      function(from) {
        new("preproc_data_list",
            x_train = get_x_train(from),
            x_test = get_x_test(from),
            x_all = get_x_all(from),
            y_train = get_y_train(from),
            y_test = get_y_test(from),
            y_all = get_y_all(from)#,
            #x_train_scaled = get_x_train_scaled(from),
            #x_test_scaled = get_x_test_scaled(from),
            #x_all_scaled = get_x_all_scaled(from)
            # ... other data subsets
        )
      })

#' @title Convert preproc_data to list
#'
#' @description Coerces a `preproc_data` object to a standard R `list` object.
#'
#' @param from A `preproc_data` object.
#'
#' @return A `list` object.
#' @name as.list
#' @export
setAs("preproc_data", "list", # New coercion method
      function(from) {
        L <- as(from, "preproc_data_list") # Convert to preproc_data_list first
        # Now convert the preproc_data_list object to a regular list
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


#' @describeIn as.list Method for coercion of a `preproc_data` object to a list.
setMethod("as.list", signature = "preproc_data",
          function(x) {
            L <- as(x, "preproc_data_list") # Convert to preproc_data_list first
            # Now convert the preproc_data_list object to a regular list
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
