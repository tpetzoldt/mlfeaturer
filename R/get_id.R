#' @title Get ID Columns for all data
#'
#' @description Extracts ID columns (ID) from the preprocessed data.
#'
#' @param object A `preproc_data` object.
#' @param as_matrix logical TRUE if the function should return matrix,
#'   or a data frame or tibble otherwise.
#' @param ... Additional arguments (currently not used).
#'
#' @return A data frame or matrix containing the ID column(s).
#' @export
setGeneric("get_id_all", function(object, ...) standardGeneric("get_id_all"))

#' @title Get ID Columns for test data
#'
#' @description Extracts ID columns (ID) from the preprocessed data.
#'
#' @param object A `preproc_data` object.
#' @param as_matrix logical TRUE if the function should return matrix,
#'   or a data  frame or tibble otherwise.
#' @param ... Additional arguments (currently not used).
#'
#' @return A data frame or matrix containing the ID column(s).
#' @export
setGeneric("get_id_test", function(object, ...) standardGeneric("get_id_test"))

#' @title Get ID Columns for training data
#'
#' @description Extracts ID columns (ID) from the preprocessed data.
#'
#' @param object A `preproc_data` object.
#' @param as_matrix logical TRUE if the function should return matrix,
#'   or a data frame or tibble otherwise.
#' @param ... Additional arguments (currently not used).
#'
#' @return A data frame or matrix containing the ID column(s).
#' @export
setGeneric("get_id_train", function(object, ...) standardGeneric("get_id_train"))

#' @describeIn get_id_all Method extracting id columns for all data
setMethod("get_id_all", signature = "preproc_data",
          function(object, as_matrix = TRUE) {
            params <- object@params
            df <- object@data |>
              select(params@id_col)

            if (as_matrix) {
              return(as.matrix(df))
            } else {
              return(df)
            }
          })

#' @describeIn get_id_train Method for extracting id columns for the training data
setMethod("get_id_train", signature = "preproc_data",
          function(object, as_matrix = TRUE) {
            params <- object@params
            df <- object@data |>
              dplyr::filter(.data[[params@split_col]]) |>
              select(params@id_col)

            if (as_matrix) {
              return(as.matrix(df))
            } else {
              return(df)
            }
          })

#' @describeIn get_id_test Method for extracting id columns for the test data
setMethod("get_id_test", signature = "preproc_data",
          function(object, as_matrix = TRUE) {
            params <- object@params
            df <- object@data |>
              dplyr::filter(!.data[[params@split_col]]) |>
              select(params@id_col)

            if (as_matrix) {
              return(as.matrix(df))
            } else {
              return(df)
            }
          })
