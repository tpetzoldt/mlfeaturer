# Define S4 generics for get_-functions

#' @title Get Training Data (X)
#'
#' @description Extracts the training data (X) from the preprocessed data.
#'
#' @param object A `preproc_data` object.
#' @param type Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transfored and scaled data ("both") or
#'  original raw data ("none") will be returned.
#' @param as_matrix logical TRUE if the function should return matrix,
#'   or a data frame or tibble otherwise.
#' @param ... Additional arguments (currently not used).
#'
#' @return A data frame or matrix containing the training data (X).
#' @export
setGeneric("get_x_train", function(object, ...) standardGeneric("get_x_train"))

#' @title Get Test Data (X)
#'
#' @description Extracts the test data (X) from the preprocessed data.
#'
#' @param object A `preproc_data` object.
#' @param type Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transfored and scaled data ("both") or
#'  original raw data ("none") will be returned.
#' @param as_matrix  logical TRUE if the function should return matrix,
#'   or a data frame or tibble otherwise.
#' @param ... Additional arguments (currently not used).
#'
#' @return A data frame or matrix containing the test data (X).
#' @export
setGeneric("get_x_test", function(object, ...) standardGeneric("get_x_test"))

#' @title Get All Data (X)
#'
#' @description Extracts all the data (X) from the preprocessed data.
#'
#' @param object A `preproc_data` object.
#' @param type Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transfored and scaled data ("both") or
#'  original raw data ("none") will be returned.
#' @param as_matrix  logical TRUE if the function should return matrix,
#'   or a data frame or tibble otherwise.
#' @param ... Additional arguments (currently not used).
#'
#' @return A data frame or matrix containing all the data (X).
#' @export
setGeneric("get_x_all", function(object, ...) standardGeneric("get_x_all"))

#' @title Get Training Data (Y)
#'
#' @description Extracts the training data (Y) from the preprocessed data.
#'
#' @param object A `preproc_data` object.
#' @param type Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transfored and scaled data ("both") or
#'  original raw data ("none") will be returned.
#' @param as_matrix  logical TRUE if the function should return matrix,
#'   or a data frame or tibble otherwise.
#' @param ... Additional arguments (currently not used).
#'
#' @return A data frame or matrix containing the training data (Y).
#' @export
setGeneric("get_y_train", function(object, ...) standardGeneric("get_y_train"))

#' @title Get Test Data (Y)
#'
#' @description Extracts the test data (Y) from the preprocessed data.
#'
#' @param object A `preproc_data` object.
#' @param type Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transfored and scaled data ("both") or
#'  original raw data ("none") will be returned.
#' @param as_matrix  logical TRUE if the function should return matrix,
#'   or a data frame or tibble otherwise.
#' @param ... Additional arguments (currently not used).
#'
#' @return A data frame or matrix containing the test data (Y).
#' @export
setGeneric("get_y_test", function(object, ...) standardGeneric("get_y_test"))

#' @title Get All Data (Y)
#'
#' @description Extracts all the data (Y) from the preprocessed data.
#'
#' @param object A `preproc_data` object.
#' @param type Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transfored and scaled data ("both") or
#'  original raw data ("none") will be returned.
#' @param as_matrix  logical TRUE if the function should return matrix,
#'   or a data frame or tibble otherwise.
#' @param ... Additional arguments (currently not used).
#'
#' @return A data frame or matrix containing all the data (Y).
#' @export
setGeneric("get_y_all", function(object, ...) standardGeneric("get_y_all"))


#' @describeIn get_x_train Method for extracting training data (X).
setMethod("get_x_train", signature = "preproc_data",
          function(object, type = c("both", "scale", "transform", "none"), as_matrix = TRUE) {
            type = match.arg(type)
            params <- object@params
            df <- object@data
            cols_to_remove <- c(params@id_col, params@target_col, params@split_col)

            x_train <- df |>
              dplyr::filter(!.data[[params@split_col]]) |>
              select(-all_of(cols_to_remove)) |>
              (\(.) if (type %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (type %in% c("both", "scale")) scale_x(., params) else .)()

            if (as_matrix) {
              return(as.matrix(x_train))
            } else {
              return(x_train)
            }
          })

#' @describeIn get_x_test Method for extracting test data (X).
setMethod("get_x_test", signature = "preproc_data",
          function(object, type = c("both", "scale", "transform", "none"), as_matrix = TRUE) {
            type = match.arg(type)
            params <- object@params
            df <- object@data
            cols_to_remove <- c(params@id_col, params@target_col, params@split_col)
            x_test <- df |>
              dplyr::filter(.data[[params@split_col]]) |>
              select(-all_of(cols_to_remove))  |>
              (\(.) if (type %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (type %in% c("both", "scale")) scale_x(., params) else .)()

            if (as_matrix) {
              return(as.matrix(x_test))
            } else {
              return(x_test)
            }
          })

#' @describeIn get_x_all Method for extracting all data (X).
setMethod("get_x_all", signature = "preproc_data",
          function(object, type = c("both", "scale", "transform", "none"), as_matrix = TRUE) {
            type = match.arg(type)
            params <- object@params
            df <- object@data
            cols_to_remove <- c(params@id_col, params@target_col, params@split_col)
            x_all <- df |>
              select(-all_of(cols_to_remove)) |>
              (\(.) if (type %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (type %in% c("both", "scale")) scale_x(., params) else .)()

            if (as_matrix) {
              return(as.matrix(x_all))
            } else {
              return(x_all)
            }
          })

#' @describeIn get_y_train Method for extracting training data (Y).
setMethod("get_y_train", signature = "preproc_data",
          function(object, type = c("transform", "none", "scale", "both"), as_matrix = TRUE) {
            type = match.arg(type)
            #if (type %in% c("both", "scale")) warning("scaling not implemented for y")
            params <- object@params
            df <- object@data
            y_train <- df |>
              dplyr::filter(!.data[[params@split_col]]) |>
              select(all_of(params@target_col)) |>
              (\(.) if (type %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (type %in% c("both", "scale")) scale_x(., params) else .)()

            if (as_matrix) {
              return(as.matrix(y_train))
            } else {
              return(y_train)
            }
          })

#' @describeIn get_y_test Method for extracting test data (Y).
setMethod("get_y_test", signature = "preproc_data",
          function(object, type = c("transform", "none", "scale", "both"), as_matrix = TRUE) {
            type = match.arg(type)
            #if (type %in% c("both", "scale")) warning("scaling not implemented for y")
            params <- object@params
            df <- object@data
            y_test <- df |>
              dplyr::filter(.data[[params@split_col]]) |>
              select(all_of(params@target_col)) |>
              (\(.) if (type %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (type %in% c("both", "scale")) scale_x(., params) else .)()

            if (as_matrix) {
              return(as.matrix(y_test))
            } else {
              return(y_test)
            }
          })

#' @describeIn get_y_all Method for extracting all data (Y).
setMethod("get_y_all", signature = "preproc_data",
          function(object, type = c("transform", "none", "scale", "both"), as_matrix = TRUE) {
            type = match.arg(type)
            #if (type %in% c("both", "scale")) warning("scaling not implemented for y")
            params <- object@params
            df <- object@data
            y_all <- df |>
              select(all_of(params@target_col)) |>
              (\(.) if (type %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (type %in% c("both", "scale")) scale_x(., params) else .)()

            if (as_matrix) {
              return(as.matrix(y_all))
            } else {
              return(y_all)
            }
          })

