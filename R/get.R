# Define S4 generics for get_-functions

#' @title Get Training Data (X)
#'
#' @description Extracts the training data (X) from the preprocessed data.
#'
#' @param object A `feature_data` object.
#' @param prep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
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
#' @param object A `feature_data` object.
#' @param prep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
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
#' @param object A `feature_data` object.
#' @param prep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
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
#' @param object A `feature_data` object.
#' @param prep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
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
#' @param object A `feature_data` object.
#' @param prep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
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
#' @param object A `feature_data` object.
#' @param prep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
#'  original raw data ("none") will be returned.
#' @param as_matrix  logical TRUE if the function should return matrix,
#'   or a data frame or tibble otherwise.
#' @param ... Additional arguments (currently not used).
#'
#' @return A data frame or matrix containing all the data (Y).
#' @export
setGeneric("get_y_all", function(object, ...) standardGeneric("get_y_all"))

#' @title Get All Data (X and Y)
#'
#' @description Access complete data set from the preprocessed data.
#'
#' @param object A `feature_data` object.
#' @param prep Character argument if transformed data ("transform"),
#'  scaled data ("scale" ), transformed and scaled data ("both") or
#'  original raw data ("none") will be returned.
#' @param as_matrix  logical TRUE if the function should return matrix,
#'   or a data frame or tibble otherwise.
#' @param ... Additional arguments (currently not used).
#'
#' @return A data frame or matrix containing all data, including `id_col` and `split_col`.
#' @export
setGeneric("get_data", function(object, ...) standardGeneric("get_data"))


#' @describeIn get_x_train Method for extracting training data (X).
setMethod("get_x_train", signature = "feature_data",
          function(object, prep = c("both", "scale", "transform", "none"), as_matrix = TRUE) {
            prep = match.arg(prep)
            params <- object@params
            df <- object@data
            cols_to_remove <- c(params@id_col, params@target_col, params@split_col)

            x_train <- df |>
              dplyr::filter(!.data[[params@split_col]]) |>
              select(-all_of(cols_to_remove)) |>
              (\(.) if (prep %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (prep %in% c("scale")) scale_data(., params) else .)() |>
              (\(.) if (prep %in% c("both")) scale_data(., params, transformed = TRUE) else .)()

            if (as_matrix) {
              return(as.matrix(x_train))
            } else {
              return(x_train)
            }
          })

#' @describeIn get_x_test Method for extracting test data (X).
setMethod("get_x_test", signature = "feature_data",
          function(object, prep = c("both", "scale", "transform", "none"), as_matrix = TRUE) {
            prep = match.arg(prep)
            params <- object@params
            df <- object@data
            cols_to_remove <- c(params@id_col, params@target_col, params@split_col)
            x_test <- df |>
              dplyr::filter(.data[[params@split_col]]) |>
              select(-all_of(cols_to_remove))  |>
              (\(.) if (prep %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (prep %in% c("scale")) scale_data(., params) else .)() |>
              (\(.) if (prep %in% c("both")) scale_data(., params, transformed = TRUE) else .)()

            if (as_matrix) {
              return(as.matrix(x_test))
            } else {
              return(x_test)
            }
          })

#' @describeIn get_x_all Method for extracting all data (X).
setMethod("get_x_all", signature = "feature_data",
          function(object, prep = c("both", "scale", "transform", "none"), as_matrix = TRUE) {
            prep = match.arg(prep)
            params <- object@params
            df <- object@data
            cols_to_remove <- c(params@id_col, params@target_col, params@split_col)
            x_all <- df |>
              select(-all_of(cols_to_remove)) |>
              (\(.) if (prep %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (prep %in% c("scale")) scale_data(., params) else .)() |>
              (\(.) if (prep %in% c("both")) scale_data(., params, transformed = TRUE) else .)()

            if (as_matrix) {
              return(as.matrix(x_all))
            } else {
              return(x_all)
            }
          })

#' @describeIn get_y_train Method for extracting training data (Y).
setMethod("get_y_train", signature = "feature_data",
          function(object, prep = c("both", "scale", "transform", "none"), as_matrix = TRUE) {

            prep = match.arg(prep)
            params <- object@params
            df <- object@data
            y_train <- df |>
              dplyr::filter(!.data[[params@split_col]]) |>
              select(all_of(params@target_col)) |>
              (\(.) if (prep %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (prep %in% c("scale")) scale_data(., params) else .)() |>
              (\(.) if (prep %in% c("both")) scale_data(., params, transformed = TRUE) else .)()

            if (as_matrix) {
              return(as.matrix(y_train))
            } else {
              return(y_train)
            }
          })

#' @describeIn get_y_test Method for extracting test data (Y).
setMethod("get_y_test", signature = "feature_data",
          function(object, prep = c("both", "scale", "transform", "none"), as_matrix = TRUE) {

            prep = match.arg(prep)
            params <- object@params
            df <- object@data
            y_test <- df |>
              dplyr::filter(.data[[params@split_col]]) |>
              select(all_of(params@target_col)) |>
              (\(.) if (prep %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (prep %in% c("scale")) scale_data(., params) else .)() |>
              (\(.) if (prep %in% c("both")) scale_data(., params, transformed = TRUE) else .)()

            if (as_matrix) {
              return(as.matrix(y_test))
            } else {
              return(y_test)
            }
          })

#' @describeIn get_y_all Method for extracting all data (Y).
setMethod("get_y_all", signature = "feature_data",
          function(object, prep = c("both", "scale", "transform", "none"), as_matrix = TRUE) {
            prep = match.arg(prep)
            #if (prep %in% c("both", "scale")) warning("scaling not implemented for y")
            params <- object@params
            df <- object@data
            y_all <- df |>
              select(all_of(params@target_col)) |>
              (\(.) if (prep %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (prep %in% c("scale")) scale_data(., params) else .)() |>
              (\(.) if (prep %in% c("both")) scale_data(., params, transformed = TRUE) else .)()

            if (as_matrix) {
              return(as.matrix(y_all))
            } else {
              return(y_all)
            }
          })


#' @describeIn get_data Method for extracting all data.
setMethod("get_data", signature = "feature_data",
          function(object, prep = c("both", "scale", "transform", "none"), as_matrix = TRUE) {
            prep = match.arg(prep)
            params <- object@params
            df <- object@data
            cols_to_remove <- c(params@id_col, params@split_col)
            xy_all <- df |>
              select(-all_of(cols_to_remove)) |>
              (\(.) if (prep %in% c("both", "transform")) transform_data(., params) else .)() |>
              (\(.) if (prep %in% c("scale")) scale_data(., params) else .)() |>
              (\(.) if (prep %in% c("both")) scale_data(., params, transformed = TRUE) else .)()

            # combine transformed/scaled columns with id, target and split
            xy_all <- bind_cols(xy_all, select(df, all_of(cols_to_remove))) |>
              select(names(df)) # restore original order

            if (as_matrix) {
              return(as.matrix(xy_all))
            } else {
              return(xy_all)
            }
          })
