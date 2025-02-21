#' @title Create Preprocessed Data Object
#'
#' @description Constructs a `preproc_data` object, calculates scaling parameters,
#'   and optionally applies initial data transformations.
#'
#' @param data A data frame containing the raw data.
#' @param id_col Character vector specifying the name of the ID column (optional).
#' @param target_col Character vector specifying the name of the target variable column.
#' @param split_col Character vector specifying the name of the column used for
#'   train/test split.
#' @param scale_option Character string specifying the scaling option.
#'   Must be one of "train", "test", or "both". Defaults to "train".
#' @param scale_method Character string specifying the scaling method.
#'   Must be one of "zscore" (standardization) or "minmax" (normalization).
#'   Defaults to "zscore".
#' @param fun_transform A named list of functions to apply to the data *before* scaling.
#'   The names of the list elements should correspond to the columns to transform.
#' @param fun_inverse A named list of functions representing the inverse
#'   transformations of `fun_transform`.  These are applied during the
#'   `inverse_transform` method.
#'
#' @return A `preproc_data` object.
#'
#' @details
#' This function calculates scaling parameters (mean, standard deviation, min, max)
#' based on the specified `scale_option` and `scale_method`. It then creates a
#' `preproc_data` object, storing the original data and the calculated parameters.
#' If `fun_transform` is provided, it applies the specified transformations to
#' the data before creating the object.
#'
#' @examples
#' # Example usage (replace with your actual data and column names)
#' df <- data.frame(
#'   id = 1:10,
#'   x = runif(10),
#'   y = rnorm(10),
#'   z = 1:10,
#'   split = sample(c(TRUE, FALSE), 10, replace = TRUE)
#' )
#'
#' transformations <- list(
#'   x = log,
#'   y = \(y) sqrt(5 + y),
#'   z = \(z) z^2
#' )
#'
#' inverse_transformations <- list(
#'   x = exp,
#'   y = \(y) y^2 - 5,
#'   z = sqrt
#' )
#'
#' prep_data <- create_preprocessed_data(
#'   df, id_col = "id", target_col = "y", split_col = "split",
#'   fun_transform = transformations, fun_inverse = inverse_transformations)
#' print(prep_data)
#'
#' @export
create_preprocessed_data <- function(data, id_col = NULL, target_col, split_col,
                                     scale_option = c("train", "test", "both"),
                                     scale_method = c("zscore", "minmax"),
                                     fun_transform = NULL, fun_inverse = NULL) {

  scale_option <- match.arg(scale_option)
  scale_method <- match.arg(scale_method)

  ## define which columns to use
  cols_to_remove <- if (!is.null(id_col)) {
    c(id_col, split_col)
  } else {
    c(split_col)
  }

  ## split data
  train_data <- data |> dplyr::filter(.data[[split_col]])
  test_data <- data |> dplyr::filter(!.data[[split_col]])
  all_data <- data

  ## calculate scaling parameters
  data_for_scaling <-
    switch(scale_option,
           train = train_data |> select(-all_of(cols_to_remove)),
           test =  test_data  |> select(-all_of(cols_to_remove)),
           both =  all_data   |> select(-all_of(cols_to_remove))
    )

  mean_vals <- apply(data_for_scaling, 2, mean)
  sd_vals   <- apply(data_for_scaling, 2, sd)
  min_vals  <- apply(data_for_scaling, 2, min)
  max_vals  <- apply(data_for_scaling, 2, max)


  params <- new("preproc_params",
                id_col = id_col, target_col = target_col, split_col = split_col,
                scale_option = scale_option, scale_method = scale_method,
                mean_vals = mean_vals, sd_vals = sd_vals,
                min_vals = min_vals, max_vals = max_vals,
                fun_transform = fun_transform, fun_inverse = fun_inverse,
                transformed = FALSE)

  return(new("preproc_data", data = data, params = params))
}

