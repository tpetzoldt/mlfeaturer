#' @title Create Preprocessed Data Object
#'
#' @description Constructs a `feature_data` object, calculates scaling parameters,
#'   and optionally applies initial data transformations.
#'
#' @param data A data frame containing the raw data.
#' @param id_col Character vector specifying the name of the ID column (optional).
#' @param target_col Character vector specifying the name of the target variable column.
#' @param split_col Character vector specifying the name of the column used for
#'   train/test split.
#' @param scale_option Character string specifying the scaling option.
#'   Must be one of "train", "test", or "all". Defaults to "train".
#' @param scale_method Character string specifying the scaling method.
#'   Must be one of "zscore" (standardization) or "minmax" (normalization).
#'   Defaults to "zscore".
#' @param fun_transform A named list of functions to apply to the data *before* scaling.
#'   The names of the list elements should correspond to the columns to transform.
#' @param fun_inverse A named list of functions representing the inverse
#'   transformations of `fun_transform`.  These are applied during the
#'   `inverse_transform` method.
#'
#' @return A `feature_data` object.
#'
#' @details
#' This function calculates scaling parameters (mean, standard deviation, min, max)
#' based on the specified `scale_option` and `scale_method`. It then creates a
#' `feature_data` object, storing the original data and the calculated parameters.
#' If `fun_transform` is provided, it applies the specified transformations to
#' the data before creating the object.
#'
#' If no `id_col` is given, an additional column `id` with row numers is created.
#'
#' If a `split_col` is missing, an random `split` column is created.
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
create_preprocessed_data <- function(data, target_col,
                                     id_col = NULL, split_col = NULL,
                                     scale_option = c("train", "test", "all"),
                                     scale_method = c("zscore", "minmax", "none"),
                                     fun_transform = NULL, fun_inverse = NULL) {

  scale_option <- match.arg(scale_option)
  scale_method <- match.arg(scale_method)

  ## dummy bugfix
  #if (is.null(fun_transform)) fun_transform <- list(x=\(x) x)

  if (is.null(id_col)) {
    if ("id" %in% names(data)) stop('Column "id" not set as "id_col".')
    data <- data |>
      mutate(id = seq_len(n()))
    id_col <- "id"
  }

  if (is.null(split_col)) {
    if ("split" %in% names(data)) stop('Column "split" not set as "split_col".')
    data <- data |>
      mutate(split = sample(c(TRUE, FALSE), n(), replace = TRUE))
    split_col <- "split"
  }


  ## define which columns to use
  cols_to_remove <- if (!is.null(id_col)) {
    c(id_col, split_col)
  } else {
    c(split_col)
  }

  ## calculate scaling parameters
  data_for_scaling <-
    switch(scale_option,
           train = data |>
             dplyr::filter(.data[[split_col]]) |>
             select(-all_of(cols_to_remove)),
           test =  data |>
             dplyr::filter(!.data[[split_col]]) |>
             select(-all_of(cols_to_remove)),
           all =  data   |>
              select(-all_of(cols_to_remove)),
    )

  ## calculate scaling parameters for original data
  mean_vals <- apply(data_for_scaling, 2, mean)
  sd_vals   <- apply(data_for_scaling, 2, sd)
  min_vals  <- apply(data_for_scaling, 2, min)
  max_vals  <- apply(data_for_scaling, 2, max)


  ## scale data when necessary
  if (!is.null((fun_transform)))
    data_for_scaling <- transform_data(data_for_scaling, funs = fun_transform)

  ## calculate scaling parameters for transformed data
  ## identical if no transformation was done
  t_mean_vals <- apply(data_for_scaling, 2, mean)
  t_sd_vals   <- apply(data_for_scaling, 2, sd)
  t_min_vals  <- apply(data_for_scaling, 2, min)
  t_max_vals  <- apply(data_for_scaling, 2, max)


  params <- new("feature_params",
                id_col = id_col, target_col = target_col, split_col = split_col,
                scale_option = scale_option, scale_method = scale_method,
                mean_vals = mean_vals, sd_vals = sd_vals,
                min_vals = min_vals, max_vals = max_vals,
                t_mean_vals = t_mean_vals, t_sd_vals = t_sd_vals,
                t_min_vals = t_min_vals, t_max_vals = t_max_vals,

                fun_transform = fun_transform, fun_inverse = fun_inverse,
                transformed = FALSE)

  return(new("feature_data", data = data, params = params))
}

