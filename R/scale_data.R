#' @title Scale Data
#'
#' @description Scales the data using the stored parameters.
#'
#' @param object A `feature_data` object or a data frame.
#' @param params n `feature_params` object with scaling parameters
#' @param transformed `TRUE` to use parameters of transformed data.
#' @param ... Additional arguments (currently not used).
#'
#' @return A `feature_data` object with the scaled data.
#' @export
setGeneric("scale_data", function(object, params, ...) standardGeneric("scale_data"))

#' @describeIn scale_data Method for scaling data in a `feature_data` object.
setMethod("scale_data", signature = c(object = "feature_data", params = "missing"),
          function(object, ...) {
            params <- object@params
            df <- object@data
            cols <- names(object@data)
            #cols_to_remove <- c(params@id_col, params@target_col, params@split_col)
            cols_to_remove <- c(params@id_col, params@split_col)

            scaled_df <- df |>
              select(-all_of(cols_to_remove)) |>
              mutate(across(everything(),
                            \(.x) {
                              if (params@scale_method == "zscore") {
                                scale(.x,
                                      params@t_mean_vals[cur_column()],
                                      params@t_sd_vals[cur_column()])
                              } else if (params@scale_method == "minmax") {
                                ml_norm(.x,
                                        params@t_min_vals[cur_column()],
                                        params@t_max_vals[cur_column()])
                              } else {
                                .x
                              }
                            }))

            non_scaled_df <- df |> select(all_of(cols_to_remove))
            scaled_df <- bind_cols(non_scaled_df, scaled_df) |>
              select(cols)

            object@data <- scaled_df
            return(object)
            #return(scaled_df)
          })

#' @describeIn scale_data Method for scaling data in a `feature_data` object.
setMethod("scale_data", signature = c(object = "data.frame", params = "feature_params"),
          function(object, params, transformed = FALSE, ...) {
            #cat("data frame method \n")
            df <- object

            if (transformed) {
              mean_vals <- params@t_mean_vals
              sd_vals <- params@t_sd_vals
              min_vals <- params@t_min_vals
              max_vals <- params@t_max_vals
            } else {
              mean_vals <- params@mean_vals
              sd_vals <- params@sd_vals
              min_vals <- params@min_vals
              max_vals <- params@max_vals
            }
            # use transformed local scaling parameters
            scaled_df <- df |>
              mutate(across(everything(),
                            \(.x) {
                              if (params@scale_method == "zscore") {
                                scale(.x,
                                      mean_vals[cur_column()],
                                      sd_vals[cur_column()])
                              } else if (params@scale_method == "minmax") {
                                ml_norm(.x,
                                        min_vals[cur_column()],
                                        max_vals[cur_column()])
                              } else {
                                .x
                              }
                            }))

            return(scaled_df)
          })
