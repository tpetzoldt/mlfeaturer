#' @title Scale Data
#'
#' @description Scales the data using the stored parameters.
#'
#' @param object A `preproc_data` object or a data frame.
#' @param params n `preproc_params` object with scaling parameters
#' @param ... Additional arguments (currently not used).
#'
#' @return A `preproc_data` object with the scaled data.
#' @export
setGeneric("scale_x", function(object, params, ...) standardGeneric("scale_x"))

#' @describeIn scale_x Method for scaling data in a `preproc_data` object.
setMethod("scale_x", signature = c(object = "preproc_data", params = "missing"),
          function(object, ...) {
            params <- object@params
            df <- object@data
            cols <- names(object@data)
            cols_to_remove <- c(params@id_col, params@target_col, params@split_col)

            scaled_df <- df |>
              select(-all_of(cols_to_remove)) |>
              mutate(across(everything(),
                            \(.x) {
                              if (params@scale_method == "zscore") {
                                scale(.x,
                                      params@mean_vals[cur_column()],
                                      params@sd_vals[cur_column()])
                              } else if (params@scale_method == "minmax") {
                                ml_norm(.x,
                                        params@min_vals[cur_column()],
                                        params@max_vals[cur_column()])
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

#' @describeIn scale_x Method for scaling data in a `preproc_data` object.
setMethod("scale_x", signature = c(object = "data.frame", params = "preproc_params"),
          function(object, params, ...) {
            #cat("data frame method \n")
            #print(str(params))
            #params <- object@params
            df <- object

            ## todo: make this more elegant avoiding redundant calculations
            # for (col in names(params@mean_vals)) {
            #   mean_vals <- transform(params@mean_vals[col], params@fun_transform[col])
            #   sd_vals   <- transform(params@sd_vals[col], params@fun_transform[col])
            #   min_vals  <- transform(params@min_vals[col], params@fun_transform[col])
            #   max_vals  <- transform(params@max_vals[col], params@fun_transform[col])
            # }

            # todo: rescaled parameters are not used
            scaled_df <- df |>
              mutate(across(everything(),
                            \(.x) {
                              if (params@scale_method == "zscore") {
                                scale(.x,
                                      params@mean_vals[cur_column()],
                                      params@sd_vals[cur_column()])
                              } else if (params@scale_method == "minmax") {
                                ml_norm(.x,
                                        params@min_vals[cur_column()],
                                        params@max_vals[cur_column()])
                              } else {
                                .x
                              }
                            }))

            return(scaled_df)
          })
