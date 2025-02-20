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

            # create local copies of scaling parameters
            mean_vals <- params@mean_vals
            sd_vals <- params@sd_vals
            min_vals <- params@min_vals
            max_vals <- params@max_vals

            # transform scaling parameters
            # todo: y is not scaled with this method
            columns <- intersect(names(params@mean_vals), names(params@mean_vals))
            for (col in names(columns)) {
              mean_vals[col] <- params@fun_transform[[col]](params@mean_vals[col])
              sd_vals[col]   <- params@fun_transform[[col]](params@sd_vals[col])
              min_vals[col]  <- params@fun_transform[[col]](params@min_vals[col])
              max_vals[col]  <- params@fun_transform[[col]](params@max_vals[col])
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
