## small utilities that are not exported

ml_norm <- function(x, xmin, xmax) {
      (x - xmin) / (xmax - xmin)
}

minmax <- function(x, xmin, xmax) {
  (x - xmin) / (xmax - xmin)
}


inv_minmax <- function(x, xmin, xmax) {
  x * (xmax - xmin) + xmin
}

inv_scale <- function(x, center, scale) {
  center + x * scale
}

inv_scale_y <- function(object, params, transformed = FALSE, ...) {
  df <- object

  if (transformed) {
    # use transformed scaling parameters
    mean_vals <- params@t_mean_vals
    sd_vals <- params@t_sd_vals
    min_vals <- params@t_min_vals
    max_vals <- params@t_max_vals
  } else {
    # use original scaling parameters
    mean_vals <- params@mean_vals
    sd_vals <- params@sd_vals
    min_vals <- params@min_vals
    max_vals <- params@max_vals
  }

  scaled_df <- df |>
    mutate(across(everything(),
                  \(.x) {
                    if (params@scale_method == "zscore") {
                      inv_scale(.x,
                            mean_vals[cur_column()],
                            sd_vals[cur_column()])
                    } else if (params@scale_method == "minmax") {
                      inv_minmax(.x,
                              min_vals[cur_column()],
                              max_vals[cur_column()])
                    } else {
                      .x
                    }
                  }))

  return(scaled_df)
}
