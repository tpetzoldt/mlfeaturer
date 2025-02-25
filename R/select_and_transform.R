select_and_transform <- function(df, params, prep, as_matrix = TRUE) {
  ret <-
    df |>
    (\(.) if (prep %in% c("both", "transform")) transform_data(., params) else .)() |>
    (\(.) if (prep %in% c("scale")) scale_data(., params) else .)() |>
    (\(.) if (prep %in% c("both")) scale_data(., params, transformed = TRUE) else .)()

  if (as_matrix) return(as.matrix(ret)) else return(ret)
}
