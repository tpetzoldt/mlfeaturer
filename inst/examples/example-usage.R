# Example Usage

df <- data.frame(
  id = 1:10,
  x = runif(10),
  y = rnorm(10),
  z = 1:10,
  split = sample(c(TRUE, FALSE), 10, replace = TRUE)
)

transformations <- list(
  x = log,
  y = \(y) sqrt(5 + y),
  z = \(z) z^2
)

inverse_transformations <- list(
  x = exp,
  y = \(y) y^2 - 5,
  z = sqrt
)

prep_data <- create_preprocessed_data(df, id_col = "id", target_col = "y", split_col = "split",
                                      fun_transform = transformations, fun_inverse = inverse_transformations)


prep_data <- create_preprocessed_data(df, id_col = "id", target_col = "y", split_col = "split")



df
print(prep_data@data)


## set slots directly
prep_data <- set_transformations(prep_data, transformations, inverse_transformations)

prep_data <- transform_data(prep_data, transformations) # Apply transformations
print(prep_data@data)

prep_data2 <- inverse_transform(prep_data)
print(prep_data2@data)


str(get_x_all(prep_data, return_matrix=FALSE))

prep_data <- scale_data(prep_data) # Scale the data
print(prep_data@data)

prep_data2 <- inverse_transform(prep_data)
print(prep_data2@data)

#Access data subsets
print(get_x_train(prep_data))


prep_data@params@fun_transform
prep_data@params@fun_inverse

prep_data2 <- inverse_transform(prep_data) # error
prep_data2@data

