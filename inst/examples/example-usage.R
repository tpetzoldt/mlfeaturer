# Example Usage

df <- tibble(
  id = 1:10,
  x = runif(10),
  y = rnorm(10) + 5,
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

foo <- create_preprocessed_data(df, id_col = "id", target_col = "y", split_col = "split",
 fun_transform = transformations, fun_inverse = inverse_transformations)


df
foo@data


L <- as(foo, "preproc_data_list")

L <- as(foo, "list")

as.list(foo)


get_x_test(foo, type="scale")
get_x_test(foo, type="both") # default is both
get_x_test(foo) # default is both
get_x_test(foo, type="none")

get_x_train(foo)

get_y_test(foo)

get_y_all(foo)
get_y_all(foo, type="transform")

get_x_all(foo)
get_y_all(foo)


get_id_train(foo)

str(get_x_all(foo, as_data_frame=TRUE))


## set slots directly
#foo <- set_transformations(foo, transformations, inverse_transformations)
#foo <- transform_data(foo, transformations) # Apply transformations
#print(foo@data)

## todo: implement a method for base::transform
foo2 <- transform_data(foo)

## untransformed data don't allow inverse
foo3 <- inverse_transform(foo)

# transformed data can be re-transformed
foo3 <- inverse_transform(foo2)

foo3@data

## transformed slot is NA to indicate back-and-forth tranform
foo3@params@transformed

str(get_x_all(foo))

foo <- scale_x(foo) # Scale the data
print(foo@data)

foo2 <- inverse_transform(foo)
print(foo2@data)

#Access data subsets
print(get_x_train(foo))


foo@params@fun_transform
foo@params@fun_inverse


foo3 <- scale_data(foo2)

foo2@data
foo3@data
