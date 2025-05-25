library("mlfeaturer")
library("nnet")
library("dplyr")
library("ggplot2")

xy <- as.data.frame(matrix(rnorm(1e8, mean=100, sd=10), ncol=10))

colnames(xy) <- paste0("A", seq_len(ncol(xy)))

## scale only, no transform
system.time(
  mlf <-
    xy |>
    mutate(no = seq_len(n())) |>
    create_preprocessed_data(target_col = "A1",
                             id_col = "no",
                             scale_method = "zscore",
                             scale_option = "train",
                             fun_transform = list(A2 = \(x) sqrt(x)))
)

dummy <- get_x_all(mlf)

dummy <- get_x_test(mlf, prep="transform")
boxplot(dummy)
