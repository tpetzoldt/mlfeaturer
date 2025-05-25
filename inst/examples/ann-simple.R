library("mlfeaturer")
library("nnet")
library("dplyr")
library("ggplot2")

## phytoplankton growth rate data set
data(dauta4)


## convert factor with species to rows of dummy variables
spec_dummy <-
  model.matrix(~ species - 1, dauta4) |>
  as.data.frame()


dt4 <-
  dauta4 |>
  bind_cols(spec_dummy) |>
  mutate(no = seq_len(n())) |>
  create_preprocessed_data(target_col = "growthrate",
                           id_col = c("species", "no"),
                           scale_method = "minmax",
                           scale_option = "train")


## use scaled data for both, x and y
set.seed(1234)
net <- nnet(get_x_train(dt4, prep="both"), get_y_train(dt4, prep="both"),
            size=8, maxit=1000, trace=FALSE)

plot(get_y_train(dt4, "both"), predict(net),
     pch = "+", col=as.factor(get_id_train(dt4)[,"species"]))

ml_evaluate(dt4, net) # "both" is the default
ml_evaluate(dt4, net, xprep="both", yprep="both")
ml_evaluate(dt4, net, xprep="scale", yprep="scale") # same if no transformation

# check coefficient of determination manually
cat("R^2 (train)=", 1 - var(residuals(net))/var(get_y_train(dt4)), "\n")


## compare prediction and data in the transformed scale
y_pred <-
  predict(dt4, net, xprep="both", yprep="both") |>
  as.data.frame() |>
  # rename column to avoid duplication with original data
  rename(growthrate_pred = growthrate)

get_data(dt4, prep="both", as_matrix=FALSE) |>
  bind_cols(y_pred) |>
  ggplot(aes(light, growthrate)) + geom_point() +
  geom_line(aes(light, growthrate_pred)) +
  facet_grid(species ~ temperature)

## compare prediction and data in the original scale
y_pred_orig_scale <-
  predict(dt4, net,
          xprep="both", yprep="both",
          #subset="all",
          to_original_scale = TRUE) |>
  as.data.frame() |>
  rename(growthrate_pred = growthrate)

get_data(dt4, prep="none", as_matrix=FALSE) |>
  bind_cols(y_pred_orig_scale) |>
  ggplot(aes(light, growthrate)) + geom_point() +
  geom_line(aes(light, growthrate_pred)) +
  facet_grid(species ~ temperature)


plot(predict(dt4, net), residuals(dt4, net))

plot(predict(dt4, net, to_original_scale=TRUE), residuals(dt4, net))

