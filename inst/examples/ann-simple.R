library("mlfeaturer")
library("nnet")
library("dplyr")
library("ggplot2")

## phytoplankton growth rate data set
data(dauta4)

## scale data, no transformation
dt4 <-
  dauta4 |>
  mutate(species_code = as.numeric(species),
         no=1:n()) |>
  create_preprocessed_data(target_col = "growthrate",
                           id_col = c("species", "no"),
                           scale_method = "minmax",
                           scale_option = "all")


## use scaled data for both, x and y
set.seed(1234)
net <- nnet(get_x_train(dt4, prep="both"), get_y_train(dt4, prep="both"),
            size=10, maxit=1000, trace=FALSE)

plot(get_y_train(dt4, "both"), predict(net),
     pch = "+", col=as.factor(get_id_train(dt4)[,"species"]))

ml_evaluate(dt4, net, xtype="both", ytype="both")

# check coefficient of determination
cat("R^2 (train)=", 1 - var(residuals(net))/var(get_y_train(dt4, "both")), "\n")


# compare mlfeaturer results with default functions
#cbind(predict(net), predict(dt4, net, subset="train"))
#cbind(residuals(net), residuals(dt4, net, subset="train"))

y_pred <- predict(dt4, net) |>
  as.data.frame() |>
  rename(yy = growthrate)

df <- get_data(dt4, prep="scale", as_matrix=FALSE) |>
  bind_cols(y_pred)



df |>
  ggplot(aes(light, growthrate)) + geom_point() +
  geom_line(aes(light, yy)) +
  facet_grid(species ~ temperature)


y_pred_orig_scale <- mlfeaturer:::inv_minmax(y_pred,
                                            dt4@params@min_vals["growthrate"],
                                            dt4@params@max_vals["growthrate"])


tmp <- predict(dt4, net, xprep="both", yprep="both", subset="all")


#y_pred_orig_scale <- predict(dt4, net,
#                             prep = "scale", subset = "all",
#                             to_original_scale = TRUE) |>
#  rename(yyy = growthrate)

df2 <- get_data(dt4, prep="none", as_matrix=FALSE) |>
  bind_cols(y_pred_orig_scale)


df2 |>
  ggplot(aes(light, growthrate)) + geom_point() +
  geom_line(aes(light, yy)) +
  facet_grid(species ~ temperature)


