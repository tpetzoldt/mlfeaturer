library("mlfeaturer")
library("nnet")
library("dplyr")
library("ggplot2")

data(dauta4)


## only scaling, no transformation
dt4 <-
  dauta4 |>
  mutate(species_code = as.numeric(species),
         no=1:n()) |>
  create_preprocessed_data(target_col = "growthrate",
                           id_col = c("species", "no"),
                           scale_method = "minmax",
                           scale_option = "train")


net <- nnet(get_x_train(dt4), get_y_train(dt4, prep="both"), size=10, maxit=1000, trace=FALSE)

plot(get_y_train(dt4, "both"), predict(net), pch = "+", col=as.factor(get_id_train(dt4)[,"species"]))

cat("R^2=", 1 - var(residuals(net))/var(get_y_train(dt4, "both")), "\n") # coefficient of determination

ml_evaluate(dt4, net) # todo: check xtype and ytype

cbind(predict(net), predict(dt4, net, subset="train"))

cbind(residuals(net), residuals(dt4, net, subset="train"))
