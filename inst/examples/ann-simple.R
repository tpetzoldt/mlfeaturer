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
                           scale_option = "train")


## use scaled data for both, x and y
set.seed(1234)
net <- nnet(get_x_train(dt4), get_y_train(dt4, prep="both"),
            size=10, maxit=1000, trace=FALSE)

plot(get_y_train(dt4, "both"), predict(net),
     pch = "+", col=as.factor(get_id_train(dt4)[,"species"]))

ml_evaluate(dt4, net, xtype="both", ytype="both")

# check coefficient of determination
cat("R^2 (train)=", 1 - var(residuals(net))/var(get_y_train(dt4, "both")), "\n")


# compare mlfeaturer results with default functions
cbind(predict(net), predict(dt4, net, subset="train"))
cbind(residuals(net), residuals(dt4, net, subset="train"))

y_pred <- predict(dt4, net)

pred <- tibble(y_pred = y_pred)
df <- get_data(dt4, prep="scale", as_matrix=FALSE)


df <- cbind(pred, df)

df |>
  ggplot(aes(light, growthrate)) + geom_point() +
  geom_line(aes(light, y_pred)) +
  facet_grid(species ~ temperature)

## todo: implement invscale method

## test code, generalize this ....
invscale <- function(x, center, scale) {
  center + x * scale
}

y_pred2 <- data.frame(y_pred = unname(invscale(predict(dt4, net),
                    dt4@params@min_vals["growthrate"],
                    dt4@params@max_vals["growthrate"])))

df2 <- get_data(dt4, prep="none", as_matrix=FALSE)

df2 <- cbind(y_pred2, df2)

df2 |>
  ggplot(aes(light, growthrate)) + geom_point() +
  geom_line(aes(light, y_pred)) +
  facet_grid(species ~ temperature)

