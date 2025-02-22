library("mlfeaturer")
library("dplyr")
library("ggplot2")

data(dauta4)

## scaling and transformation ==================================================
dt4 <-
  dauta4 |>
  mutate(species = as.numeric(species)) |>
  create_preprocessed_data(target_col = "growthrate",
                           scale_method = "minmax",
                           scale_option = "both",
                           fun_transform = list(
                             light = \(x) 0.1 * x,
                             temperature = \(x) log(x),
                             growthrate = \(x) x^2
                           )
  )

boxplot(tibble(none = get_y_all(dt4, type="none"),
               scale = get_y_all(dt4, type="scale"),
               transform = get_y_all(dt4, type="transform"),
               both = get_y_all(dt4, type="both")),
        main="target y: growthrate")

boxplot(tibble(none = get_x_all(dt4, type="none")[,"temperature"],
              scale = get_x_all(dt4, type="scale")[,"temperature"],
              transform = get_x_all(dt4, type="transform")[,"temperature"],
              both = get_x_all(dt4, type="both")[,"temperature"]),
        main="input x: temperature")

get_data(dt4, type="none") |>
  ggplot(aes(light, growthrate)) + geom_point() + facet_grid(species ~ temperature)

get_data(dt4, type="scale") |>
  ggplot(aes(light, growthrate)) + geom_point() + facet_grid(species ~ temperature)

get_data(dt4, type="transform") |>
  ggplot(aes(light, growthrate)) + geom_point() + facet_grid(species ~ temperature)

get_data(dt4, type="both") |>
  ggplot(aes(light, growthrate)) + geom_point() +  facet_grid(species ~ temperature)
