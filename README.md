# mlfeaturer

## Machine Learning: Feature Engineering for Regression Models

Package **mlfeaturer** aims to provide data manipulation support for a
subset of machine learning problems, currently regression problems
with multiple input variables $x$ and a single input variable and a
single input variable $y$. A typical initial task is to transform and
scale the variables, split them into training and test sets, and then
and, after model fitting, compare the model outputs with the original
data, with the test data, and with test data, and with independent new
data.

It fills a gap between a style of creating a large number of
intermediate data frames and variables, as found in some introductory
tutorials, and full-fledged packages like
[caret](https://topepo.github.io/caret/),
[recipes](https://recipes.tidymodels.org/) or
[mlr3](https://mlr3.mlr-org.com/).

It uses R's S4 object orientation, is intentionally simple and mostly
intended for our own tasks.

## Installation

```
remotes::install_.packages_github("https://github.com/tpetzoldt/mlfeaturer")
```

## Documentation

The documentation is found on [https://tpetzoldt.github.io/mlfeaturer/](https://tpetzoldt.github.io/mlfeaturer/).

## Status

embryonic, assembling just a few ideas
