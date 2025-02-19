## small utilities that are not exported

ml_norm <- function(x, xmin, xmax) {
      (x - xmin) / (xmax - xmin)
}

invscale <- function(x, center, scale) {
  center + x * scale
}
