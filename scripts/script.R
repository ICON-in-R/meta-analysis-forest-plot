# script


require(metafor)
require(meta)
require(grid)
require(ggplot2)
library(dplyr)
library(glue)

# data1 <- read.table(glue::glue("../data/data1.csv"),
#                     header = TRUE,
#                     sep = ",")
# 
# data2 <- read.table(glue::glue("../data/data2.csv"),
#                     header = TRUE,
#                     sep = ",")

set.seed(123)
n_studies <- 30

# binary data
data1 <- data.frame(
  author = paste("Estudio", 1:n_studies),
  N1 = sample(50:200, n_studies, replace = TRUE),
  N2 = sample(50:200, n_studies, replace = TRUE)) |> 
  rowwise() |>
  mutate(
    n1 = sample(10:N1, 1, replace = TRUE),
    n2 = sample(10:N2, 1, replace = TRUE))

# calculate rr
dat <- metafor::escalc(
  measure = "RR",
  ai = n1,
  bi = N1 - n1,
  ci = n2,
  di = N2 - n2,
  data = data1,
  slab = author
)

# rr data
data2 <- data.frame(
  yi = rnorm(3, mean = 0, sd = 0.2),
  vi = rnorm(3, mean = 0.1, sd = 0.05),
  author = paste("Estudio Extra", 1:3)
)

dat2 <- bind_rows(dat, data2)

meta1 <- metafor::rma(yi, vi, data = dat2, method = "DL")

#########
# plots
#########

metafor::forest(meta1)

forest_plot(meta1)
forest_plot(meta1, save = TRUE)

forest_plot(meta1, altplot = TRUE)
forest_plot(meta1, altplot = TRUE, save = TRUE)
