# script


require(metafor)
require(meta)
require(grid)
require(ggplot2)


folder <- "Death"
# folder <- "Severe"

data1 <- read.table(glue::glue("../{folder}/ModernaPfizerCOVID19.csv"),
                    header = TRUE,
                    sep = ",")

data2 <- read.table(glue::glue("../{folder}/ModernaPfizerVE.csv"),
                    header = TRUE,
                    sep = ",")

dat <- metafor::escalc(
  measure = "RR",
  ai = n1,
  bi = N1 - n1,
  ci = n2,
  di = N2 - n2,
  data = data1,
  slab = author)

dat2 <- rbind(dat, data2)

meta1 <- metafor::rma(yi, vi, data = dat2, method = "DL")

forest_plot(meta1)
forest_plot(meta1, save = TRUE)

forest_plot(meta1, altplot = TRUE)
forest_plot(meta1, altplot = TRUE, save = TRUE)
