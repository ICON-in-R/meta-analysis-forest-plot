# forest plot script

require(metafor)
require(meta)
require(grid)
require(ggplot2)


data_raw <- read.table("ModernaPfizerCOVID19_death1.csv",
# data_raw <- read.table("ModernaPfizerCOVID19_death.csv",
                       header = TRUE,
                       sep = ",")
data1 <- data_raw
# data1 <- data_raw[-3, ]

#use escalc function to estimate log RR (yi) with corresponding variance (vi) which will then be used in the meta-analysis
dat <- escalc(
  measure = "RR",
  ai = n1,
  bi = N1 - n1,
  ci = n2,
  di = N2 - n2,
  data = data1,
  slab = author
)

#run meta-analysis with rma function using log RR (yi) and corresponding variance vi as estimated by escalc function
meta1 <- rma(yi, vi, data = dat, method = "DL")

#create label for forest plot
lab <- paste(
  "\n",
  "Total events:",
  paste(sum(dat$n1)),
  "(Spikevax), ",
  paste(sum(dat$n2)),
  "(Comirnaty) ",
  "\n",
  "Heterogeneity: Chi²=",
  paste(round(meta1$QE, 2)),
  ", df=",
  paste(meta1$k - 1),
  paste("(P= ", round(meta1$QEp, 2), "),", sep = ""),
  "I²=",
  paste(
    paste(round(meta1$I2, 1), "%", sep = ""),
    "\n",
    "Test for overall effect: Z=",
    paste(paste(round(meta1$zval, 2), sep = ""), paste("(P= ", round(meta1$pval, 4), "),",
                                                       sep = ""))
  )
)

#create label to show number of events and sample size
ev1 <- data.frame(paste(paste(data1$n1), "/", paste(data1$N1)))
ev2 <- data.frame(paste(paste(data1$n2), "/", paste(data1$N2)))

#combine labels for both arms with label on immunocompromising condition
ev <- cbind.data.frame(data.frame(c(ev1, ev2)), data1$type) |> `colnames<-`(c("a","b","c"))

dev.new(width=12, height=9, noRStudioGD = TRUE)

x_orig <- par("usr")[1]
x_max <- par("usr")[4]

forest(
  meta1,
  xlim = c(-16, 7),
  at = log(c(0.001, 0.001, 0.1, 1, 10, 100)),
  ilab = ev,
  ilab.xpos = c(-9.5, -7, -4.5),
  atransf = exp,
  slab = data1$author,
  mlab = lab,
  xlab = "Risk Ratio (log scale)",
  showweights = TRUE,
  header = c("   Study \n (nRCTs)"),
  alim = c(-3, 2),
  psize = 0,
  width = 3)

# modify ilab.xpos argument to position the columns in ev argument

#### -> wi is the inverse variance (proportional to standard error). Used for graphing of the blue box in forest plot. big box = less uncertainty
wi <- 1 / sqrt(meta1$vi)
psize <- wi / sum(wi)
psize <- (psize - min(psize)) / (max(psize) - min(psize))
psize <- (psize * 1.0) + 0.5

op <- par(font = 3)
par(op)

text(-2, -1.6, expression(italic("favours Spikevax")), cex = 0.75)
text(2, -1.6, expression(italic("favours Comirnaty")), cex = 0.75)

grid.text("COVID-19 hospitalization Spikevax vs. Comirnaty",
          x = 0.5,
          y = 0.92,
          gp = gpar(cex = 1.5))

n_bin <- length(meta1$slab)
# text(x = x_orig + c(0,3.5,7,13.15,17,19,21), n_bin + 1.5,
# text(x = x_orig + (x_max - x_orig)*c(0,.25,.33,.42,.83,.87, 0.91),
text(x = c(-9.5, -7, -4.5),
     y = n_bin + 2,
     font = 2,
     # pos = 4,
     labels = c(
       # "   Study \n (nRCTs)",
       "  Spikevax \n (any doses) \n    n/N",
       "  Comirnaty \n (any doses) \n   n/N",
       "Medical\ncondition"
       # "Weight",
       # "Random \n Effects",
       # "Risk Ratio \n [95% CI]"
       ))

# the object meta1 contains all results of the meta-analysis which will be shown in graphical display
points(meta1$yi,
       n_bin:1,
       pch = 15,
       col = rep("blue", 3),
       cex = psize)

dev.print(pdf,
          file = "forest_SpikevaxComirnaty.pdf",
          width = 12,
          height = 9)
dev.off()
