# functions

#' Custom Forest Plot
#' 
#' @param meta_out Output of metafor::rma
#' @param save Logical
#' @param altplot Alternative plot
#' @param dpi Dots per inch
#' @param plot_top 
#' @param title_line specifying a value for line overrides the default placement of labels, and places them this many lines outwards from the plot edge.
#' @param width 
#' @param height 
#'
#' @importFrom metafor forest
#'
forest_plot <- function(meta_out,
                        save = FALSE,
                        altplot = FALSE,
                        # plot_units = "in",
                        dpi = 96,   # NG laptop
                        plot_top = 4,
                        title_line = 2,
                        width = 12,
                        height = 11,
                        title_text = "My Title",
                        ref = "X",
                        comp = "Y",
                        filename = "forest.png") {
  
  if (save) {
    png(glue::glue("plots/{filename}"), width = width*dpi, height = height*dpi, res = dpi)
    on.exit(dev.off())
  } else if (shiny::isRunning()) {
    #
  } else {
    dev.new(width = width,
            height = height,
            noRStudioGD = TRUE)
  }
  
  if (!altplot) {
    lab <- glue(
      "\nTotal events: {sum(dat$n1)} ({ref}), {sum(dat$n2)} ({comp}) \n",
      "Heterogeneity: Chi²={round(meta_out$QE, 2)}, df={meta_out$k - 1} ",
      "(P= {round(meta_out$QEp, 2)}), I²={round(meta_out$I2, 1)}% \n",
      "Test for overall effect: Z={round(meta_out$zval, 2)} ",
      "(P= {round(meta_out$pval, 4)})"
    )
    
    # ratio text
    ev1 <- data.frame(paste(paste(dat2$n1), "/", paste(dat2$N1)))
    ev2 <- data.frame(paste(paste(dat2$n2), "/", paste(dat2$N2)))
    
    ev <- cbind(data.frame(c(ev1, ev2), "type"))
    # ev <- cbind(data.frame(c(ev1, ev2)), dat2$type)  ##TODO: what is dat2$type
    
    metafor::forest(
      meta_out,
      atransf = exp,
      slab = dat2$author,
      mlab = lab,
      xlab = "Risk Ratio (log scale)",
      showweights = TRUE,
      at = log(c(0.001, 0.001, 0.1, 1, 10, 100, 1000)),
      xlim = c(-35, 35),
      alim = c(-60, 70),
      ilab = ev,
      ilab.xpos = c(-18, -10, 10),
      psize = 0,
      width = 5,
      top = plot_top
    )
    
    n_studies <- length(meta_out$slab)
    
    wi <- 1 / sqrt(meta_out$vi)
    psize <- wi / sum(wi)
    psize <- (psize - min(psize)) / (max(psize) - min(psize))
    psize <- (psize * 1.0) + 0.5
    
    op <- par(font = 3)
    par(op)
    
    ref_txt <- glue::glue("favours {ref}")
    comp_txt <- glue::glue("favours {comp}")
    
    print(comp_txt)
    
    text(-3, -1.8, bquote(italic(.(ref_txt))), cex = 0.75)
    text(5, -1.8, bquote(italic(.(comp_txt))), cex = 0.75)
    
    # grid.text("COVID-19 Death Spikevax vs. Comirnaty", 0.5, 1,
    #           gp = gpar(cex = 1.5))
    title(main = title_text, line = title_line)
    
    text(-30, n_studies + 2, "Study")
    text(-30, n_studies + 1.5, "(nRCTs)")
    text(c(-18, -10, 10), n_studies + 2, c(ref, comp, "Type"))
    text(c(-18, -10), n_studies + 1.5, c("n/N", "n/N"))
    text(19, n_studies + 1.5, "Weight")
    text(25, n_studies + 1.5, "Random \n Effects")
    text(32, n_studies + 1.5, "Risk Ratio \n [95% CI]")
    
    points(
      meta_out$yi,
      rev(seq_along(meta_out$yi)),
      pch = 15,
      col = rep("blue", 21),
      cex = psize)
  } else {
    
    # create label for forest plot
    lab <- glue(
      "\nTotal events: {sum(dat2$n1)} ({ref}), {sum(dat2$n2)} ({comp}) \n",
      "Heterogeneity: Chi²={round(meta_out$QE, 2)}, df={meta_out$k - 1} ",
      "(P= {round(meta_out$QEp, 2)}), I²={round(meta_out$I2, 1)}% \n",
      "Test for overall effect: Z={round(meta_out$zval, 2)} ",
      "(P= {round(meta_out$pval, 4)})"
    )
    
    # create label to show number of events and sample size
    ev1 <- data.frame(paste(paste(dat2$n1), "/", paste(dat2$N1)))
    ev2 <- data.frame(paste(paste(dat2$n2), "/", paste(dat2$N2)))
    
    # combine labels for both arms with label on condition
    ev <-
      # cbind.data.frame(data.frame(c(ev1, ev2)), dat2$type) |>
      cbind.data.frame(data.frame(c(ev1, ev2)), "type") |>
      `colnames<-`(c("a", "b", "c"))
    
    x_orig <- par("usr")[1]
    x_max <- par("usr")[4]
    
    metafor::forest(
      meta_out,
      xlim = c(-16, 7),
      at = log(c(0.001, 0.001, 0.1, 1, 10, 100)),
      ilab = ev,
      ilab.xpos = c(-9.5, -7, -4.5),
      atransf = exp,
      slab = dat2$author,
      mlab = lab,
      xlab = "Risk Ratio (log scale)",
      showweights = TRUE,
      header = c("   Study \n (nRCTs)"),
      alim = c(-3, 2),
      psize = 0,
      width = 3,
      top = plot_top
    )
    
    # modify ilab.xpos argument to position the columns in ev argument
    
    #### -> wi is the inverse variance (proportional to standard error)
    # Used for graphing of the blue box in forest plot
    # big box = less uncertainty
    wi <- 1 / sqrt(meta_out$vi)
    psize <- wi / sum(wi)
    psize <- (psize - min(psize)) / (max(psize) - min(psize))
    psize <- (psize * 1.0) + 0.5
    
    op <- par(font = 3)
    par(op)
    
    text(-2, -1.6, expression(italic("favours X")), cex = 0.75)
    text(2, -1.6, expression(italic("favours Y")), cex = 0.75)
    
    title(main = title_text, line = title_line)
    
    n_bin <- length(meta_out$slab)
    
    # text(x = x_orig + c(0,3.5,7,13.15,17,19,21), n_bin + 1.5,
    # text(x = x_orig + (x_max - x_orig)*c(0,.25,.33,.42,.83,.87, 0.91),
    text(
      x = c(-9.5, -7, -4.5),
      y = n_bin + 3,
      font = 2,
      # pos = 4,
      labels = c(
        # "   Study \n (nRCTs)",
        "  X \n (any doses) \n    n/N",
        "  Y \n (any doses) \n   n/N",
        "Medical\ncondition"
        # "Weight",
        # "Random \n Effects",
        # "Risk Ratio \n [95% CI]"
      )
    )
    
    points(
      meta_out$yi,
      n_bin:1,
      pch = 15,
      col = rep("blue", 21),
      cex = psize)
  } 
}
