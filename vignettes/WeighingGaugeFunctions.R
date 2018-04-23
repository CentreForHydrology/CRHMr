## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(CRHMr)
library(ggplot2)
wg <- wg

## ------------------------------------------------------------------------
weighingGaugePlot(wg)

## ------------------------------------------------------------------------
wg1 <- weighingGauge1(wg, maxGapLength = 10000)
weighingGaugePlot(wg1)

## ------------------------------------------------------------------------
wg2 <- weighingGauge2(wg1, spikeThreshold = 200, maxSpikeGap = 3)
weighingGaugePlot(wg2)

## ------------------------------------------------------------------------
wg3 <- weighingGauge5(wg2) # using the default value of 50 mm for resetThreshold
weighingGaugePlot(wg3)

## ------------------------------------------------------------------------
wg4 <- weighingGauge4(wg1, smallDropThreshold = 0.05, serviceThreshold = -50)
weighingGaugePlot(wg4)

## ------------------------------------------------------------------------
interval <- weighingGaugeInterval(wg3)
plotObs(interval[, c(1, 3)]) + ggplot2::theme(legend.position = "none")

