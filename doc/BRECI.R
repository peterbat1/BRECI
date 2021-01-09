## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(BRECI)

## ----echo = TRUE, out.width="40%"---------------------------------------------
BRECIplot(ras1, ras2, saveToFile = FALSE)

## ----echo = TRUE, out.width="40%"---------------------------------------------
BRECIplot(ras3, ras4, saveToFile = FALSE)

## ----echo = TRUE, out.width="40%"---------------------------------------------
BRECIplot(ras5, ras6, saveToFile = FALSE)

## ----eval = FALSE-------------------------------------------------------------
#  gainLossMaps(ras1, ras2)
#  

## ----eval = FALSE-------------------------------------------------------------
#  gainLossMaps(ras1, ras2, outFolder = "/path/to/output_folder", saveImages = TRUE)
#  

