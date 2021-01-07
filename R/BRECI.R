# Binned Relative Environmental Change Index (BRECI) function
#
# Computes an index to summarise the degree and nature of differences between
# two rasters representing environmental suitability over the same geographical
# extent
#
# This function is based on code developed in April 2017 but the first approach
# to this kind of index was developed in 2009-2010. Code for that earlier method
# is no longer available.
#
# The two rasters to be compared using this function MUST:
#  1. Have the same extent and resolution (grid cell size); and,
#
#  2. Have values in gridcells scaled in the same way. The rasters may be
#  normalised values such that the sum of grid cell values across the extent = 1
#  (as in MaxEnt raw output) or adjusted so that values in a grid cell may range
#  from 0 to 1 (as in MaxEnt logistic output).
#
# Peter D. Wilson
# Biodiversity Analyst
# Evolutionary Ecology Research Section
# Science & Conservation Branch
# Royal Botanic Garden, Sydney
#
# 28 April 2018; 09 December 2018: Made into a function within a new R-project 'BRECI'.

#library(raster)

#' Make a BRECI plot
#'
#' Make a BRECI plot from two rasters output by an ENM fitting process. Typically, this is MaxEnt but any raster output from any ENM fitting method is acceptable provided that the rasters are on identical geometries and have values in gridcells scaled in the same way.
#'
#' @param ras1 Character. Full path to the baseline raster (earliest in time).
#' @param ras2 Character. Full path to the future raster.
#' @param outFilename Character. Full path to the output file.
#' @param saveToFile Logical. Should the output plot be saved to file. DEFAULT = TRUE.
#' @param plotTitle Character. Title to be give to the plot.
#' @param plotWidth Integer. Width of the output plot in pixels. Default is 250 pixels.
#' @param plotHeight Integer. height of the output plot in pixels. Default is 188 pixels.
#' @param brks Numeric. An array of bin break points. Default gives 5 equally spaced bins between 0 and 1.
#' @param binNames Character. An array of names assigned to the bins.
#' @param binCols Character. An array of standard R colour names or hexadecimal colour values.
#' @param scaleFactor Character. Selects scaling method for bin values. One of 'ras1Bins' or 'numCells' (default). See Details for an explanation.
#'
#' @return TRUE
#' @export
#'
#' @details {
#' The two rasters to be compared using this function \emph{MUST} have:
#' \enumerate{
#'  \item the same extent and resolution (grid cell size); and,
#'
#'  \item values in gridcells scaled in the same way. The rasters may be
#'  normalised values such that the sum of grid cell values across the extent = 1
#'  (as in MaxEnt raw output) or adjusted so that values in a grid cell may range
#'  from 0 to 1 (as in MaxEnt logistic and cloglog output, and many other modelling methods).
#'}
#'
#' \emph{scaleFactor} determines the way in which values in bins are computed. Selecting 'ras1Bins' computes the value for a bin as a fraction of the number of pixels in the first raster. Values in a bin can therefore be greater than +/- 1, and are \emph{greatly} exaggerated in size.
#'
#' For the ith bin:
#'
#'  \verb{    Bin_size(i) = [ras2_count(i) - ras1_count(i)]/ras1_count(i)}
#'
#' Selecting 'numCells' (the default value) computes the difference in the number of cells in that bin as a proportion of the total number of cells in the raster extent.
#'
#' For the ith bin:
#'
#'   \verb{    Bin_size(i) = [ras2_count(i) - ras1_count(i)]/Total_number_cells}
#'
#' This function is based on code developed in April 2017 but the first approach to this kind of index was developed in 2009-2010.
#'
#' }
#'
#' @examples
#' data(breciRasters)
#'
#' # Default plot
#' BRECIplot(ras1, ras2, plotTitle = "Acacia linifolia", outFilename = "~/testPlot.png")
#'
#' # A colour-blind friendly palette derived from info found here:
#' # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#' # last accessed 2020-03-14
#' colTable <- matrix(c(213,94,0,204,121,167,240,228,66,0,158,115,0,114,178), 5, 3, byrow = TRUE)/256
#' niceColours <- rev(rgb(colTable))
#' BRECIplot(ras1, ras2, plotTitle = "Acacia linifolia", outFilename = "~/testPlot.png", binCols = niceColours)
#'
BRECIplot <- function(ras1 = NULL, ras2 = NULL, outFilename = NULL, saveToFile = TRUE,
                      plotTitle = "", plotWidth = 275, plotHeight = 200,
                      brks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                      binNames = c("V. low", "Low", "Med.", "High", "V. high"),
                      binCols = c("dodgerblue", "darkolivegreen2", "gold1", "orange", "red"),
                      scaleFactor = "numCells")
{
  if ((is.null(ras1)) || (class(ras1) != "RasterLayer"))
    stop("An object of class 'RasterLayer' is required for parameter 'ras1'")

  if ((is.null(ras2)) || (class(ras2) != "RasterLayer"))
    stop("An object of class 'RasterLayer' is required for parameter 'ras2'")

  if (raster::extent(ras1) != raster::extent(ras2))
    stop("Spatial extent of 'ras1' and 'ras2' must be the same")

  if (!all(raster::res(ras1) == raster::res(ras2)))
    stop("Spatial resolution (grid cell size) of 'ras1' and 'ras2' must be the same")

  if (!sp::identicalCRS(ras1, ras2))
    stop("Coordinate Reference System (CRS) of 'ras1' and 'ras2' must be identical")

  if ((is.null(outFilename) || (outFilename == "")) && saveToFile)
    stop("Parameter 'outFilename' must be given a value because 'saveToFile' = TRUE")

  if (!(scaleFactor %in% c("ras1Bins", "numCells")))
    stop("scaleFactor must be one of 'ras1Bins' or 'numCells'")

  binNamePos <- brks[1:(length(brks) - 1)] + diff(brks)/2

  ras1_values <- raster::values(ras1)
  na_ind <- which(is.na(ras1_values))
  if (length(na_ind) > 0) ras1_values <- ras1_values[-na_ind]

  ras2_values <- raster::values(ras2)
  na_ind <- which(is.na(ras2_values))
  if (length(na_ind) > 0) ras2_values <- ras2_values[-na_ind]

  # Classify values by bin
  ras1_cut <- cut.default(ras1_values, breaks = brks)
  ras2_cut <- cut.default(ras2_values, breaks = brks)

  # Cross-tabulation quickly count number of cells in each bin
  xTab <- table(ras1_cut,ras2_cut)

  # Marginal sums in the cross-tabulation will let us see the numbers of cells
  # in each bin for each map
  map1Binned <- rowSums(xTab)
  map2Binned <- colSums(xTab)

  # Compute the proportional change in each bin, and clamp infinite values
  # caused by dividing by a very small value to a tractable but large number
  if (scaleFactor == "ras1Bins")
    z <- (map2Binned - map1Binned)/map1Binned
  else
    z <- (map2Binned - map1Binned)/raster::ncell(ras1) #map1Binned

  infInd <- which(is.infinite(z))
  if (length(infInd) > 0)
  {
    z[infInd] <- sign(map2Binned[infInd] - map1Binned[infInd]) * 10
  }

  # and clamp Nan to zero
  nanInd <- which(is.nan(z))
  if (length(nanInd) > 0) z[nanInd] <- 0

  # If necessary, set output device for png image to be rendered to file:
  if (saveToFile) png(outFilename, width = plotWidth, height = plotHeight)
  op <- par(oma = c(0, 0, 0, 0), mar = c(4, 3, 2, 0.1), mgp = c(2, 0.5, 0))
  # Make the plot:
  # Set xlim bounds:
  biggestDiff <- ifelse(max(z) < 1, 1, max(z))
  plot(1,1,
       xlim = c(-biggestDiff, biggestDiff),
       ylim = c(0, 1),
       type = "n",
       xlab = "",
       ylab = "",
       frame.plot = FALSE,
       yaxt = "n")

  mtext(c("Loss", "Gain"), at = c(-biggestDiff/2, biggestDiff/2), font = 2)

  mtext(substitute(italic(x), list(x = plotTitle)),
        side = 3, cex = 1.1, font = 2, line = 1)

  mtext("Proportional change", side = 1, line = 1.75)

  if (scaleFactor == "ras1Bins")
    mtext("(relative to baseline raster bin values)", side = 1, line = 2.5, cex = 0.7)
  else
    mtext("(relative to total num. cells)", side = 1, line = 2.5, cex = 0.7)

  mtext("Env. suitability class", side = 2, line = 2)

  # Set alternate rows to light grey background of improved readability
  rect(-biggestDiff, brks[1], biggestDiff, brks[2], col = "grey90", border = NA)
  rect(-biggestDiff, brks[3], biggestDiff, brks[4], col = "grey90", border = NA)
  rect(-biggestDiff, brks[5], biggestDiff, brks[6], col = "grey90", border = NA)


  mtext(binNames, at = binNamePos, side = 2, cex = 0.8, las = 2, line = -0.1)

  abline(v = 0, lty = 2)

  # Make rectangles for each bin:
  for (thisBin in 1:length(binNames))
  {
    if ((z[thisBin] != 0) && !is.nan(z[thisBin]))
    {
      if (z[1] < 0)
      {
        rect(z[thisBin], brks[thisBin], 0, brks[thisBin + 1], col = binCols[thisBin])
      }
      else
      {
        rect(0, brks[thisBin], z[thisBin], brks[thisBin + 1], col = binCols[thisBin])
      }
    }
  }

  par(op)

  if (saveToFile) dev.off()

  invisible(TRUE)
}
