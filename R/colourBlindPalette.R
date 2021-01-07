# Colour-blind friendly palette derived from info found here:
#  http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# last accessed 2020-03-14
#

#' Generate a colour-blind friendly palette
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' ## Call BRECIplot replacing default colours with colour-blind friendly palette
#'
#' BRECIplot(ras1, ras2, plotTitle = "Acacia linifolia", outFilename = "~/testPlot.png", binCols = cbPalette())
#' }
cbPalette <- function()
{
  rev(rgb(red = matrix(c(213, 94, 0,
                         204, 121, 167,
                         240, 228, 66,
                         0, 158, 115,
                         0, 114, 178),
                       nrow = 5,
                       ncol = 3,
                       byrow = TRUE)/256))
}
