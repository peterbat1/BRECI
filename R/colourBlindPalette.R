# Colour-blind freindly palette derived from info found here:
#  http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# last accessed 2020-03-14
#
xx <- matrix(c(213,94,0,204,121,167,240,228,66,0,158,115,0,114,178),5,3,byrow = TRUE)/256

cc <- rev(rgb(red = xx))

