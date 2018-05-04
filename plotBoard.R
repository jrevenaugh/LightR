# plotBoard
#
# Use ggplot2 to create a Lights Out board graphic.  grid is the collection
# of lights stored as a vector reading row-wise from upper left.  TRUE is lit.
#
require(tidyverse)
require(png)
require(grid)
source("global.R")

img <- readPNG( "on.png" )
onGrob <- rasterGrob( img, interpolate = FALSE )
img <- readPNG( "off.png" )
offGrob <- rasterGrob( img, interpolate = FALSE )

plotBoard <- function(grid, withGrobs = TRUE) {
  n <- sqrt(length(grid))
  buttons <- data.frame(x = rep(seq( 0, 5, length.out = n), n),
                        y = rep(seq(5, 0, length.out = n), each = n))
  buttons$gc <- grid
  darkButtons <- buttons %>% filter(gc == FALSE)
  litButtons <- buttons %>% filter(gc == TRUE)
  bRange <- diff(range(buttons$x))
  buttonSize <- bRange / (2.1 * n)
  xRange <- (bRange + 3 * buttonSize) / 2
  xMean <- mean(buttons$x)
  xMin <- xMean - xRange
  xMax <- xMean + xRange
  yMin <- xMean - xRange
  yMax <- xMean + xRange

  square <- data.frame( x = c(xMin, xMax, xMax, xMin, xMin),
                        y = c(yMin, yMin, yMax, yMax, yMin))


  g <- ggplot() +
    scale_y_continuous(limits = c(xMin - 1, xMax + 1)) +
    scale_x_continuous(limits = c(xMin - 1, xMax + 1)) +
    coord_equal(expand = FALSE) +

  # Outline board
    geom_polygon(data = square,
                 aes(x, y),
                 fill = backgroundColor,
                 color = "black",
                 size = 2)

  # Add dark buttons
  if (withGrobs) {
    for (i in seq_along(darkButtons$x)) {
        g <- g + annotation_custom(offGrob,
                                  xmin = darkButtons$x[i] - buttonSize,
                                  xmax = darkButtons$x[i] + buttonSize,
                                  ymin = darkButtons$y[i] - buttonSize,
                                  ymax = darkButtons$y[i] + buttonSize)
    }
  } else {
    g <- g + geom_tile(data = darkButtons, aes(x, y),
                       width = 2 * buttonSize,
                       height = 2 * buttonSize,
                       fill = "springgreen4",
                       color = "black")
  }

  # Add lit buttons
  if (withGrobs) {
    for (i in seq_along(litButtons$x)) {
      g <- g + annotation_custom(onGrob,
                                 xmin = litButtons$x[i] - buttonSize,
                                 xmax = litButtons$x[i] + buttonSize,
                                 ymin = litButtons$y[i] - buttonSize,
                                 ymax = litButtons$y[i] + buttonSize)
    }
  } else {
    g <- g + geom_tile(data = litButtons, aes(x, y),
                       width = 2 * buttonSize,
                       height = 2 * buttonSize,
                       fill = "springgreen",
                       color = "black")
  }
  g <- g + theme_void()

  return(g)
}
