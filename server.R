# server
#
# LightR shiny server

require(tidyverse)
require(shiny)
source("plotBoard.R")
source("ga2.R")

server <- function(input, output, session) {

  # Reactives ------------------------------------------------------------------
  grid <- reactiveValues(g = rep(TRUE, 100))

  hint <- reactiveValues(current = 0,
                         show = FALSE,
                         hints = 0)

  gCenter <- reactiveValues(x = 0,
                            y = 0)

  winner <- reactiveValues(yeah = FALSE)

  # Event Observers ------------------------------------------------------------
  # Reset game
  observeEvent(c(input$reset, input$nSquares), {
    x <- rep(0, input$nSquares^2)
    l <- as.integer(input$difficulty)
    nPress <- round(input$nSquares^2 * diffScale[l], 0)
    k <- sample(1:input$nSquares^2, size = nPress)
    x[k] <- 1
    grid$g <- mmultGA2(buildA(input$nSquares), x)

    winner$yeah <- FALSE
    n <- input$nSquares
    gCenter$x <- rep(seq( 0, 5, length.out = n), n)
    gCenter$y <- rep(seq(5, 0, length.out = n), each = n)
    hint$current <- 0
    hint$show <- FALSE
  })

  # Provide a hint
  observeEvent(input$hint, {
    if (!hint$show) {                 # You just got a hint, you need to act
      if (hint$current == 0 | (hint$current >= length(hint$hints))) {
        x <- solveGA2(buildA(input$nSquares), grid$g)
        hint$hints <- which(x == 1)
        hint$current <- 1
        hint$show <- TRUE
      } else {
        hint$current <- hint$current + 1
        hint$show = TRUE
      }
    }
  }, ignoreInit = FALSE
)

  # Process toggle if location is valid
  observeEvent(input$board_click, {
    x <- input$board_click$x
    y <- input$board_click$y
    gDist <- sqrt((gCenter$x - x)^2 + (gCenter$y - y)^2)
    l <- which.min(gDist)
    boxRow <- (l - 1) %/% input$nSquares + 1
    boxCol <- (l - 1) %% input$nSquares + 1
    k <- c(l)
    if (boxRow > 1) k <- c(k, l - input$nSquares)
    if (boxRow < input$nSquares) k <- c(k, l + input$nSquares)
    if (boxCol > 1) k <- c(k, l - 1)
    if (boxCol < input$nSquares) k <- c(k, l + 1)
    grid$g[k] <- !grid$g[k]
    if (sum(grid$g) == 0) winner$yeah <- TRUE
    if (hint$current > 0) {
      if (l != hint$hints[hint$current]) hint$current <- 0
    }
    hint$show <- FALSE
  })

  # Main Panel -----------------------------------------------------------------
  # Gameboard plot
  output$board <- renderPlot({
    g <- plotBoard(grid$g)
    if (winner$yeah) {
      g <- g + annotate("text",
                        x = 2.5,
                        y = 2.5,
                        label = "Winner!",
                        size = 50,
                        color = winningColor)
    }
    if (hint$show) {
      l <- hint$hints[hint$current]
      g <- g + annotate("point",
                        x = gCenter$x[l],
                        y = gCenter$y[l],
                        color = "white",
                        fill = "red",
                        size = 10,
                        pch = 22)
    }
    return(g)
  })
}
