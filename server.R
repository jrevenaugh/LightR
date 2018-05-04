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

  solution <- reactiveValues(optimal = 0,
                             current = 0)

  # Event Observers ------------------------------------------------------------
  # Reset game
  # Reacts to New Game button, puzzle size slider and puzzle difficulty
  # selector
  observeEvent(c(input$reset, input$nSquares, input$difficulty), {
    x <- rep(0, input$nSquares^2)
    l <- as.integer(input$difficulty)
    nPress <- round(input$nSquares^2 * diffScale[l], 0)
    k <- sample(1:input$nSquares^2, size = nPress)
    x[k] <- 1
    grid$g <- mmultGA2(buildA(input$nSquares), x)
    X <- optimalSolution(grid$g)
    solution$optimal <- X$n
    solution$current <- 0

    winner$yeah <- FALSE
    n <- input$nSquares
    gCenter$x <- rep(seq( 0, 5, length.out = n), n)
    gCenter$y <- rep(seq(5, 0, length.out = n), each = n)
    hint$current <- 0
    hint$show <- FALSE
  })

  # Provide a hint.  This is a bit difficult.  We want a sequence of hints to
  # eventually produce a solution.  But it's hard if the player doesn't follow
  # them.  Reveal a list of sequential hints if player follows them.  Start fresh
  # when they don't.
  observeEvent(input$hint, {
    if (!hint$show) {                 # You just got a hint, you need to act
      if (hint$current == 0 | (hint$current >= length(hint$hints))) {
        X <- optimalSolution(grid$g)
        x <- X$x
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

  # Process toggle if location is valid.  Pay attention to hint status--if there's
  # a hint shown and player follows it, increment hint counter.  If they don't
  # follow it, refresh the counter
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
    solution$current <- solution$current + 1
    hint$show <- FALSE
  })

  # Render step counter
  output$nSolution <- renderText(paste0("Shortest: ",
                                        solution$optimal,
                                        "\nCurrent: ",
                                        solution$current))

  # Main Panel -----------------------------------------------------------------
  # Gameboard plot.  Most of the work is done in plotBoard.  Extra items added
  # if last moved solved puzzle or if there's an active hint.
  output$board <- renderPlot({
    g <- plotBoard(grid$g, input$withGrobs)
    if (winner$yeah) {
      g <- g + annotate("text",
                        x = 2.5,
                        y = 2.5,
                        label = "Winner!",
                        size = 50,
                        color = winningColor)
    }
    if (hint$show) {
      bRange <- diff(range(gCenter$x))
      buttonSize <- bRange / (2.1 * input$nSquares)
      l <- hint$hints[hint$current]
      g <- g + annotate("tile",
                        x = gCenter$x[l],
                        y = gCenter$y[l],
                        color = "white",
                        fill = hintColor,
                        width = 2 * buttonSize,
                        height = 2 * buttonSize,
                        alpha = 0.6)
    }
    return(g)
  })
}
