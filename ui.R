# UI
#
# Establish a pageWithSidebar with input control widgets and instructions on the LHS
# and board illustration on RHS
# on RHS.

require(shiny)

ui <- bootstrapPage(
  title = "Lights Out",
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  plotOutput(outputId = "board",
             height = "100%",
             width = "100%",
             click = "board_click"),

  absolutePanel(top = 10, left = 10, width = "260px", draggable = TRUE,
                wellPanel(h4("Lights Out\U2122 V1.0"),
                          sliderInput(inputId = "nSquares",
                                      label = "Board Size:",
                                      min = 3, max = 10, step = 1,
                                      value = 5,
                                      round = 1,
                                      ticks = FALSE,
                                      width = "60%"),
                          checkboxInput(inputId = "withGrobs",
                                        label = "Fancy Buttons",
                                        value = FALSE),
                          selectInput(inputId = "difficulty",
                                      label = "Difficulty:",
                                      choices = c("Easy" = 1,
                                                  "Medium" = 2,
                                                  "Hard" = 3,
                                                  "Crazy" = 4),
                                      selected = 2),
                          verbatimTextOutput(outputId = "nSolution",
                                             placeholder = TRUE),
                          actionButton(inputId = "reset",
                                       label = "New Game"),
                          actionButton(inputId = "hint",
                                       label = "Hint"),
                          actionButton(inputId = "help",
                                       label = "Help"),
                          style = "opacity: 0.8; background:#FAFAFA;")
  )
)
