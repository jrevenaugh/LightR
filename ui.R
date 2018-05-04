# UI
#
# Establish a pageWithSidebar with input control widgets and instructions on the LHS
# and board illustration on RHS
# on RHS.

require(shiny)

ui <- fluidPage(title = "LightR v1.0",
        sidebarLayout(
          sidebarPanel(
            h4("Lights Out"),
            br(),
            h4("Instructions"),
            h5("Objective:"),
            "Turn off the all the lights.", br(), br(),
            "Press a square to toggle it and its N-E-W-S neighbors.",
            "There's always a solution and you never need to hit the",
            "same button more than once",
            hr(),
            wellPanel(
              style = "background-color: #fefeff;",
              sliderInput(inputId = "nSquares",
                          label = "Board Size:",
                          min = 3, max = 10, step = 1,
                          value = 5,
                          round = 1,
                          ticks = FALSE,
                          width = "60%"),
              checkboxInput(inputId = "withGrobs",
                            label = "Fancy Buttons",
                            value = TRUE),
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
                           label = "Hint")
            )
          ),
          mainPanel(plotOutput(outputId = "board",
                               height = "600px",
                               click = "board_click")
          )
        )
)
