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
            "Turn off the all the lights.",
            "Pressing a square toggles it and it's N-E-W-S neighbors.",
            "You never have to hit the same square twice",
            "and a solution is guaranteed.",
            hr(),
            wellPanel(
              style = "background-color: #fefeff;",
              sliderInput(inputId = "nSquares",
                          label = "Board Size:",
                          min = 3, max = 10, step = 1,
                          value = 5,
                          round = 1,
                          ticks = TRUE,
                          width = "60%"),
              selectInput(inputId = "difficulty",
                          label = "Difficulty:",
                          choices = c("Easy" = 1,
                                      "Medium" = 2,
                                      "Hard" = 3,
                                      "Crazy" = 4),
                          selected = 2),
              actionButton(inputId = "reset",
                           label = "New Game"),
              hr(),
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
