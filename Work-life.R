library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("Work-Life Balance Evaluator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("workHours", "Work Hours (per week):", min = 0, max = 80, value = 40),
      sliderInput("leisureHours", "Leisure Hours (per week):", min = 0, max = 50, value = 10),
      sliderInput("stressLevel", "Stress Level (1-10):", min = 1, max = 10, value = 5)
    ),
    mainPanel(
      plotOutput("balancePlot"),
      textOutput("recommendation")
    )
  )
)

# Server
server <- function(input, output) {
  output$balancePlot <- renderPlot({
    df <- data.frame(
      Category = c("Work", "Leisure"),
      Hours = c(input$workHours, input$leisureHours)
    )
    ggplot(df, aes(x = Category, y = Hours, fill = Category)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Work vs. Leisure Balance")
  })
  
  output$recommendation <- renderText({
    if (input$workHours > 50) {
      "Your work hours are high. Consider reducing to improve balance."
    } else if (input$leisureHours < 10) {
      "You have limited leisure time. Try incorporating more relaxation activities."
    } else {
      "Your work-life balance looks good!"
    }
  })
}

# Run App
shinyApp(ui = ui, server = server)
