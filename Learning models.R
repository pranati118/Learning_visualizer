library(shiny)
library(ggplot2)

# UI
ui <- navbarPage(
  title = "Interactive Learning Modules",
  
  # Home Page
  tabPanel("Home",
           fluidPage(
             titlePanel("Interactive Learning Modules"),
             p("Welcome! Explore different modules to learn interactively."),
             tags$ul(
               tags$li("Normal Distribution Visualizer"),
               tags$li("Monte Carlo Pi Estimation"),
               tags$li("Supply & Demand Simulator"),
               tags$li("SIR Epidemic Model"),
               tags$li("Time Series Visualizer"),
               tags$li("Game Theory Simulator")
             )
           )
  ),
  
  # Page 1: Normal Distribution Visualizer
  tabPanel("Normal Distribution",
           sidebarLayout(
             sidebarPanel(
               sliderInput("mean", "Mean:", min = -10, max = 10, value = 0),
               sliderInput("sd", "Standard Deviation:", min = 0.1, max = 5, value = 1)
             ),
             mainPanel(
               plotOutput("normPlot"),
               p("Adjust the mean and standard deviation to change the shape.")
             )
           )
  ),
  
  # Page 2: Monte Carlo Pi Estimation
  tabPanel("Monte Carlo Pi Estimation",
           sidebarLayout(
             sidebarPanel(
               sliderInput("points", "Number of Points:", min = 100, max = 10000, value = 1000)
             ),
             mainPanel(
               plotOutput("piPlot"),
               p("Visualize random points to estimate the value of π.")
             )
           )
  ),
  
  # Page 3: Supply & Demand Simulator
  tabPanel("Supply & Demand",
           sidebarLayout(
             sidebarPanel(
               sliderInput("supplySlope", "Supply Slope:", min = 0.1, max = 2, value = 1),
               sliderInput("demandSlope", "Demand Slope:", min = -2, max = -0.1, value = -1)
             ),
             mainPanel(
               plotOutput("marketPlot"),
               p("Understand market equilibrium with adjustable slopes.")
             )
           )
  ),
  
  # Page 4: SIR Epidemic Model
  tabPanel("SIR Epidemic Model",
           sidebarLayout(
             sidebarPanel(
               sliderInput("beta", "Infection Rate (β):", min = 0.1, max = 1, value = 0.3),
               sliderInput("gamma", "Recovery Rate (γ):", min = 0.1, max = 1, value = 0.1),
               sliderInput("initInfected", "Initial Infected:", min = 1, max = 100, value = 10)
             ),
             mainPanel(
               plotOutput("sirPlot"),
               p("Simulate how an epidemic evolves with different parameters.")
             )
           )
  ),
  
  # Page 5: Time Series Visualizer
  tabPanel("Time Series Visualizer",
           sidebarLayout(
             sidebarPanel(
               sliderInput("window", "Moving Average Window:", min = 1, max = 20, value = 5)
             ),
             mainPanel(
               plotOutput("tsPlot"),
               p("Analyze trends with moving averages.")
             )
           )
  ),
  
  # Page 6: Game Theory Simulator
  tabPanel("Game Theory",
           sidebarLayout(
             sidebarPanel(
               numericInput("p1_strat1", "Player 1, Strategy 1 Payoff:", value = 3),
               numericInput("p1_strat2", "Player 1, Strategy 2 Payoff:", value = 2),
               numericInput("p2_strat1", "Player 2, Strategy 1 Payoff:", value = 4),
               numericInput("p2_strat2", "Player 2, Strategy 2 Payoff:", value = 1)
             ),
             mainPanel(
               verbatimTextOutput("payoffMatrix"),
               p("Adjust payoffs to explore Nash Equilibria.")
             )
           )
  )
)

# Server
server <- function(input, output) {
  
  # Normal Distribution
  output$normPlot <- renderPlot({
    x <- seq(-30, 30, length.out = 300)
    y <- dnorm(x, mean = input$mean, sd = input$sd)
    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line() +
      labs(title = "Normal Distribution", x = "Value", y = "Density") +
      theme_minimal()
  })
  
  # Monte Carlo Pi Estimation
  output$piPlot <- renderPlot({
    set.seed(123)
    x <- runif(input$points)
    y <- runif(input$points)
    inside_circle <- (x^2 + y^2) <= 1
    pi_est <- 4 * sum(inside_circle) / input$points
    ggplot(data.frame(x, y, inside_circle), aes(x, y, color = inside_circle)) +
      geom_point(size = 1) +
      coord_fixed() +
      labs(
        title = paste("Monte Carlo Simulation of π:", round(pi_est, 4)),
        x = "X Coordinate", y = "Y Coordinate"
      ) +
      theme_minimal()
  })
  
  # Supply & Demand
  output$marketPlot <- renderPlot({
    price <- seq(0, 10, length.out = 100)
    supply <- input$supplySlope * price
    demand <- 10 + input$demandSlope * price
    ggplot() +
      geom_line(aes(price, supply), color = "blue", linetype = "dashed") +
      geom_line(aes(price, demand), color = "red") +
      labs(title = "Supply & Demand Curves", x = "Price", y = "Quantity") +
      theme_minimal()
  })
  
  # SIR Epidemic Model
  output$sirPlot <- renderPlot({
    beta <- input$beta
    gamma <- input$gamma
    S <- 1000 - input$initInfected
    I <- input$initInfected
    R <- 0
    days <- 100
    results <- data.frame(Day = 1:days, S = NA, I = NA, R = NA)
    for (day in 1:days) {
      new_infected <- beta * S * I / 1000
      new_recovered <- gamma * I
      S <- S - new_infected
      I <- I + new_infected - new_recovered
      R <- R + new_recovered
      results[day, ] <- c(day, S, I, R)
    }
    ggplot(results, aes(x = Day)) +
      geom_line(aes(y = S, color = "Susceptible")) +
      geom_line(aes(y = I, color = "Infected")) +
      geom_line(aes(y = R, color = "Recovered")) +
      labs(title = "SIR Epidemic Model", x = "Day", y = "Population") +
      theme_minimal()
  })
  
  # Time Series
  output$tsPlot <- renderPlot({
    set.seed(123)
    ts_data <- cumsum(rnorm(100))
    ma <- zoo::rollmean(ts_data, input$window, fill = NA)
    data <- data.frame(Time = 1:100, Value = ts_data, MA = ma)
    ggplot(data, aes(x = Time)) +
      geom_line(aes(y = Value), color = "blue") +
      geom_line(aes(y = MA), color = "red") +
      labs(title = "Time Series with Moving Average", x = "Time", y = "Value") +
      theme_minimal()
  })
  
  # Game Theory
  output$payoffMatrix <- renderPrint({
    matrix(
      c(input$p1_strat1, input$p1_strat2, input$p2_strat1, input$p2_strat2),
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("Player 1", "Player 2"), c("Strategy 1", "Strategy 2"))
    )
  })
}

# Run the App
shinyApp(ui, server)
