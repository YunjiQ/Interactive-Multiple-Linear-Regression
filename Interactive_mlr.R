# Load the Packages
library(shiny)
library(ggplot2)
library(dplyr)


# Dateset
eco <- economics %>% 
  rename("consumption" = "pce",
         "population" = "pop",
         "saving_rate" = "psavert",
         "unemployment" = "unemploy") %>% 
  select(-uempmed)

# Multiple Linear Regression Model
fit_cons_mr <- lm(consumption ~ saving_rate + population + unemployment, data = eco)


# UI Framework
ui <- fluidPage(
  # Set title
  titlePanel("Interactive Multiple Linear Regression"),
  
  sidebarLayout(
    # Set panel for side bar
    sidebarPanel(
      # Sliders and text boxes for each predictor
      sliderInput("s_rate_slider", "Saving Rate", min(eco$saving_rate), max(eco$saving_rate), mean(eco$saving_rate)),
      numericInput("s_rate_text", "", value = mean(eco$saving_rate), step = 0.1),
      sliderInput("pop_slider", "Population", min(eco$population), max(eco$population), mean(eco$population)),
      numericInput("pop_text", "", value = mean(eco$population), step = 10),
      sliderInput("unemp_slider", "Unemployment", min(eco$unemployment), max(eco$unemployment), mean(eco$unemployment)),
      numericInput("unemp_text", "", value = mean(eco$unemployment), step = 1),
    ),
    # Set panel for plot
    mainPanel(
      plotOutput("regressionPlot"),
      
      textOutput("prediction")
    )
  )
)


# Server Framework
server <- function(input, output, session) {
  
  # Update the text box and slider at the same time when either value changes
  observe({
    updateNumericInput(session, "s_rate_text", value = input$s_rate_slider)
  })
  observe({
    updateNumericInput(session, "pop_text", value = input$pop_slider)
  })
  observe({
    updateNumericInput(session, "unemp_text", value = input$unemp_slider)
  })
  observe({
    updateSliderInput(session, "s_rate_slider", value = input$s_rate_text)
  })
  observe({
    updateSliderInput(session, "pop_slider", value = input$pop_text)
  })
  observe({
    updateSliderInput(session, "unemp_slider", value = input$unemp_text)
  })
  
  # Predict consumption based on input values
  predictedValue <- reactive({
    # Get the input value
    new_data <- data.frame(
      saving_rate = ifelse(is.null(input$s_rate_text) || input$s_rate_text == "", input$s_rate_slider, input$s_rate_text),
      population = ifelse(is.null(input$pop_text) || input$pop_text == "", input$pop_slider, input$pop_text),
      unemployment = ifelse(is.null(input$unemp_text) || input$unemp_text == "", input$unemp_slider, input$unemp_text)
    )
    # Input values to the model and make prediction
    predict(fit_cons_mr, newdata = new_data)
  })
  
  # Show Predicted output value (round down to 2 decimals)
  output$prediction <- renderText({
    paste("Predicted Consumption:", round(predictedValue(), 2))
  })
  
  # Show predicted output plot with GGPlot
  output$regressionPlot <- renderPlot({
    ggplot(data = eco, aes(x = saving_rate + population + unemployment)) +
      geom_point(aes(y = consumption), size = 1) +
      geom_point(aes(x = sum(input$s_rate_text, input$pop_text, input$unemp_text), y = predictedValue()), color = "red", size = 3) +
      geom_line(aes(y = fitted(fit_cons_mr)), color = "orange", size = 0.8) +
      labs(title = "Original Data vs. Real-time Predicted Value",
           x = "Predictors",
           y = "Consumption") +
      theme_minimal()
  })
}


# Run the Interactive App
shinyApp(ui = ui, server = server)
