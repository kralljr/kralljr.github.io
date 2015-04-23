# File to define input and output
library(dplyr)
shinyServer(function(input, output) {
	
  # Reactive variables change with input (checkboxes etc.)
  variable <- reactive({input$variable})
  month <- reactive({input$month})

  # Define output based on variables (need () to make reactive)
  output$text1 <- renderText({paste0("Variable = ", variable(), ", Month = ", 
    month())})

  # Define output plot
  output$plot <- renderPlot({
    hist(airquality[airquality$Month == month(), variable()], main = "", 
    xlab = "")})
  


})
