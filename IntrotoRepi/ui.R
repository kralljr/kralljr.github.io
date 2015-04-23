# load library
library(shiny)

# Define how application looks
shinyUI(fluidPage(
  # Add title
  titlePanel("Histogram of variables in airquality dataset"),
  # Define sidebar
  sidebarLayout(
      
    # Add inputs in dropdown menu (select)
    sidebarPanel(
      selectInput("variable", label = "Variable:", choices = list("Ozone" = "Ozone", 
          "Solar" = "Solar.R", "Wind" = "Wind", "Temp" = "Temp"), 
          selected = "Ozone"),

      selectInput("month", label = "Month:", choices = list("May" = 5, 
        "June" = 6, "July" = 7, "August" = 8, "September" = 9), selected = 5)),
    
    
    # Add outputs in main panel
    mainPanel(
      textOutput("text1"),
      plotOutput("plot"))
  
  )
))



