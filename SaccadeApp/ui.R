#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Saccade Data Analysis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose MAT File",
                multiple = TRUE, accept = NULL, width = NULL, buttonLabel = "Browse...",
                placeholder = "No file selected"
      ),
      fileInput("file2", "Choose MAT File",
                multiple = TRUE, accept = NULL, width = NULL, buttonLabel = "Browse...",
                placeholder = "No file selected"
      ),
      numericInput("start","Starting Index:",1,min=1,step=1),
      numericInput("end","Ending Index:",10000,min=1,step=1),
      submitButton("Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("vigPlot1"),
      plotOutput("vigPlot2"),
      plotOutput("saccPlot1")
    )
  )
))
