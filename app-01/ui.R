library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Well Counts by Field"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("depth",
                  "Minimum well depth (ft):",
                  min = 0,
                  max = 30e3,
                  value = 1e3),
      sliderInput("field.cutoff",
                  "Field cutoff (%)",
                  min = 0,
                  max = 100,
                  value = 1),
      dateRangeInput("tsteps",
                     "Well Completion Date Range",
                     start = "2005-01-01",
                     end = "2014-11-01",
                     min = "1985-01-01",
                     max = "2014-11-01",
                     startview = "decade")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))