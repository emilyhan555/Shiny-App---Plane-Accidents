ui <- fluidPage(
  titlePanel("USA Plane Accident Visualizer (1974-2019)"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year", label = "Timeline:", 
                  min = min(as.numeric(substr(planeAccident3.df$Event.Date,1,4))),
                  max = max(as.numeric(substr(planeAccident3.df$Event.Date,1,4))),
                  value = c(1974,2019)),
      helpText("Change the time range to see how many accidents occurred during that period."),
      hr(),
      
      selectInput(inputId = "x",label = "Cause of Air Crash(Graph 2):", 
                  choices = axis_vars,
                  selected = "Broad.Phase.of.Flight"),
      hr(),
      
      sliderInput(inputId = "bin", label = "Number of Bins(Graph 3):",
                  min = 5, max = 50,
                  value = 20),
      hr(),
      
      selectInput(inputId = "y", label = "Most Dangerous(Graph 4):",
                  choices = axis_vars2,
                  selected = "Make")
      ),
    
      
    mainPanel(
      helpText("Please select time range to see the Number of Accidents Occured during that period."),
      leafletOutput(outputId = "AccidentMap", width = "100%", height = 400),
      hr(),
      helpText("Data contains information on civil aviation accidents within the United States, 
                its territories, and in international waters."),
      br(),
      plotOutput(outputId = "barplot"),
      br(),
      plotOutput(outputId = "histplot"),
      br(),
      plotOutput(outputId = "barplot2")
      
    )
  )
)
