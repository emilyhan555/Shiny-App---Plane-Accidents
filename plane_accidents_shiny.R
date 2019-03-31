planeAccident.df <- read.csv("Plane_Accidents.csv", sep = ";")
summary(planeAccident.df)

sapply(planeAccident.df, function(x){sum(is.na(x))})
names(planeAccident.df)
planeAccident.df <- planeAccident.df[, c(-14,-31,-32)]

planeAccident.df$Total.Fatal.Injuries[is.na(planeAccident.df$Total.Fatal.Injuries)] <- 0
planeAccident.df$Total.Minor.Injuries[is.na(planeAccident.df$Total.Minor.Injuries)] <- 0 
planeAccident.df$Total.Serious.Injuries[is.na(planeAccident.df$Total.Serious.Injuries)] <- 0
planeAccident.df$Total.Uninjured[is.na(planeAccident.df$Total.Uninjured)] <- 0

planeAccident.df[planeAccident.df == 'N/A' | planeAccident.df == ""] <- NA
planeAccident.df$Latitude[planeAccident.df$Latitude == 0] <- NA
planeAccident.df$Longitude[planeAccident.df$Longitude == 0] <- NA
sapply(planeAccident.df, function(x){sum(is.na(x))})

planeAccident1.df <- planeAccident.df[, c(-9,-10,-13,-19,-20,-22)]
sapply(planeAccident1.df, function(x){sum(is.na(x))})

planeAccident1.df$Number.of.Engines[planeAccident1.df$Number.of.Engines == 0] <- NA
planeAccident2.df <- na.omit(planeAccident1.df)
sapply(planeAccident2.df, function(x){sum(is.na(x))})

planeAccident2.df$Engine.Type[planeAccident2.df$Engine.Type == 'None'] <- 'Unknown' 
planeAccident2.df$Broad.Phase.of.Flight[planeAccident2.df$Broad.Phase.of.Flight == 'UNKNOWN'] <- 'OTHER'
planeAccident2.df$Event.Date <- as.Date(planeAccident2.df$Event.Date, format = "%Y-%m-%d")

planeAccident2.df$Make <- sapply(planeAccident2.df$Make, function(x){toupper(x)})
planeAccident2.df$Make <- as.factor(planeAccident2.df$Make)

planeAccident2.df$Model <- gsub("[()&]", "", planeAccident2.df$Model)
planeAccident2.df$Model <- gsub(" |--|---|-","", planeAccident2.df$Model)
planeAccident2.df$Model <- sapply(planeAccident2.df$Model, function(x){toupper(x)})
planeAccident2.df$Model <- as.factor(planeAccident2.df$Model)

planeAccident2.df$Number.of.Engines <- as.factor(planeAccident2.df$Number.of.Engines)


planeAccident2.df$Location <- as.character(planeAccident2.df$Location)
library(stringr)
planeAccident2.df$Location <- str_to_title(planeAccident2.df$Location)
simpleCap <- function(x){
  paste(substring(x,1,str_length(x)-1), toupper(substring(x,str_length(x),str_length(x))), sep = "", collapse=" ")
}
planeAccident2.df$Location <- sapply(planeAccident2.df$Location, simpleCap)
planeAccident2.df$Location <- as.factor(planeAccident2.df$Location)
planeAccident2.df <- planeAccident2.df[planeAccident2.df$Country == "United States",]

st <- function(x){
  substr(x, str_length(x)-1, str_length(x))
}
planeAccident2.df$State <- sapply(planeAccident2.df$Location, st)
planeAccident2.df$State <- as.factor(planeAccident2.df$State)

YearMonth <- substr(planeAccident2.df$Event.Date,1,7)
planeAccident2.df$Year <- as.Date(paste(YearMonth,"-01",sep=""))


library(dplyr)
planeAccident3.df <- distinct(planeAccident2.df)

planeAccident3.df$Event.Id <- droplevels(planeAccident3.df$Event.Id)
planeAccident3.df$Investigation.Type <- droplevels(planeAccident3.df$Investigation.Type)
planeAccident3.df$Accident.Number <- droplevels(planeAccident3.df$Accident.Number)
planeAccident3.df$Location <- droplevels(planeAccident3.df$Location)
planeAccident3.df$Country <- droplevels(planeAccident3.df$Country)
planeAccident3.df$Injury.Severity <- droplevels(planeAccident3.df$Injury.Severity)
planeAccident3.df$Aircraft.Damage <- droplevels(planeAccident3.df$Aircraft.Damage)
planeAccident3.df$Make <- droplevels(planeAccident3.df$Make)
planeAccident3.df$Model <- droplevels(planeAccident3.df$Model)
planeAccident3.df$Amateur.Built <- droplevels(planeAccident3.df$Amateur.Built)
planeAccident3.df$Engine.Type <- droplevels(planeAccident3.df$Engine.Type)
planeAccident3.df$Purpose.of.Flight <- droplevels(planeAccident3.df$Purpose.of.Flight)
planeAccident3.df$Weather.Condition <- droplevels(planeAccident3.df$Weather.Condition)
planeAccident3.df$Broad.Phase.of.Flight <- droplevels(planeAccident3.df$Broad.Phase.of.Flight)
planeAccident3.df$State <- droplevels(planeAccident3.df$State)
planeAccident3.df$Report.Status <- droplevels(planeAccident3.df$Report.Status)


library(shiny)
library(ggplot2)
library(leaflet)

axis_vars <- c("Broad Phase of Flight" = "Broad.Phase.of.Flight",
               "Injury Severity" = "Injury.Severity",
               "Investigation Type" = "Investigation.Type",
               "Amateur Built" = "Amateur.Built",
               "Aircraft Damage" = "Aircraft.Damage",
               "Engine Type" = "Engine.Type",
               "Number of Engines" = "Number.of.Engines",
               "Weather Condition" = "Weather.Condition")

axis_vars2 <- c("Make" = "Make",
                "Model" = "Model",
                "U.S. State" = "State")

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


server <- function(input, output) {
  
    yr <- reactive({
      minyear <- input$year[1]
      maxyear <- input$year[2]
      
      a <- as.numeric(substr(planeAccident3.df$Event.Date,1,4)) >= minyear & as.numeric(substr(planeAccident3.df$Event.Date,1,4)) <= maxyear
      pa <- planeAccident3.df[a,]
    }) 
  
    
    output$AccidentMap <- renderLeaflet({
      map <- leaflet(yr()) %>% addTiles() %>%
              addMarkers(lng = ~Longitude, 
                         lat = ~Latitude,
                         clusterOptions = markerClusterOptions(),
                         popup = as.character(planeAccident3.df$Location)) 
      map
    })
 
    
    theme_update(plot.title = element_text(hjust = 0.5,family='', face='bold', size=16))
    theme_update(plot.subtitle = element_text(hjust = 0.5, size = 12))
    
    output$barplot <- renderPlot({                                     
        ggplot(data = yr(), aes_string(x = input$x)) +
          geom_bar(stat = "count", position = "dodge", fill = "darkorchid2")+
          geom_text(aes(label=..count..), stat='count',position=position_dodge(0.9),vjust=-0.3)+
          labs(title="Number of Accidents",
               subtitle = "Please select a factor to see the Number of Accidents Occurred")+
          xlab(names(axis_vars)[axis_vars == input$x])+
          ylab("Number of Accidents Occurred")+
          scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n"))
       })
  
    
    output$histplot <- renderPlot({
      ggplot(data = yr())+
        geom_histogram(aes(x = Year), stat = "count", bins = input$bin, binwidth = 10, fill = "dodgerblue2") +
        labs(title="Number of Accidents by Year",
             subtitle = "Please select time range to see the Number of Accidents Occurred during that period.")+
        xlab("Year")+ ylab("Number of Accidents Occurred")
    })
  
    top10 <- reactive({
      a <- planeAccident3.df[, c(input$y)]
      df <- as.data.frame(table(a))
      
    }) 
    
    output$barplot2 <- renderPlot({
      top10() %>%
        arrange(desc(Freq)) %>%
        slice(1:10) %>%
        ggplot(., aes(x=a, y=Freq))+
          geom_bar(stat='identity', fill = "orange")+
          xlab(names(axis_vars2)[axis_vars2 == input$y])+
          labs(title = paste("Top 10 ",names(axis_vars2)[axis_vars2 == input$y],"- Plane Accidents Occurred"),
              subtitle = paste("The most dangerous ",tolower(names(axis_vars2)[axis_vars2 == input$y]), "s Accidents Occurred during 1974-2019", sep = ""))
    })
    
}


shinyApp(ui = ui, server = server)
