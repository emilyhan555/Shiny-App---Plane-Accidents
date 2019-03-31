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
