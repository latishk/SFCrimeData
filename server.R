#author  Latish Khubnani
#date: 24th Nov. 2016
 
# importing the required libraries
# leaflet creates a Leaflet map widget using htmlwidgets. 
# dplyr - very important library to work with data frames
# shiny - to make a shiny app

library(leaflet)
library(RColorBrewer)
library(lattice)
library(ggplot2)
library(dplyr)
require(lubridate)
library(shiny)
library(jsonlite)

function(input, output, session) {
  #input is the input from lements of the page
  #output is the plots, tables and data to be displayed on the html page. Basically interacts with the output 
  # elements

  getAddressFromGoogle <- function(address) {
    # gets the address string and returns the geocoordinates from the google map api
    
    url <- "http://maps.google.com/maps/api/geocode/json?address="
    url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
    z <- fromJSON(url, simplifyVector = FALSE)
    if (z$status == "OK") {
      out <- c(z$results[[1]]$geometry$location$lng,
               z$results[[1]]$geometry$location$lat)
    } else {
      out <- NA
    }
    Sys.sleep(0.3)
    out
  }
  
  # this function finds the geo coordinates of limits on the uper and lower sides of the current 'pos'ition at distance
  # given 'distance'. ul = upper latitude, uln = upper longitude, ll = lower latitude, lln = lower longitude
  getcooridantes <- function(distance, pos){
    require(NISTunits)
    ul = pos[2] + 0.5*distance/110.574
    ll = pos[2]- 0.5*distance/110.574
    
    #uln = pos[1] + 0.5*distance/(111.320*cos(NISTdegTOradian(pos[1])))
    #lln = pos[1] - 0.5*distance/(111.320*cos(NISTdegTOradian(pos[1])))
    uln = pos[1] + 0.5*distance/(111.320)
    lln = pos[1] - 0.5*distance/(111.320)
    
    return(list(upperLat = ul, lowerLat = ll , upperLon=uln, lowerLon = lln))
  }
    
  
  
  # Here we are processing the data. We need the tally (count) of crimes district wise to plot the stacked bar graph
  # reactive() creates reactive exressions, when a value changes the exression is updated. We are using this to
  # update the map when ever the user changed the year. 
  # the '%>%' is called function chaning. 
    dfsum <- sfcrime5 %>% group_by(PdDistrict,Category) %>% tally()
    a <-reactive({sfcrime5[sfcrime5$Year == input$year, ]})
    output$map <-renderLeaflet({
      yeardata = a()
      yeardata<-yeardata[sample.int(nrow(yeardata), 10000),]
      getPalette = colorRampPalette(brewer.pal(9, "Set1"))
      pop <- paste(sep = "<br/>",yeardata$Category,paste("Date:",yeardata$Date), yeardata$Descript)
      pal <- colorFactor(getPalette(39), yeardata$Category)
      leaflet(yeardata)%>%addTiles()%>%addProviderTiles("CartoDB.Positron")%>%
        setView(lat = 37.77542, lng = -122.4165, zoom = 13)%>%addCircles(lng =~lng, lat = ~lat, weight = 1,radius = 10, color=~getPalette(39), popup=~pop)%>%
        addLegend("bottomright", pal =pal, values = ~yeardata$Category ,layerId = 2,opacity = 0.8, title = "Crime Category" )
      
    })
  
    # output$districtwiseCrimeDistribution <-renderPlot({
    #   
    #   colorCount = length(unique(dfsum$Category))
    #   getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    #   ggplot(data = dfsum, aes(x = PdDistrict, y = n, label = n, fill = Category, text = paste("Count:", n))) + 
    #     geom_bar(stat="identity")+ scale_fill_manual(values = getPalette(colorCount))
    #   #gg<-ggplotly(p, tooltip = c("text", "fill"))
    #   #gg
    # })
    
    # Here we are using Plotly instead of Plot as plotly provides use with automatic hovering text and interaction 
    # with the plots. These look visually more pleasing and provide better understanding.
    output$districtwiseCrimeDistribution1 <-renderPlotly({
      
      colorCount = length(unique(dfsum$Category))
      getPalette = colorRampPalette(brewer.pal(9, "Set3"))
      plot_ly(data = dfsum, x = dfsum$PdDistrict, y = dfsum$n, type = "bar", color = ~dfsum$Category, 
              colors = getPalette(39), text = dfsum$Category)%>%group_by(Category)%>%layout(barmode 
             = "stack", legend = list(x = 100, y = 100),
             title = "Districtwise Crime Distribution", xaxis = list(title = "Police District"),
             yaxis = list(side = 'left', title = "Incidents counts", zeroline = FALSE))
    })
    
    output$info <- renderText({
      if (is.null(input$plot1_click$x)) return("")
      else {
        lvls <- unique(dfsum$PdDistrict)
        n <- lvls[round(input$plot1_click$x)]
        paste0("\nCount=", round(input$plot1_click$y))
      }})
      
      v <- reactiveValues(doPlot = FALSE)
      observeEvent(input$go, {
        # 0 will be coerced to FALSE
        # 1+ will be coerced to TRUE
        v$doPlot <- input$go
        #print(v)
      })
      output$k_nearest<-renderPlotly({
        if (v$doPlot == FALSE) return()
        
        isolate({
          out<- if(input$k > 0){k = input$k
          address = input$add
          p = getAddressFromGoogle(paste(address, ", SF "))
          #print(p)
          pos = getcooridantes(1, p)
          
          filteredData<-sfcrime5[((sfcrime5$lat < pos$upperLat)&
                                   (sfcrime5$lat > pos$lowerLat)& 
                                   (sfcrime5$lng > pos$lowerLon)& 
                                   (sfcrime5$lng < pos$upperLon)& 
                                   (sfcrime5$Year > 2010)), 
                                ]
          b<-filteredData%>% group_by(Category) %>% tally()
          colnames(b)[colnames(b)=="n"] <- "Count"
          
          b<-b[order(b$Count,decreasing=T)[1:k],]
          s = sum(b$Count)
          b<-b%>%mutate(per = 100*Count/s)
          b<-b[with(b, order(-Count)), ]
          getPalette = colorRampPalette(brewer.pal(9, "Set1"))
          
          plot_ly(data = b, x = b$Category, y = b$Count, type = "bar", text = paste(round(b$per, 2), "%"))%>%layout(title = paste0("Bar Plot of ",k," highest Incidents"),
                    xaxis = list(title = "Category"),
                    yaxis = list(side = 'left', title = 'Cumulative Counts', zeroline = FALSE))
          } else{
            
            k = 5
            address = "2300 Block of 24TH AV"
            p = getAddressFromGoogle(paste(address, ", SF "))
            print(p)
            pos = getcooridantes(2, p)
            
            filteredData<-sfcrime5[((sfcrime5$lat < pos$upperLat)&
                                     (sfcrime5$lat > pos$lowerLat)& 
                                     (sfcrime5$lng > pos$lowerLon)& 
                                     (sfcrime5$lng < pos$upperLon)& 
                                     (sfcrime5$Year > 2010)), 
                                  ]
            b<-filteredData%>% group_by(Category) %>% tally()
            colnames(b)[colnames(b)=="n"] <- "Count"
            
            b<-b[order(b$Count,decreasing=T)[1:k],]
            s = sum(b$Count)
            b<-b%>%mutate(per = 100*Count/s)
            b<-b[with(b, order(-Count)), ]
            plot_ly(data = b, x = b$Category, y = b$Count, type = "bar", text = paste(round(b$per, 2), "%"))%>%layout(title = paste0("Bar Plot of ",k," highest Incidents"),
                                                                                                                      xaxis = list(title = "Category"),
                                                                                                                      yaxis = list(side = 'left', title = 'Cumulative Counts', zeroline = FALSE))  
            
          }
          out
          
        })
        
      })
      output$k_nearest_time<-renderPlotly({
        if (v$doPlot == FALSE) return()
        
        isolate({
          out<- if(input$k > 0){k = input$k
          address = input$add
          p = getAddressFromGoogle(paste(address, ", SF "))
          pos = getcooridantes(1, p)
          filteredData<-sfcrime5[((sfcrime5$lat < pos$upperLat)&
                                   (sfcrime5$lat > pos$lowerLat)& 
                                   (sfcrime5$lng > pos$lowerLon)& 
                                   (sfcrime5$lng < pos$upperLon)& 
                                   (sfcrime5$Year > 2010)), 
                                ]
          b<-filteredData%>% group_by(Category) %>% tally()
          colnames(b)[colnames(b)=="n"] <- "Count"
          b<-b[order(b$Count,decreasing=T)[1:k],]
          print(b$Category)
          
          c<- filter(filteredData, Category %in% unique(b$Category))
          c<-c%>% group_by(Category, Hour) %>% tally()
          getPalette = colorRampPalette(brewer.pal(9, "Set1"))
          pal <- colorFactor(getPalette(k), c$Category )
          plot_ly(data =c , x = c$Hour, y = c$n, type = "scatter", mode = "lines+markers", color = ~c$Category, 
                  colors = getPalette(k))%>%layout(title = 'Distribution based on Hour and different Counts',
                                                   yaxis = list(title = "Counts"),
                                                   xaxis = list(side = 'left', title = 'Hour', zeroline = FALSE))          
          } else{
            
            k = 5
            address = "2300 Block of 24TH AV"
            p = getAddressFromGoogle(paste(address, ", SF "))
            print("sheet")
            pos = getcooridantes(1, p)
            
            filteredData<-sfcrime5[((sfcrime5$lat < pos$upperLat)&
                                      (sfcrime5$lat > pos$lowerLat)& 
                                      (sfcrime5$lng > pos$lowerLon)& 
                                      (sfcrime5$lng < pos$upperLon)& 
                                      (sfcrime5$Year > 2010)), 
                                   ]
            b<-filteredData%>% group_by(Category) %>% tally()
            colnames(b)[colnames(b)=="n"] <- "Count"
            b<-b[order(b$Count,decreasing=T)[1:k],]
            print(b$Category)
            
            c<- filter(filteredData, Category %in% unique(b$Category))
            c<-c%>% group_by(Category, Hour) %>% tally()
            getPalette = colorRampPalette(brewer.pal(9, "Set1"))
            plot_ly(data =c , x = c$Hour, y = c$n, type = "scatter", mode = "lines+markers", color = ~c$Category, 
                    colors = getPalette(k))%>%layout(title = 'Distribution based on Hour and different Counts',
                                                     yaxis = list(title = "Counts"),
                                                     xaxis = list(side = 'left', title = 'Hour', zeroline = FALSE)) 
            
          }
          out
          
        })
        
      })
    
 }
