library(shiny)
library(oce)
library(gridExtra)
library(ggplot2)
library(leaflet)
######## start
#### app
shinyServer(
  function(input,output, session) {
   
   
    output$map <- renderLeaflet({
      leaflet() %>% addTiles() %>% setView(lng = 26.549723, lat = 39.091648, zoom = 13)
    })
    
    
    #Testing
    #observe({
    #  click<-input$map_marker_click
    #  if(is.null(click))
    #    return()
    #  text<-paste("Latitude ", click$lat, "Longtitude ", click$lng)
    #  text2<-paste("You've selected point ", click$id)
    #  map$clearPopups()
    #  map$showPopup( click$lat, click$lng, text)
    #  output$Click_text<-renderText({
    #    text2
    #  })
      
    #})
    # End testing
    ##### successful testing -----------------------start
    observeEvent(input$map_click,{
      ## Get the click info like had been doing
      click <- input$map_click
      latt<-click$lat
      lngg<-click$lng
      print("click")
      
      
      print(latt)
      leafletProxy('map') %>% # use the proxy to save computation
        addCircles(lng=click$lng, lat=click$lat, group='circles',
                   weight=1, radius=100, color='black', fillColor='orange',
                   fillOpacity=0.5, opacity=1)
    })
    ######## successful testing ------------------------- end
    observeEvent(input$mybutton, {
      click<-input$map_click
      #print("BUTTON")
      #print("Kalimera")
      d1<-as.character(input$dat)
      d2<-"T00:00:00+0200"
      d3<-paste0(d1,d2)
      #print(d3)
      
      
       
      x<-get_forecast_for(click$lat, click$lng,units="si", d3)
      output$myPlot <- renderPlot({
      
        #DARK_SKY_API_KEY ="9a852aa7dd07a84c0371826803ca773d"
       darksky_api_key(force = FALSE)
        
        
        
        z<-data.frame(Hour = as.character(), Temperature=as.numeric(), PrecipitationProbability = as.numeric(), CloudCover = as.numeric(), WindSpeed = as.numeric())
        z<- data.frame(Hour = x$hourly$time, Temperature=x$hourly$temperature, PrecipitationProbability = x$hourly$precipProbability, CloudCover = x$hourly$cloudCover, WindSpeed = x$hourly$windSpeed)
        for(i in 1:24) {
          if (z$WindSpeed[i]<= 0.3) {# CALM
            z$WaveHeight[i] <- 0
          } else if (z$WindSpeed[i] > 0.3 & z$WindSpeed[i] <= 1.5){# LIGHT AIR
            z$WaveHeight[i] <- 0.2/2
          } else if (z$WindSpeed[i] > 1.5 & z$WindSpeed[i] <= 3.3) {# LIGHT BREEZE
            z$WaveHeight[i] <- ((0.5 - 0.2)/2)+0.2
          } else if (z$WindSpeed[i] > 3.3 & z$WindSpeed[i] <= 5.5) {# GENTLE BREEZE
            z$WaveHeight[i] <- ((1 - 0.5)/2)+0.5
          } else if (z$WindSpeed[i] > 5.5 & z$WindSpeed[i] <= 7.9) {# MODERATE BREEZE
            z$WaveHeight[i] <- ((2 - 1)/2)+1
          } else if (z$WindSpeed[i] > 7.9 & z$WindSpeed[i] <= 10.7) {# FRESH BREEZE
            z$WaveHeight[i] <- ((3 - 2)/2)+2
          } else if (z$WindSpeed[i] > 10.7 & z$WindSpeed[i] <= 13.8) {# STRONG BREEZE
            z$WaveHeight[i] <- ((4 - 3)/2)+3
          } else if (z$WindSpeed[i] > 13.8 & z$WindSpeed[i] <= 17.1) {# HIGH WIND, MODERATE GALE, NEAR GALE
            z$WaveHeight[i] <- ((5.5 - 4)/2)+4
          } else if (z$WindSpeed[i] > 17.1 & z$WindSpeed[i] <= 20.7) {# GALE, FRESH GALE
            z$WaveHeight[i] <- ((7.5 - 5.5)/2)+5.5
          } else if (z$WindSpeed[i] > 20.7 & z$WindSpeed[i] <= 24.4) {# STRONG/SEVERE GALE
            z$WaveHeight[i] <- ((10-7.5)/2)+7.5
          } else if (z$WindSpeed[i] > 24.4 & z$WindSpeed[i] <= 28.4) {# STORM, WHOLE GALE
            z$WaveHeight[i] <- ((12.5 - 10)/2)+10
          } else if (z$WindSpeed[i] > 28.4 & z$WindSpeed[i] <= 32.6) {# VIOLENT STORM
            z$WaveHeight[i] <- ((16 - 12.5)/2)+12.5
          } else if (z$WindSpeed[i] > 32.6) {# HURRICANE FORCE
            z$WaveHeight[i] <- 17
          } else {
            z$WaveHeight[i] <- NA
          }
        }
        z$Decision <- 1
        # kati na grapsw
        for (i in 1:24) {
          t <- as.POSIXlt(paste0(d1," ",i,":00:00"), format="%Y-%m-%d %H:%M:%OS",tz="Europe/Athens")
          result = oce::sunAngle(t, 39.091648, 26.549723, useRefraction=TRUE)   
          z$Sungle[i]<- as.numeric(result[2])
        }
        #ggplot(z, aes(x= z$Hour, y = z$WaveHeight)) + geom_point() + geom_line()
        for(i in 1:24) {
          if(z$Temperature[i]<=30 & z$WindSpeed[i]<=3.3 & z$PrecipitationProbability[i]<=0.5 & z$CloudCover[i]<=0.25 & z$WaveHeight[i]<=0.5) {
            # asdasd
            z$decision[i] <- "yes"
          } else {
            z$decision[i] <- "no"
          } 
        }
        p1 <- ggplot(z, aes(x=z$Hour, y=z$Temperature)) + geom_line() + geom_point() + labs(title="Temperature",x="Hour", y = "Temperature in Celsius")
        p2 <- ggplot(z, aes(x=z$Hour, y=z$PrecipitationProbability)) + geom_line() + geom_point() + labs(title="Precipitation Probability",x="Hour", y = "Probability in %")
        p3 <- ggplot(z, aes(x=z$Hour, y=z$CloudCover)) + geom_line() + geom_point() + labs(title="Cloud Cover",x="Hour", y = "Cloud Cover in %")
        p4 <- ggplot(z, aes(x=z$Hour, y=z$WindSpeed)) + geom_line() + geom_point() + labs(title="Wind Speed",x="Hour", y = "Wind Speed in m/s")
        p5 <- ggplot(z, aes(x=z$Hour, y=z$WaveHeight)) + geom_line() + geom_point() + labs(title="Wave Height",x="Hour", y = "Wave Height in m")
        p6 <- ggplot(z, aes(x=z$Hour, y=z$Sungle)) + geom_line() + geom_point() + labs(title="Azimuth",x="Hour", y = "Azimuth in degrees")
        p7 <- ggplot(z, aes(x=z$Hour, y=z$Decision, fill=decision)) + geom_bar(stat="identity") + labs(title="Flight Time Windows",x="Hour", y = "Decision")+scale_fill_manual(values=c("#FF4136","#2ECC40"))+theme(legend.position = "none")
        
        
        grid.arrange(p7, p1, p2, p3, p4, p5, p6, ncol = 1)
      })
    })
    
   
  }
)
# ggplot(z, aes(x=rownames(z), y=z$PrecipitationProbability)) + geom_line() + geom_point()