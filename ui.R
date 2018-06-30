library(shiny)
library(darksky)
library(leaflet)
############ start
#### data acquisition
#### web app
shinyUI(
  fluidPage(
    titlePanel( title = "UAS flight Prediction Model"),
    sidebarLayout(
      sidebarPanel(strong(h2("Menu")),
                   dateInput("dat",strong("Date:"),value= Sys.Date()),
                   leafletOutput("map"),
                   #submitButton(text="Can I Fly?")
                   actionButton(inputId="mybutton",label="Can I Fly?")
      ),
      mainPanel((strong(h2("Main Panel"))),
                h3(textOutput("dat")),
                plotOutput("myPlot", width = "100%", height="1000px")
      )
    )
  )
)

