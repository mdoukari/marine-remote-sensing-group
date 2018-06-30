library(shiny)
library(darksky)
############ start
#### data acquisition
#### web app
shinyUI(
  fluidPage(
    titlePanel( title = "UAV Prediction Model"),
    sidebarLayout(
      sidebarPanel(strong(h2("Menu")),
        dateInput("dat",strong("Date:"),value= Sys.Date()),
        submitButton("Can I Fly?")             
      ),
      mainPanel((strong(h2("Parameters"))),
        h3(textOutput("dat")),
        plotOutput("myPlot", width = "100%", height="1000px")
      )
    )
  )
)
