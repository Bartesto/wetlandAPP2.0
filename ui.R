library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)


# Define UI for application
shinyUI(fluidPage(
  #theme = shinytheme("paper"),
  titlePanel(("SWWMP Modeller")),
  sidebarLayout(
    sidebarPanel(
      h3("Model a wetland's depth using USGS Landsat shortwave infrared data"),
      textOutput("numwlands"),
      selectInput("wland", "Wetland:",
                  choices = mychoices),
      helpText("Choose the number of days allowed between measured depth and 
               satellite data for the model"),
      numericInput("daydiff", "Dd - Days difference - default 10", value = 10),
      helpText("Choose margin of error for depth measurement"),
      numericInput("thresh", "Et - Error threshold (m) - default 0", value = 0, min = 0, 
                   max = 0.1, step = 0.01),
      radioButtons("mod", "Model Type:",
                   choices = list("Log Model" = 1,
                                  "Linear Model (neg values zeroed)" = 2),
                   selected = 1),
      downloadButton('downloadModPlot', 'Model Plot'),
      downloadButton('downloadData', 'Predictions Data'),
      br(),
      br(),
      h4("General Info"),
      helpText("USGS Landsat shortwave infrared data has been extracted from the 
               historical archive for each wetland available in the dropdown list 
               above. These values are then matched with depth measurements 
               obtained from field visits. A choice of either logarithmic or linear 
               model is then used to model wetland depth."),
      helpText("There are two variables available for adjustment to improve model 
               fit. 'Days difference' refers to the number of days allowed between
               a depth measurement in the field and available satellite data. 
               'Error threshold (m)' refers to allowable measurement error from 
               the field data. Adjusting these variables may improve the model 
               fit which can be guaged by the plot and model summary table. When 
               happy with the model use the 'Download' buttons to access the data.
               To be able to choose a download location you might have to alter your 
               browser settings."),
      br(),
      br(),
      br(),
      tags$img(height = 114,
               width = 285,
               src = "DPaW_logo.png")
    ),
    
    mainPanel(
      plotOutput("mod"),
      tableOutput("modsum"),
      br(),
      br(),
      plotlyOutput("pred"),
      textOutput("textpds"),
      br(),
      br(),
      h4("Data Info"),
      textOutput("textfd"),
      textOutput("textld"),
      textOutput("textsc"),
      plotlyOutput("BoMmthly")
      # br(),
      # plotlyOutput("BoMann")
    )
  )
)
)

