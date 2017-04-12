library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)


# Define UI for application
shinyUI(fluidPage(
  #theme = shinytheme("paper"),
  useShinyjs(),
  titlePanel("SWWMP Modeller"),
  sidebarLayout(
    sidebarPanel(
      h3("Model a wetland's depth using USGS Landsat shortwave infrared data"),
      textOutput("numwlands"),
      selectInput("wland", "Wetland:",
                  choices = mychoices, selected = "ALB1"),
      helpText("Choose the number of days allowed between measured depth and 
               satellite data for the model"),
      numericInput("daydiff", "DD - Days Difference - default 10", value = 10),
      helpText("Choose upper threshold"),
      numericInput("Uthresh", "UT - Upper threshold (m) - default 15", value = 15, min = 0, 
                   max = 0.1, step = 0.01),
      helpText("Choose lower threshold"),
      numericInput("Lthresh", "LT - Lower threshold (m) - default 0", value = 0, min = 0, 
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
               model is then used to model wetland depth and produce predicted depths."),
      h4("The Modelling Tab"),
      helpText("There are two variables available for adjustment to improve model 
               fit. 'Days difference' refers to the number of days allowed between
               a depth measurement in the field and available satellite data. 
               'Error threshold (m)' refers to allowable measurement error from 
               the field data. Adjusting these variables may improve the model 
               fit which can be guaged by the plot and model summary table. When
               happy with the model use the 'Download' button to access the data.
               The plots can be downloaded as well by using the 'camera' icon when 
               interacting with the plot. To be able to choose a download location 
               you might have to alter your browser settings."),
      h4("The Predictions Tab"),
      helpText("If using the APP to estimate depths in the absence of field data,
               this tab can help. By zooming all plots to a period of interest,
               the predicted hydroperiods can be examined in context with monthly 
               and annual rainfall. Rainfall measurements come from a monthly interpolated
               dataset downloaded from the Australian Bureau of Meteorology"),
      br(),
      br(),
      br(),
      tags$img(height = 114,
               width = 285,
               src = "DPaW_logo.png")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Modelling", 
                           plotOutput("mod"),
                           tableOutput("modsum"),
                           br(),
                           br(),
                           plotlyOutput("pred1"),
                           textOutput("textpds"),
                           textOutput("textfd"),
                           textOutput("textld")),
                  tabPanel("Predictions", 
                           plotlyOutput("pred2"),
                           plotlyOutput("BoMmthly"),
                           actionButton("hide1", "Hide Monthly"),
                           plotlyOutput("BoMann"),
                           actionButton("hide2", "Hide Annual"))
                  )
    )
  )
)
)