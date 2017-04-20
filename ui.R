library(shiny)
library(shinythemes)
library(shinyjs)
library(plotly)


# Define UI for application
shinyUI(fluidPage(
  #theme = shinytheme("paper"),
  useShinyjs(),
  titlePanel("SWWMP Modeller"),
  h6("version 2.0"),
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
      radioButtons("dtype", "Data Type:",
                   choices = list("Standard" = 1,
                                  "Include logger data if available" = 2),
                   selected = 1),
      downloadButton('downloadData', 'Predictions Data'),
      br(),
      br(),
      h4("General Info"),
      p("USGS Landsat shortwave infrared data has been extracted from the 
        historical archive for each wetland available in the dropdown list 
        above. These values are then matched with depth measurements 
        obtained from field visits. A choice of either logarithmic or linear 
        model is then used to model wetland depth and produce predicted depths."),
      h4("The Modelling Tab"),
      p("There are five variables available for adjustment to improve model fit."),
      p(span(strong("Days difference"), style = "color:blue"), "refers to the 
        number of days allowed between a depth measurement in the field and 
        available satellite data."),
      p(span(strong("Upper threshold"), style = "color:blue"),  "and",  
        span(strong("Lower threshold"), style = "color:blue"), "limit the range 
        of depths available to model."),
      p(span(strong("Model Type"), style = "color:blue"),  "allows the choice 
        between a logarithmic or linear model"),
      p(span(strong("Data Type"), style = "color:blue"),  "allows the inclusion 
        of logger data (continuous records) if they exist for a wetland"),
      p("Adjusting these variables may improve the model fit which can be guaged 
        by the plot, model summary table and predictions plot. When happy with the 
        model, the 'Download' button can be used to access the data."),
      p("The plots can be downloaded as well by using the 'camera' icon when 
        interacting with the plot. To be able to choose a download location you 
        might have to alter your browser settings."),
      h4("The Predictions Tab"),
      p("If using the APP to estimate depths in the absence of field data, this 
        tab can help. By zooming all plots to a period of interest, the predicted 
        hydroperiods can be examined in context with monthly and annual rainfall. 
        Rainfall measurements come from a monthly interpolated dataset produced 
        by the Australian Bureau of Meteorology (BoM)"),
      tags$div(class="header", checked=NA,
               tags$p("The BoM data is available from "),
               tags$a(href="http://www.bom.gov.au/jsp/awap/rain/archive.jsp?colour=colour&map=totals&period=month&area=nat", "here")),
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
                           plotlyOutput("mod1"),
                           br(),
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