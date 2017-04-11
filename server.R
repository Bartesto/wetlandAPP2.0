library(shiny)
library(dplyr)
library(ggplot2)
library(broom)
library(plotly)

source("global.R")

# Define server logic for app
shinyServer(function(input, output) {
  
  #Reactive data frame to test if enough data to model
  df <- reactive({
    df2model(input$wland, input$daydiff, input$thresh)
    
  })
  output$numwlands <- renderText({
    paste0("Choose one of  ", length(mychoices), " wetlands.")
  })
  
  #Model Plot
  modPlotInput <- function(){
    df <- df()
    model <- mod(df, input$mod)
    modData <- mData(df, model, input$mod)
    modname <- ifelse(input$mod == 1, "log-model", "linear-model")
    
    ggplot()+
      geom_point(data = df, aes_string(x = 'b5', y = 'depth.i'))+
      geom_line(data = modData, aes(x = X1, y = pred), col = 'red', size = 1)+
      geom_ribbon(data = modData, aes(x = X1, ymax = ub, ymin = lb ), alpha = 0.2)+
      theme_bw()+
      ggtitle(paste0(input$wland, " ", modname,
                     "  (Dd:", input$daydiff, "  Et:", input$thresh, ")"))+
      theme(plot.title = element_text(size = 13, face = "bold", hjust = 0))+
      xlab('shortwave infrared (Digital Number)')+
      ylab('Depth (m)')
    
  }
  
  output$mod <- renderPlot({
    
    #validation test and error message
    validate(
      need(length(df()[,1]) > 4, "Sorry not enough historical data points to model")
    )
    #model plot
    modPlotInput()
    
  })
  
  #Stats Output
  output$modsum <- renderTable({
    
    #validation test and error message
    validate(
      need(length(df()[,1]) > 4, "Sorry not enough historical data points to model")
    )
    
    #make table
    df <- df2model(input$wland, input$daydiff, input$thresh)
    model <- mod(df, input$mod)
    tabdf <- glance(model)
    tabdf$n <- tabdf$df + tabdf$df.residual
    tabdf},include.rownames = FALSE)
  
  #Predictions Plot
  predPlotInput <- function(){
    df <- df2model(input$wland, input$daydiff, input$thresh)
    model <- mod(df, input$mod)
    hDepth.i <- dfpredhist(input$wland)
    b5.i <- dfpredb5(input$wland)
    b5modelled <- pData(b5.i, model, input$mod)
    modname <- ifelse(input$mod == 1, "log-model", "linear-model")
    
    dfplot60 <- segmentdf(b5modelled, 1)
    dfplot70 <- segmentdf(b5modelled, 2)
    dfplot80 <- segmentdf(b5modelled, 3)
    
    p <- ggplot(dfplot60, aes(x = DATE, y = value))+
               geom_point(aes(colour = "Predicted"))+
               geom_point(data = hDepth.i, aes(colour = "Measured"), shape = 3, 
                          size = 2.5)+
               geom_line(linetype = "solid", aes(colour = "<= 60 days"))+
               geom_line(data = dfplot70, aes(colour = "61-70 days"), 
                         linetype = "dashed")+
               geom_line(data = dfplot80, aes(colour = "71-80 days"), 
                         linetype = "dotted")+
               scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
               scale_colour_manual(values = c( "#336699", "#336699", "#336699", 
                                               "red", "#336699"),
                                   guide = guide_legend(override.aes = list(
                                     linetype = c(0, 0, 1, 2, 3),
                                     shape = c(16, 3, NA, NA, NA))),
                                   labels = c('predicted','actual', '<= 60 days', 
                                              '61-70 days',
                                              '71-80 days'),
                                   name = "")+
               theme_bw()+
               ggtitle(paste0(input$wland, ' predictions ', modname,
                              "  (Dd:", input$daydiff, "  Et:", input$thresh, ")"))+
               theme(plot.title = element_text(size = 13, face = 'bold', 
                                               hjust = 0),
                     axis.text.x = element_text(angle = 90, vjust=0.5),
                     legend.position = "bottom")+
               ylab('Depth (m)')+
               xlab('Date')
   p2 <- ggplotly(p, tooltip = c("DATE", "value"))
    
   for(i in 1:5){
     for(j in 1:length(p2$x$data[[i]]$text)){
       #date and julian day
       out <- substr(p2$x$data[[i]]$text[j], 6, 16)
       date <- format(as.Date(substr(p2$x$data[[i]]$text[j], 6, 16)), " %d-%m-%Y")
       jul <- format(as.Date(substr(p2$x$data[[i]]$text[j], 6, 16)), " %j")
       info <- paste0(date, "<br>", "JULIAN:", jul)
       p2$x$data[[i]]$text[j] <- gsub(pattern = out, 
                                      replacement = info, p2$x$data[[i]]$text[j])
       #depth (m)
       out2 <- substr(p2$x$data[[i]]$text[j], 36, 40)
       din <- "Depth(m)"
       p2$x$data[[i]]$text[j] <- gsub(pattern = out2, 
                                      replacement = din, p2$x$data[[i]]$text[j])
     }
   }
   p2
    
  }

  output$pred <- renderPlotly({
    
    #validation test and error message
    validate(
      need(length(df()[,1]) > 4, "Sorry not enough historical data points to model")
    )
    
    #predictions plot
    predPlotInput()
  })

  
  ### BoM
  #Predictions Plot
  BoMmthlyPlotInput <- function(){
    Bdfmthly <- BoMdfmthly(input$wland)
    
    
    p <- ggplot(df)+
      geom_bar(aes(x = DATE, y = mm), stat = "identity", fill = "blue")+
      scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                   limits = as.Date(c("1987-01-01", "2017-01-01")),
                   expand=c(0.007,1)) +
      theme_bw() +
      labs(x = "Date", y = "Rain (mm)",
           caption = "Bureau of Meteorology")+
      ggtitle("Monthly Interpolated Rainfall")+
      theme(plot.title = element_text(size = 13, face = 'bold', hjust = 0),
            axis.text.x = element_text(angle = 90, vjust=0.5))
    p2 <- ggplotly(p, tooltip = c("DATE", "value"))
    
    # for(i in 1:5){
    #   for(j in 1:length(p2$x$data[[i]]$text)){
    #     #date and julian day
    #     out <- substr(p2$x$data[[i]]$text[j], 6, 16)
    #     date <- format(as.Date(substr(p2$x$data[[i]]$text[j], 6, 16)), " %d-%m-%Y")
    #     jul <- format(as.Date(substr(p2$x$data[[i]]$text[j], 6, 16)), " %j")
    #     info <- paste0(date, "<br>", "JULIAN:", jul)
    #     p2$x$data[[i]]$text[j] <- gsub(pattern = out, 
    #                                    replacement = info, p2$x$data[[i]]$text[j])
    #     #depth (m)
    #     out2 <- substr(p2$x$data[[i]]$text[j], 36, 40)
    #     din <- "Depth(m)"
    #     p2$x$data[[i]]$text[j] <- gsub(pattern = out2, 
    #                                    replacement = din, p2$x$data[[i]]$text[j])
    #   }
    # }
    p2
    
  }
  
  output$BoMmthly <- renderPlotly({
    
    #validation test and error message
    validate(
      need(length(df()[,1]) > 4, "Sorry not enough historical data points to model")
    )
    
    #predictions plot
    BoMmthlyPlotInput()
  })
  
  
  
  output$textpds <- renderText({
    dfpred <- dfpredb5(input$wland)
    paste0("Number of predicted depths for ", input$wland, ": ",
           length(na.omit(dfpred[,2])))
  })
  
  output$textfd <- renderText({
    dfpred <- dfpredb5(input$wland)
    paste0("First date of satellite data: ", format(head(dfpred[,1], n=1),
                                                    "%d-%m-%Y"))
  })
  
  output$textld <- renderText({
    dfpred <- dfpredb5(input$wland)
    paste0("Last date of satellite data: ", format(tail(dfpred[,1], n=1), 
                                                   "%d-%m-%Y"))
  })
  
  output$textsc <- renderText({
    dfpred <- dfpredb5(input$wland)
    paste0("Number of suitable scenes for ", input$wland, ": ",
           length(na.omit(dfpred[,2])))
  })
  
  #Data for export
  datasetInput1 <- function(){
    df <- df2model(input$wland, input$daydiff, input$thresh)
    model <- mod(df, input$mod)
    head <- csvHead(model, input$daydiff, input$thresh)
    return(head)
  }
  
  datasetInput2 <- function(){
    df <- df2model(input$wland, input$daydiff, input$thresh)
    model <- mod(df, input$mod)
    hDepth.i <- dfpredhist(input$wland)
    b5.i <- dfpredb5(input$wland)
    b5modelled <- pData(b5.i, model, input$mod)
    return(b5modelled)
  }
  
  ##Download Buttons
  #MODEL
  output$downloadModPlot <- downloadHandler(
    filename = function() {
      modname <- ifelse(input$mod == 1, "log-model-", "linear-model-")
      paste(input$wland, '-Mod-', modname, Sys.Date(), '.jpeg', sep = '') 
    },
    content = function(file) {
      ggsave(file, plot = modPlotInput(), width = 15, 
             height = 10, units = 'cm')
    }
  )

  #PREDICTIONS DATA
  output$downloadData <- downloadHandler(
    filename = function() {
      modname <- ifelse(input$mod == 1, "log-model-", "linear-model-")
      paste(input$wland, '-Pred-', modname, Sys.Date(),'.csv', sep = '') 
    },
    content = function(file) {
      write.table(datasetInput1(), file, sep = ",", row.names = FALSE, 
                  col.names = FALSE)
      write.table(datasetInput2(), file, sep = ",", row.names = FALSE, 
                  append = TRUE)
      
    }
  )
})