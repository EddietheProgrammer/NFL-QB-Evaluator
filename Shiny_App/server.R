library(shiny)
shinyServer(function(input, output) {
  
   your_data <- read.csv("/Users/calvi/Downloads/QB_Evauluation_With_Salary.csv")
  
  selectedData <- reactive({
    your_data[, c("AAV", input$ycol)]
  })
  
  model1 <- lm(interceptions ~ AAV, data = your_data)
  model2 <- lm(Wins ~ AAV, data = your_data)
  model3 <- lm(Losses ~ AAV, data = your_data)
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData())
    
    if(input$ycol=='interceptions'){abline(model1, col = "blue", lwd = 2)}
    if(input$ycol=='Wins'){abline(model2, col = "green", lwd = 2)}
    if(input$ycol=='Losses'){abline(model3, col = "red", lwd = 2)}
    
  })
  
  output$pred1p <- renderText({if(input$ycol=='interceptions'){anova(model1)$'Pr(>F)'[1]}})
  output$pred2p <- renderText({if(input$ycol=='Wins'){anova(model2)$'Pr(>F)'[1]}})
  output$pred3p <- renderText({if(input$ycol=='Losses'){anova(model3)$'Pr(>F)'[1]}})
  
  output$pred1slope <- renderText({if(input$ycol=='interceptions'){model1[[1]][2]}})
  output$pred2slope <- renderText({if(input$ycol=='Wins'){model2[[1]][2]}})
  output$pred3slope <- renderText({if(input$ycol=='Losses'){model3[[1]][2]}})
  
  output$pred1intercept <- renderText({if(input$ycol=='interceptions'){model1[[1]][1]}})
  output$pred2intercept <- renderText({if(input$ycol=='Wins'){model2[[1]][1]}})
  output$pred3intercept <- renderText({if(input$ycol=='Losses'){model3[[1]][1]}})
  
  output$table <- renderTable({
    head(selectedData(),10)
  })
  
  
  
  
  
  
})