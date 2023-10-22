library(shiny)
shinyUI(fluidPage(
  titlePanel('QB_Valuation Metric Vs Their Salary for the 2022 NFL Season'),
  sidebarLayout(
    sidebarPanel(
      selectInput('ycol', 'Select Gameweek, Season, Or Post Season Stat', names(USArrests)[c(1,2,4)],
                  selected=names(USArrests)[[1]]),
      h1(""),
      h5("head of dataset"),      
      tableOutput('table')      
    ),
    mainPanel(
      h5("The chart displays the position of quarterbacks (QBs) in the 2022 NFL season relative to their salaries based on the QB_Valuatin Metric"),
      h5("Source: 'NFLverse' dataset from R."),
      h5("A linear regression model is calculated for each type of QB Stat and shown below."),
      plotOutput("plot1"),
      h4("Slope"),
      textOutput("pred1slope"),
      textOutput("pred2slope"),
      textOutput("pred3slope"),
      h4("Intercept"),
      textOutput("pred1intercept"),
      textOutput("pred2intercept"),
      textOutput("pred3intercept"),
      h5("The linear model of crime explained by percent of urban population can be evaluated with the P value"),
      h4("P Value of Regression Model"),
      textOutput("pred1p"),
      textOutput("pred2p"),
      textOutput("pred3p")
      
    )
  )
))