# Project 3

library(ggplot2)

shinyUI(navbarPage("NFL 2018",

  ###########################
  # Tab 1: Information
  tabPanel("Information",
           h1("NFL 2018 Data Analysis"),
           column(6,
             h3("Data Exploration"),
               h6("1. Contains filtering ability by Team, Unit, and Play Type \n"),
               h6("2. Produces downloadable histogram based on filtered data \n"),
               h6("3. Generates summary of filter-specific results \n"),
               hr(),
             h3("Clustering"),
               h6("1. Uses summarized data by team for Offense and Defense \n"),
               h6("2. K-Means Clustering algorithm with specifiable unit and number of clusters \n"),
               h6("3. Dendogram for unit selected showing similarities among teams \n")
           ),
           column(6,
             h3("Modeling"),
               h6("1. Contains two models (Simple and Multiple Linear Regression) with Yards as response \n"),
               h6("2. Adjustable parameter selection with adjustable inputs \n"),
               h6("3. Model-generated predictions based on user input \n"),
               hr(),
             h3("The Data"),
               h6("1. Overview of the data set used for this analysis \n"),
               h6("2. Contains filtering ability by Team, Unit, and Play Type \n"),
               h6("3. Option to download data set by filter settings \n")
           )
  ),
  ###########################
  # Tab 2: Data Exploration
  tabPanel("Data Exploration",
           sidebarPanel(
             h3("Tab 2 title"),
             # Select box to choose team
             selectizeInput("team", "Choose a Team", choices = sort(unique(nfl$DefenseTeam))),
             # Radio buttons for offense or defense
             radioButtons("unit", "Choose a Unit", choices = c("OFFENSE", "DEFENSE")),
             # Radio buttons for type of play
             radioButtons("typeOfPlay", "Choose a Play Type", choices = c("PASS", "RUSH", "SCRAMBLE")),
             # Number of bins for histogram
             sliderInput("bins", "Number of Bins for Histogram",
                         min = 1, max = 20, value = 10, step = 1),
             # Download Button
             downloadButton("downloadPlot", "Download Plot as PDF")
           ),
           mainPanel(
             uiOutput("tab2title"),
             plotOutput("tab2histYards"),
             textOutput("tab2histYardsText")
           )
  ),
  
  ###########################
  # Tab 3: Clustering
  tabPanel("Clustering",
           headerPanel("NFL K-Means Clustering"),
           sidebarPanel(
             # Radio buttons for offense or defense
             radioButtons("tab3unit", "Choose a Unit", choices = c("OFFENSE", "DEFENSE")),
             numericInput("clusters", "Number of Clusters", value = 3, min = 1, max = 9)
           ),
           mainPanel(
             plotOutput("tab3plot", click = "tab3plotClick"),
             verbatimTextOutput("tab3plotInfo"),
             plotOutput("tab3dendo")
           )
  ),
  
  ###########################
  # Tab 4: Modeling
  tabPanel("Modeling",
           column(4, 
             h3("Model Specifications"),
             hr(),
             h5("Response: Yards"),
             hr(),
             radioButtons("tab4Predictors", "Choose the Predictors",
                                 choices = c("Down", "Yards for First Down", "Both")),
             hr(),
             # Down Selected
             conditionalPanel(condition = "input.tab4Predictors == 'Down'",
                              numericInput("tab4logDown", "Choose Down",
                                           min = 1, max = 4, value = 1, step = 1)),
             # Yards for First Down Selected
             conditionalPanel(condition = "input.tab4Predictors == 'Yards for First Down'",
                              numericInput("tab4logYards", "Choose Yards for First Down",
                                           min = 1, max = 40, value = 10, step = 1)),
             # Both Selected
             conditionalPanel(condition = "input.tab4Predictors == 'Both'",
                              numericInput("tab4logDown", "Choose Down",
                                           min = 1, max = 4, value = 1, step = 1),
                              numericInput("tab4logYards", "Choose Yards for First Down",
                                           min = 1, max = 40, value = 10, step = 1))
           ),
           column(8,
             h3("Simple Linear Regression"),
             uiOutput("tab4summarySLR"),
             uiOutput("SLRpredict"),
             hr(),
             h3("Multiple Linear Regression"),
             uiOutput("tab4summaryMLR"),
             uiOutput("MLRpredict")
           )
  ),
  
  ###########################
  # Tab 5: The Dataset
  tabPanel("The Dataset",
           sidebarPanel(
             h3("Tab 5 title"),
             # Select box to choose team
             selectizeInput("team5", "Choose a Team", choices = sort(unique(nfl$DefenseTeam))),
             # Radio buttons for offense or defense
             radioButtons("unit5", "Choose a Unit", choices = c("OFFENSE", "DEFENSE")),
             # Radio buttons for type of play
             radioButtons("typeOfPlay5", "Choose a Play Type", choices = c("PASS", "RUSH", "SCRAMBLE")),
             # Download button for data set
             downloadButton("downloadData5", "Download")
           ),
           mainPanel(
             h3("Main 5 Title"),
             tableOutput("tab5table")
           )
  )
)) # Close shinyUI and fluidPage
