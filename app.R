app <- shinyApp(
  
  nfl <- read.csv("/Users/Cameron_E/ShinyApps/Project 3/pbp-2018.csv") ,
  nfl <- nfl %>% select(OffenseTeam, DefenseTeam, Down, ToGo, Yards, Formation, 
                        PlayType, PassType, IsIncomplete, RushDirection, IsFumble),
  
  ui <- (
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
    
  ),
  
  server <- (
    # Create data frame for team average statistics
    teams <- as.vector(unique(na.omit(nfl$OffenseTeam)))
    newMatrix <- matrix(0, ncol = 5, nrow = 33)
    for(i in 1:33) {
      # Offense
      temp1O <- nfl %>% filter(OffenseTeam == teams[i])
      # Rush
      temp2R <- temp1O %>% filter(PlayType == "RUSH")
      ORA <- round(mean(temp2R$Yards), 2)
      # Pass
      temp2P <- temp1O %>% filter(PlayType == "PASS")
      OPA <- round(mean(temp2P$Yards), 2)
      # Defense
      temp1D <- nfl %>% filter(DefenseTeam == teams[i])
      # Rush
      temp2R <- temp1D %>% filter(PlayType == "RUSH")
      DRA <- round(mean(temp2R$Yards), 2)
      # Pass
      temp2P <- temp1D %>% filter(PlayType == "PASS")
      DPA <- round(mean(temp2P$Yards), 2)
      
      newMatrix[i,1] <- teams[i]
      newMatrix[i,2] <- as.numeric(paste(ORA))
      newMatrix[i,3] <- as.numeric(paste(OPA))
      newMatrix[i,4] <- as.numeric(DRA)
      newMatrix[i,5] <- as.numeric(DPA)
    }
    
    # Format team average data frame
    teamAvg <- as.data.frame(newMatrix)
    teamAvg1 <- teamAvg[-c(2),]
    teamAvg1$V1 <- as.character(teamAvg1$V1)
    teamAvg1$V2 <- as.double(paste(teamAvg1$V2))
    teamAvg1$V3 <- as.double(paste(teamAvg1$V3))
    teamAvg1$V4 <- as.double(paste(teamAvg1$V4))
    teamAvg1$V5 <- as.double(paste(teamAvg1$V5))
    
    
    ############# Tab 1 #############
    
    
    
    ############# Tab 2 #############
    
    # Filter the data based on inputs
    getData <- reactive({
      teams <- input$team
      units <- input$unit
      typeOfPlays <- input$typeOfPlay
      
      if(units == 'OFFENSE') {
        newData <- nfl %>% filter(OffenseTeam == teams) %>% filter(PlayType == typeOfPlays)
      } else {
        newData <- nfl %>% filter(DefenseTeam == teams) %>% filter(PlayType == typeOfPlays)
      }
    })
    
    # Create title
    output$tab2title <- renderUI({
      h3(paste(input$team, "Success on", input$unit, "in the 2018-2019 Season", sep = " "))
    })
    
    # Create histogram
    output$tab2histYards <- renderPlot({
      football <- getData()
      # Create table
      if(input$unit == "OFFENSE") {
        h <- hist(football$Yards, breaks = input$bins, labels = TRUE, xlab = "Yards", ylab = "Number of Plays",
                  main = paste0("Histogram for yards gained by ", input$team, " on ", input$typeOfPlay, " plays"))
      } else {
        h <- hist(football$Yards, breaks = input$bins, labels = TRUE, xlab = "Yards", ylab = "Number of Plays",
                  main = paste0("Histogram for yards allowed when ", input$team, " faced a ", input$typeOfPlay, " play"))
      }
    })
    
    # Download plot
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste(input$team, input$unit, input$typeOfPlay, "histogram", "png", sep = ".")
      },
      content = function(filename) {
        png(filename)
        football <- getData()
        # Create table
        if(input$unit == "OFFENSE") {
          h <- hist(football$Yards, breaks = input$bins, labels = TRUE, xlab = "Yards", ylab = "Number of Plays",
                    main = paste0("Histogram for yards gained by ", input$team, " on ", input$typeOfPlay, " plays"))
        } else {
          h <- hist(football$Yards, breaks = input$bins, labels = TRUE, xlab = "Yards", ylab = "Number of Plays",
                    main = paste0("Histogram for yards allowed when ", input$team, " faced a ", input$typeOfPlay, " play"))
        }
        dev.off()
      }
    )
    
    # Write message about results
    output$tab2histYardsText <- renderText({
      # Use filtered data
      football <- getData()
      # If OFFENSE
      if(input$unit == "OFFENSE") {
        # Write message
        paste("The average number of yards gained on offense by", input$team, "on", input$typeOfPlay, "plays was", 
              round(mean(football$Yards, na.rm = TRUE), 2), "yards.", sep = " ")
      } else {
        # Write message
        paste("The average number of yards allowed on defense by", input$team, "on", input$typeOfPlay, "plays was", 
              round(mean(football$Yards, na.rm = TRUE), 2), "yards.", sep = " ")
      }
      
    })
    
    ############# Tab 3 #############
    
    # Filter the data
    tab3data <- reactive({
      
      if(input$tab3unit == "OFFENSE") {
        teamAvg1[, c(2,3)]
      } else {
        teamAvg1[, c(4,5)]
      }
    })
    
    # Run k-means algorithm
    clusters <- reactive({
      kmeans(tab3data(), input$clusters)
    })
    
    # Create plot from k-means algorithm
    output$tab3plot <- renderPlot({
      palette(rainbow(9))
      plot(tab3data(),
           col = clusters()$cluster, pch = 20, cex = 2,
           xlab = "Rush Yards", ylab = "Pass Yards",
           main = paste0("K-Means Clustering for NFL ", input$tab3unit, 
                         " with ", input$clusters, " Clusters"))
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    # Create textbox about clicking on plot
    output$tab3plotInfo <- renderText({
      paste0("Rush Yards = ", input$tab3plotClick$x, 
             "\nPass Yards = ", input$tab3plotClick$y)
    })
    
    # Create dendogram
    output$tab3dendo <- renderPlot({
      if(input$tab3unit == "OFFENSE") {
        hierClust <- hclust(dist(data.frame(teamAvg1$V2, teamAvg1$V3)))
        plot(hierClust, xlab = "", labels = teams[c(1,3:33)], main = "Offense Dendogram")
      } else {
        hierClust <- hclust(dist(data.frame(teamAvg1$V4, teamAvg1$V5)))
        plot(hierClust, xlab = "", labels = teams[c(1,3:33)], main = "Defense Dendogram")
      }
    })
    
    ############# Tab 4 #############
    
    # Random Forest
    output$tab4plot <- renderPlot({
      hist(rnorm(100))
    })
    
    # Simple Linear Regression
    fitLinear1 <- reactive({
      football <- getData()
      football <- football %>% filter(Down > 0) %>% select(c("Down", "ToGo", "Yards"))
      if(input$tab4Predictors == "Down") {
        fit2 <- lm(formula = Yards ~ Down, data = football)
      } else if(input$tab4Predictors == "Yards for First Down") {
        fit2 <- lm(formula = Yards ~ ToGo, data = football)
      } else {
        fit2 <- lm(formula = Yards ~ Down + ToGo, data = football)
      }
    })
    
    # Render SLR
    output$tab4summarySLR <- renderUI({
      fit <- fitLinear1()
      # Both
      if(input$tab4Predictors == "Down") {
        withMathJax(
          helpText("$$Yards = 7.363-1.140(Down)$$")
        )
      } else if(input$tab4Predictors == "Both") {
        withMathJax(
          helpText("$$Yards = 6.759-1.086(Down)+0.057(Yards To Go)$$")
        )
      } else {
        withMathJax(
          helpText("$$Yards = 4.091+0.118(Yards To Go)$$")
        )
      }
    })
    
    # Multiple Linear Regression
    fitLinear <- reactive({
      football <- getData()
      football <- football %>% filter(Down > 0) %>% select(c("Down", "ToGo", "Yards"))
      # Create the models
      if(input$tab4Predictors == "Down") {
        fit1 <- lm(formula = Yards ~ Down + I(Down^2), data = football)
      } else if(input$tab4Predictors == "Yards for First Down") {
        fit1 <- lm(formula = Yards ~ ToGo + I(ToGo^2), data = football)
      } else {
        fit1 <- lm(formula = Yards ~ Down + ToGo + I(Down^2) + I(ToGo^2) + Down:ToGo, data = football)
      }
    })
    
    # Render MLR
    output$tab4summaryMLR <- renderUI({
      fit <- fitLinear()
      # Both
      if(input$tab4Predictors == "Both") {
        withMathJax(
          helpText("$$Yards = -1.417+5.154(Down)+0.461(Yards To Go)-1.137(Down)^2\n-0.005(Yards To Go)^2-0.097((Down)*(Yards To Go))$$")
        )
      } else if(input$tab4Predictors == "Down") {
        withMathJax(
          helpText("$$Yards = 3.338+3.282(Down)-0.961(Down)^2$$")
        )
      } else {
        withMathJax(
          helpText("$$Yards = 3.076+0.369(Yards To Go)-0.013(Yards To Go)^2$$")
        )
      }
    })
    
    output$SLRpredict <- renderUI({
      fit <- fitLinear1()
      d <- input$tab4logDown
      tg <- input$tab4logYards
      # Both
      if(input$tab4Predictors == "Both") {
        p <- predict(fit, newdata = data.frame(Down = d, ToGo = tg))
      } else if(input$tab4Predictors == "Down") {
        p <- predict(fit, newdata = data.frame(Down = d))
      } else {
        p <- predict(fit, newdata = data.frame(ToGo = tg))
      }
      p <- round(p,3)
      paste0("This model predicts that you will gain ", as.character(p), " yards.")
    })
    
    output$MLRpredict <- renderUI({
      fit <- fitLinear()
      d <- input$tab4logDown
      tg <- input$tab4logYards
      # Both
      if(input$tab4Predictors == "Both") {
        p <- predict(fit, newdata = data.frame(Down = d, ToGo = tg))
      } else if(input$tab4Predictors == "Down") {
        p <- predict(fit, newdata = data.frame(Down = d))
      } else {
        p <- predict(fit, newdata = data.frame(ToGo = tg))
      }
      p <- round(p,3)
      paste0("This model predicts that you will gain ", as.character(p), " yards.")
    })
    
    ############# Tab 5 ##############
    
    # Obtain new data
    getData5 <- reactive({
      teams <- input$team5
      units <- input$unit5
      typeOfPlays <- input$typeOfPlay5
      
      if(units == 'OFFENSE') {
        newData <- nfl %>% filter(OffenseTeam == teams) %>% filter(PlayType == typeOfPlays)
      } else {
        newData <- nfl %>% filter(DefenseTeam == teams) %>% filter(PlayType == typeOfPlays)
      }
    })
    
    # Print out entire data set
    tab5data <- reactive({
      data5 <- getData5()
      # Create table
      if(input$unit5 == "OFFENSE") {
        data5 <- getData5() %>% filter(OffenseTeam == input$team5) %>% filter(PlayType == input$typeOfPlay5)
      } else {
        data5 <- getData5() %>% filter(DefenseTeam == input$team5) %>% filter(PlayType == input$typeOfPlay5)
      }
      data5
    })
    
    # Render the data table
    output$tab5table <- renderTable({
      tab5data()
    })
    
    # Downloadable CSV of data set
    output$downloadData5 <- downloadHandler(
      filename = function() {
        paste(input$team5, input$unit5, input$typeOfPlay5, "csv", sep = ".")
      },
      content = function(file) {
        write.csv(tab5data(), file, row.names = FALSE)
      }
    )
    
    })
  
  )
)