library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(caret)
library(e1071)
library(randomForest)
library(nnet)
library(cluster)
library(factoextra)
library(umap)
library(dplyr)
library(readxl)
library(pROC)

# UI
ui <- fluidPage(
  
  navbarPage(title = "Shiny Insights",
             theme = shinytheme("cerulean"),
             
             tabPanel("Home",
                      fluidRow(
                        column(12,
                               div(class = "jumbotron",
                                   h1("Welcome to Shiny Insights"),
                                   p("The application is designed to load tabular data and support various functionalities such as data visualization, feature selection, and machine learning classification. The project was completed as part of the 'Software Technology' course requirements."),
                                   actionButton("clickMe", "Click Me")
                               )
                        )
                      )
             ),
             
             tabPanel("Data Upload", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV or Excel File", accept = c('.csv', '.xlsx')),
                          checkboxInput("header", "Header", TRUE),
                          radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
                          selectInput("cols", "Choose the variable", choices = "", selected = " ", multiple = TRUE)
                        ),
                        mainPanel(
                          DTOutput("dataTable")
                        )
                      )
             ), 
             
             navbarMenu("Visualization",
                        tabPanel("PCA & UMAP",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("dimRedMethod", "Select Dimensionality Reduction Method", choices = c("PCA", "UMAP")),
                                     numericInput("components", "Number of Components", value = 2, min = 2, max = 3)
                                   ),
                                   mainPanel(
                                     plotOutput("dimRedPlot")
                                   )
                                 )
                        ),
                        tabPanel("Exploratory Data Analysis",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("edaPlotType", "Choose Plot Type", choices = c("Histogram", "Boxplot")),
                                     selectInput("edaCols", "Choose Variable", choices = "", selected = "", multiple = TRUE)
                                   ),
                                   mainPanel(
                                     plotOutput("edaPlot")
                                   )
                                 )
                        )
             ),
             
             navbarMenu("Machine Learning",
                        tabPanel("Feature Selection",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("featSelMethod", "Choose Feature Selection Method", choices = c("VarianceThreshold", "Recursive Feature Elimination")),
                                     numericInput("numFeatures", "Number of Features to Select", value = 5)
                                   ),
                                   mainPanel(
                                     DTOutput("reducedData")
                                   )
                                 )
                        ),
                        tabPanel("Classification",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("classMethod", "Choose Classification Algorithm", choices = c("K-Nearest Neighbors", "Support Vector Machine")),
                                     numericInput("knnK", "K for KNN (if KNN selected)", value = 5),
                                     selectInput("classFeatures", "Features to Use", choices = "", selected = "", multiple = TRUE),
                                     selectInput("classTarget", "Target Variable", choices = "", selected = "")
                                   ),
                                   mainPanel(
                                     verbatimTextOutput("classSummary"),
                                     plotOutput("rocPlot")
                                   )
                                 )
                        )
             ),
             
             tabPanel("Info", 
                      sidebarLayout(
                        sidebarPanel(
                          "Information about this project"
                        ), 
                        mainPanel(htmlOutput("text1"))
                      )
             )
  )
)

# Server
server <- function(input, output, session) {
  
  # Data upload and table display
  observe({
    req(input$file1)
    
    file_ext <- tools::file_ext(input$file1$name)
    
    if (file_ext == "csv") {
      df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    } else if (file_ext == "xlsx") {
      df <- readxl::read_excel(input$file1$datapath)
    }
    
    updateSelectInput(session, "cols", choices = names(df))
    updateSelectInput(session, "edaCols", choices = names(df))
    updateSelectInput(session, "classFeatures", choices = names(df))
    updateSelectInput(session, "classTarget", choices = names(df))
    output$dataTable <- renderDT({ df })
  })
  
  # Visualization: PCA & UMAP
  observeEvent(input$dimRedMethod, {
    req(input$file1)
    df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    df <- df[ , sapply(df, is.numeric)]
    
    if(input$dimRedMethod == "PCA") {
      pca <- prcomp(df, center = TRUE, scale. = TRUE)
      pcaData <- data.frame(pca$x[, 1:input$components])
      colnames(pcaData) <- paste0("PC", 1:input$components)
      output$dimRedPlot <- renderPlot({
        if(input$components == 2) {
          ggplot(pcaData, aes(x = PC1, y = PC2)) + geom_point()
        } else {
          scatterplot3d::scatterplot3d(pcaData, type = "h")
        }
      })
    } else if(input$dimRedMethod == "UMAP") {
      umapData <- umap(df, n_neighbors = 15, n_components = input$components)
      output$dimRedPlot <- renderPlot({
        if(input$components == 2) {
          ggplot(as.data.frame(umapData$layout), aes(x = V1, y = V2)) + geom_point()
        } else {
          scatterplot3d::scatterplot3d(as.data.frame(umapData$layout), type = "h")
        }
      })
    }
  })
  
  # EDA Plots
  observeEvent(input$edaPlotType, {
    req(input$file1)
    df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    
    output$edaPlot <- renderPlot({
      if(input$edaPlotType == "Histogram") {
        ggplot(df, aes_string(x = input$edaCols[1])) + geom_histogram()
      } else if(input$edaPlotType == "Boxplot") {
        ggplot(df, aes_string(x = input$edaCols[1], y = input$edaCols[2])) + geom_boxplot()
      }
    })
  })
  
  # Feature Selection
  observeEvent(input$featSelMethod, {
    req(input$file1)
    df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    df <- df[ , sapply(df, is.numeric)]
    
    if(input$featSelMethod == "VarianceThreshold") {
      varThreshold <- caret::nearZeroVar(df, saveMetrics = TRUE)
      selectedFeatures <- names(varThreshold[varThreshold$nzv == FALSE,])
    } else if(input$featSelMethod == "Recursive Feature Elimination") {
      control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
      rfeResult <- rfe(df, input$numFeatures, sizes = c(1:input$numFeatures), rfeControl = control)
      selectedFeatures <- predictors(rfeResult)
    }
    
    reducedData <- df[, selectedFeatures]
    output$reducedData <- renderDT({ reducedData })
  })
  
  # Classification
  # Render UI for variable selection
  output$var_select <- renderUI({
    df <- data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    selectInput("vars", "Select Numeric Variables", choices = numeric_vars, selected = numeric_vars, multiple = TRUE)
  })
  
  # Render UI for model-specific parameters
  output$model_params <- renderUI({
    if (input$model == "k-NN") {
      numericInput("k", "Number of Neighbors (k)", value = 3, min = 1)
    } else if (input$model == "SVM") {
      numericInput("svmCost", "Cost Parameter", value = 1, min = 0.01, step = 0.01)
    }
  })
  
  # Train model based on user input
  observeEvent(input$trainButton, {
    df <- data()
    selected_vars <- input$vars
    
    if (length(selected_vars) < 2) {
      showNotification("Please select at least two numeric variables.", type = "error")
      return()
    }
    
    df_subset <- df[, selected_vars, drop = FALSE]
    df$label <- as.factor(df$label)  # Ensure response variable is a factor
    
    # Split data
    set.seed(123)  # For reproducibility
    train_index <- sample(seq_len(nrow(df)), size = 0.8 * nrow(df))
    train_data <- df[train_index, ]
    test_data <- df[-train_index, ]
    
    # Process the data
    train_data_scaled <- scale(train_data[, selected_vars, drop = FALSE])
    test_data_scaled <- scale(test_data[, selected_vars, drop = FALSE])
    
    if (input$model == "k-NN") {
      # Fit k-NN model
      knn_pred <- knn(train = train_data_scaled, 
                      test = test_data_scaled, 
                      cl = train_data$label, 
                      k = input$k)
      
      # Output results
      output$modelResult <- renderPrint({
        confusionMatrix <- table(test_data$label, knn_pred)
        confusionMatrix
      })
      
    } else if (input$model == "SVM") {
      # Fit SVM model
      svm_model <- svm(label ~ ., data = train_data, cost = input$svmCost)
      
      # Predict
      svm_pred <- predict(svm_model, newdata = test_data_scaled)
      
      # Output results
      output$modelResult <- renderPrint({
        confusionMatrix <- table(test_data$label, svm_pred)
        confusionMatrix
      })
    }
  })
}
  # Run the application 
  shinyApp(ui = ui, server = server)
