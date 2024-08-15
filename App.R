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
                                     selectInput("classMethod", "Choose Classification Algorithm", choices = c("K-Nearest Neighbors", "Decision Trees")),
                                     numericInput("knnK", "K for KNN (if KNN selected)", value = 5),
                                     selectInput("classFeatures", "Features to Use", choices = "", selected = "", multiple = TRUE),
                                     selectInput("classTarget", "Target Variable", choices = "", selected = ""),
                                     actionButton("runKNN", "Run KNN Algorithm"),
                                     actionButton("runDT", "Run Decision Trees Algorithm")  # Add a button to run Decision Trees
                                   ),
                                   mainPanel(
                                     verbatimTextOutput("classSummary"),
                                     plotOutput("rocPlot"),
                                     plotOutput("accuracyPlot"),
                                     plotOutput("confusionMatrixPlot")
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
    
    # Read the data
    file_ext <- tools::file_ext(input$file1$name)
    
    if (file_ext == "csv") {
      df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    } else if (file_ext == "xlsx") {
      df <- readxl::read_excel(input$file1$datapath)
    }
    
    df <- df[ , sapply(df, is.numeric)]  # Use only numeric data for feature selection
    
    # Ensure target variable is selected for RFE
    if (input$featSelMethod == "Recursive Feature Elimination") {
      req(input$classTarget)
      y <- df[[input$classTarget]]  # Define the target variable
      x <- df[ , setdiff(names(df), input$classTarget)]  # Exclude the target from predictors
      
      # RFE process
      control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
      rfeResult <- rfe(x = x, y = y, sizes = c(1:input$numFeatures), rfeControl = control)
      selectedFeatures <- predictors(rfeResult)
      
    } else if (input$featSelMethod == "VarianceThreshold") {
      varThreshold <- caret::nearZeroVar(df, saveMetrics = TRUE)
      selectedFeatures <- names(varThreshold[varThreshold$nzv == FALSE,])
    }
    
    reducedData <- df[, selectedFeatures]
    output$reducedData <- renderDT({ reducedData })
    
    # Update the classification feature selection input
    updateSelectInput(session, "classFeatures", choices = selectedFeatures)
  })
  
  # Classification for KNN
  observeEvent(input$runKNN, {
    req(input$file1, input$classFeatures, input$classTarget)
    
    # Read the data
    file_ext <- tools::file_ext(input$file1$name)
    
    if (file_ext == "csv") {
      df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    } else if (file_ext == "xlsx") {
      df <- readxl::read_excel(input$file1$datapath)
    }
    
    # Prepare data for classification
    df <- df[ , c(input$classFeatures, input$classTarget)]
    df[[input$classTarget]] <- as.factor(df[[input$classTarget]])
    
    set.seed(123)  # For reproducibility
    trainIndex <- createDataPartition(df[[input$classTarget]], p = 0.8, list = FALSE)
    trainData <- df[trainIndex, ]
    testData <- df[-trainIndex, ]
    
    # Train the KNN model
    knnModel <- train(as.formula(paste(input$classTarget, "~ .")), data = trainData, method = "knn", tuneGrid = data.frame(k = input$knnK))
    
    # Predict on test data
    knnPred <- predict(knnModel, newdata = testData)
    
    # Confusion matrix
    confusion <- confusionMatrix(knnPred, testData[[input$classTarget]])
    
    output$classSummary <- renderPrint({
      confusion
    })
    
    # ROC plot (for binary classification)
    output$rocPlot <- renderPlot({
      if (length(levels(testData[[input$classTarget]])) == 2) {
        roc_curve <- roc(testData[[input$classTarget]], as.numeric(knnPred))
        plot(roc_curve)
      } else {
        print("ROC plot is only available for binary classification.")
      }
    })
    
    # Plot: Accuracy vs. K values
    output$accuracyPlot <- renderPlot({
      accuracy_results <- knnModel$results
      ggplot(accuracy_results, aes(x = k, y = Accuracy)) +
        geom_line() +
        geom_point() +
        ggtitle("Accuracy vs. K values") +
        xlab("K values") +
        ylab("Accuracy")
    })
    
    # Plot: Multiclass Confusion Matrix
    output$confusionMatrixPlot <- renderPlot({
      confusion_df <- as.data.frame(confusion$table)
      ggplot(confusion_df, aes(x = Reference, y = Prediction, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = Freq), color = "white") +
        scale_fill_gradient(low = "lightblue", high = "blue") +
        ggtitle("Confusion Matrix") +
        theme_minimal()
    })
  })
  
  # Classification for Decision Trees
  observeEvent(input$runDT, {
    req(input$file1, input$classFeatures, input$classTarget)
    
    # Read the data
    file_ext <- tools::file_ext(input$file1$name)
    
    if (file_ext == "csv") {
      df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep)
    } else if (file_ext == "xlsx") {
      df <- readxl::read_excel(input$file1$datapath)
    }
    
    # Prepare data for classification
    df <- df[ , c(input$classFeatures, input$classTarget)]
    df[[input$classTarget]] <- as.factor(df[[input$classTarget]])
    
    set.seed(123)  # For reproducibility
    trainIndex <- createDataPartition(df[[input$classTarget]], p = 0.8, list = FALSE)
    trainData <- df[trainIndex, ]
    testData <- df[-trainIndex, ]
    
    # Train the Decision Tree model
    dtModel <- train(as.formula(paste(input$classTarget, "~ .")), data = trainData, method = "rpart")
    
    # Predict on test data
    dtPred <- predict(dtModel, newdata = testData)
    
    # Confusion matrix
    confusion <- confusionMatrix(dtPred, testData[[input$classTarget]])
    
    output$classSummary <- renderPrint({
      confusion
    })
    
    # ROC plot (for binary classification)
    output$rocPlot <- renderPlot({
      if (length(levels(testData[[input$classTarget]])) == 2) {
        roc_curve <- roc(testData[[input$classTarget]], as.numeric(dtPred))
        plot(roc_curve)
      } else {
        print("ROC plot is only available for binary classification.")
      }
    })
    
    # Plot: Decision Tree Model
    output$accuracyPlot <- renderPlot({
      accuracy_results <- dtModel$results
      ggplot(accuracy_results, aes(x = 1, y = Accuracy)) +
        geom_bar(stat = "identity") +
        ggtitle("Decision Tree Accuracy") +
        xlab("Decision Tree") +
        ylab("Accuracy")
    })
    
    # Plot: Multiclass Confusion Matrix
    output$confusionMatrixPlot <- renderPlot({
      confusion_df <- as.data.frame(confusion$table)
      ggplot(confusion_df, aes(x = Reference, y = Prediction, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = Freq), color = "white") +
        scale_fill_gradient(low = "lightblue", high = "blue") +
        ggtitle("Confusion Matrix") +
        theme_minimal()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
