library(caret)
data <- read.csv("knnovunsample.csv")
char_cols <- c("sex","on_thyroxine","query_on_thyroxine","on_antithyroid_medication","sick","pregnant","thyroid_surgery","I131_treatment","query_hypothyroid","query_hyperthyroid","lithium","goitre","tumor","hypopituitary","psych","referral_source","Class")
data[char_cols] <- lapply(data[char_cols], as.factor)
data$Class <- factor(make.names(data$Class))
data <- data[ ,c("T3", "referral_source", "age", "TSH", "Class")]

set.seed(123)
train_index <- createDataPartition(data$Class, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
actual_classes <- test_data$Class
test_data <- subset(test_data, select = -Class)
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# RF
set.seed(123)
library(randomForest)
rf_model <- train(Class ~ ., data = train_data, method = "rf", trControl = ctrl, ntree = 200, verbose=TRUE)

# For min and max values
datax <- read.csv("knnwithoutnorm.csv")
T3min <- min(datax$T3)
T3max <- max(datax$T3)
agemin <- min(datax$age)
agemax <- max(datax$age)
TSHmin <- min(datax$TSH)
TSHmax <- max(datax$TSH)

# install.packages("shiny")
library(shiny)
ui <- fluidPage(
  titlePanel("Hypothyroid Prediction App"),
  sidebarLayout(
    sidebarPanel(
      textInput("T3", "T3 (Numeric):", value = ""),
      selectInput("referral_source", "Referral Source (0 to 4):", choices = 0:4),
      textInput("age", "Age (Numeric):", value = ""),
      textInput("TSH", "TSH (Numeric):", value = ""),
      actionButton("submit", "Submit", icon("play-circle"))
    ),
    mainPanel(
      # Display prediction result
      textOutput("prediction")
    )
  )
)


server <- function(input, output) {
  observeEvent(input$submit, {
    n <- function(x, min_val, max_val) {
      (x - min_val) / (max_val - min_val)
    }
    
    user_input <- data.frame(
      T3 = as.numeric(input$T3),
      referral_source = factor(input$referral_source),
      age = as.numeric(input$age),
      TSH = as.numeric(input$TSH)
    )
    
    
    user_input$T3 <- n(user_input$T3, T3min, T3max)
    user_input$age <- n(user_input$age, agemin, agemax)
    user_input$TSH <- n(user_input$TSH, TSHmin, TSHmax)
    
    # Make prediction using the trained model
    prediction_probs <- predict(rf_model, newdata = user_input, type = "prob")
    positive_probs <- prediction_probs[, "X1"]
    predicted_class <- ifelse(positive_probs > 0.5, "Patient has hypothyroid.", "Patient does not have hypothyroid.")
    
    # Display prediction result
    output$prediction <- renderText({
      predicted_class
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)