# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read the Obesity Dataset
obesity_data <- read.csv("Data/Obesity.csv")

# Load the trained model
model <- readRDS("model_subset_lda.rds")  # Replace with the correct path

# Define the UI
ui <- fluidPage(
  titlePanel("Obesity Prediction"),
  sidebarLayout(
    sidebarPanel(
      # Add input fields for features used in your model
      numericInput("age", "Age:", value = NULL),
      numericInput("height", "Height (in cm):", value = NULL),
      numericInput("weight", "Weight (in kg):", value = NULL),
      selectInput("gender", "Gender:", choices = c("Female", "Male")),
      selectInput("family_history_with_overweight", "Family History with Overweight:", choices = c("Yes", "No")),
      
      selectInput("FAVC", "Do you eat high caloric food frequently ?", choices = c("Yes", "No")),
      selectInput("SCC", "Do you monitor your calorie intake ?", choices = c("Yes", "No")),
      selectInput("NCP", "How many main meals do you have daily?", choices = c("Between 1 and 2", "Three", "More than three")),
      selectInput("FCVC", "Do you usually eat vegetables in your food ?", choices = c("Never", "Sometimes","Always")),
      selectInput("SMOKE", "Do you smoke ?", choices = c("Yes", "No")),
      selectInput("FAF", "How often do you have physical activities ?", choices = c("I don't", "1 or 2 days", "2 or 4 days", "4 or 5 days")),
      selectInput("CH2O", "How often do you drink water ?", choices = c("Less than a liter", "Between 1 & 2 L", "More than 2 L")),
      selectInput("TUE", "How much time do you use on devices ?", choices = c("0-2 hours", "3-5 hours", "more than 5 hours")),
      selectInput("CALC", "How often do you drink alchohol ?", choices = c("I dont drink", "Sometimes", "Frequesnttly", "Always")),
      selectInput("CALC", "Which transportation do you usually use ?", choices = c("Automobile", "Motorbike", "Bike", "Public Transportaion", "Walking")),
      
      actionButton("predict", "Predict")
    ),
    mainPanel(
      tags$label(h3("Prediction Output After 5 Years")),
      textOutput("contents"),
      tableOutput("tabledata")
    )
  )
)

# Define the server function
server <- function(input, output) {
  # Reactive function to perform prediction
  datasetInput <- reactive({
    Age <- input$age + 5
    Height <- input$height
    Weight <- input$weight
    Gender <- input$gender
    family_history_with_overweight <- input$family_history_with_overweight

    FAVC <- input$FAVC
    FCVC <- input$FCVC
    
    SMOKE <- input$SMOKE
    FAF <- input$FAF
    
    # Create a data frame for prediction
    test <- data.frame(
      Age = Age,
      Height = Height,
      Weight = Weight,
      Gender = Gender,
      family_history_with_overweight = family_history_with_overweight,
      FAVC = FAVC, 
      FCVC = FCVC, 
      SMOKE = SMOKE, 
      FAF = FAF
    )
    
    # Perform prediction
    Prediction <- predict(model, test)
    
    # Calculate probabilities
    Probability <- predict(model, test, type = "response")
    
    # Create a data frame to display results
    Output <- data.frame(
      Prediction = Prediction,
      Probability = Probability,
      Age = Age
    )
    return(Output)
  })
  
  # Display a message when the prediction button is clicked
  output$contents <- renderPrint({
    if (input$predict > 0) {
      "Calculation Completed"
    } else {
      "Our model is ready to predict"
    }
  })
  
  # Display the prediction results in a table
  output$tabledata <- renderTable({
    if (input$predict > 0) {
      datasetInput()
    }
  })
}

# Create the shiny app
shinyApp(ui = ui, server = server)
