# Load required libraries
library(shiny)
library(DT)

# Sample data for demonstration
sample_data <- data.frame(
  ID = 1:5,
  Name = c("John", "Alice", "Bob", "Emma", "Michael"),
  Age = c(25, 30, 35, 40, 45),
  Gender = c("M", "F", "M", "F", "M"),
  Score = c(80, 75, 90, 85, 88)
)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Variable Selector Data Viewer"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu for selecting a variable
      selectInput("dropdown_variable", "Select a Variable for Dropdown Menu:",
                  choices = colnames(sample_data)),
      
      # Checkbox group for selecting another variable
      checkboxGroupInput("checkbox_variable", "Select Variables for Checkbox Group:",
                         choices = colnames(sample_data),
                         selected = colnames(sample_data))
    ),
    
    # Output: Data table
    mainPanel(
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to subset data based on selected variables
  selected_data <- reactive({
    data <- sample_data
    if (!is.null(input$dropdown_variable)) {
      data <- data[, c(input$dropdown_variable, input$checkbox_variable), drop = FALSE]
    }
    data
  })
  
  # Render data table
  output$table <- renderDT({
    datatable(selected_data())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
