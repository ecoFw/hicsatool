# Load required packages
library(shiny)
library(DT)

sample_data <- readRDS("data/hi-csa-db.rds")


for (i in seq_along(sample_data[, "Resource"])){
    x <- sample_data[i, "Resource"]
    x <- paste0("<a href='", x, "', target = '_blank'>", x, "</a>")
    sample_data[i, "Resource"] <- x
}

sample_data <- sample_data[, c("Practice", "Mitigation", "Description", "Resource")]

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Hawai‘i CSA Resource Hub"),
  sidebarLayout(
    sidebarPanel(
      ## textInput("info_text", "Enter some text:", value = "Type here"),
      ## checkboxInput("deselect_all", "Deselect All", value = FALSE),
      # Create checkboxes based on unique "Practice" values
      checkboxGroupInput(
        "practice_choices",
        "What practices are you interested in using?",
        choices = unique(sample_data$Practice),
        selected = unique(sample_data$Practice)
      ),
      actionButton("deselect", "Deselect All"),
      p("Refresh the webpage to re-select all.")
    ),
    mainPanel(
      # Display the data table without row names
      DTOutput("data_table")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  # Observer to handle "Deselect All" checkbox
   observeEvent(input$deselect, {
    updateCheckboxGroupInput(session, "practice_choices", selected = character(0))
  })
  # Reactively subset the data based on user-selected checkboxes
  output$selected <- renderText({
    paste("Selected options:", paste(input$checkboxes, collapse = ", "))
  })
  filtered_data <- reactive({
    req(input$practice_choices)  # Ensure some choices are selected
    subset(sample_data, Practice %in% input$practice_choices)
  })
  # Render the data table with clickable URLs and without row names
  output$data_table <- renderDT({
    datatable(filtered_data(), rownames = FALSE, escape = FALSE, options = list(pageLength = 10))
  })
}

# Run the Shiny app
shinyApp(ui, server)
