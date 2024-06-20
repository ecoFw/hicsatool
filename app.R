# Load required packages
library(shiny)
library(DT)
library(stringr)

sample_data <- readRDS("data/hi-csa-db.rds")

for (i in seq_along(sample_data[, "Resource"])){
    x <- sample_data[i, "Resource"]
    x <- paste0("<a href='", x, "', target = '_blank'>", x, "</a>")
    sample_data[i, "Resource"] <- x
}

sample_data <- sample_data[, c("Practice", "Mitigation", "Description", "Resource")]

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Hawai‘i CSA Resource Webtool"),
  sidebarLayout(
    sidebarPanel(
      # Create checkboxes based on unique "Practice" values
      checkboxGroupInput(
        "practice_choices",
        "What CSA practices interest you?",
        choices = unique(sample_data$Practice),
        selected = unique(sample_data$Practice)
      ),
      actionButton("deselect", "Deselect All"),
      actionButton("selectall", "Select All"),
      p(""),
      downloadButton("downloadData", "Download Selected Rows"),
      p(""),
     tags$a(href = "https://ecofw.github.io", 
            ("About the webtool"), target = "_blank")
    ),
    mainPanel(
      # Display the data table without row names
      DTOutput("data_table")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  # Observer to handle "Deselect All" button
   observeEvent(input$deselect, {
    updateCheckboxGroupInput(session, "practice_choices", 
                             selected = character(0))
  })
  # Observer to handle "Select All" button
   observeEvent(input$selectall, {
    updateCheckboxGroupInput(session, "practice_choices", 
                             selected = unique(sample_data$Practice))
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
    datatable(filtered_data(), rownames = FALSE, escape = FALSE, 
              options = list(pageLength = 10, searching = FALSE))
  })


  ## Handle the CSV download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("selected-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
        out <- filtered_data()
        out[, "Resource"] <- sapply(out[, "Resource"], 
               function(x) str_extract_all(x, "(?<=\\>).*?(?=\\<)")[[1]])
        write.csv(out, file, row.names = FALSE)
      if(length(subset(sample_data, Practice %in% input$practice_choices)) == 0) {
        showModal(modalDialog(
          title = "No Rows Selected",
          "Please select rows to download.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
    }
  )
    
}

# Run the Shiny app
shinyApp(ui, server)
