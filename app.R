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
      p("Refresh the webpage to re-select all."),
      p(""),
      sidebarPanel(
      downloadButton("downloadData", "Download Selected Rows")
    )
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
    datatable(filtered_data(), rownames = FALSE, escape = FALSE, options = list(pageLength = 50))
  })


  ## Handle the CSV download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("selected-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
        out <- filtered_data()
out <- sample_data[1:10,]
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
