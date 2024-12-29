install.packages("ggplot2")
install.packages("dplyr")

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("select_columns"),
      selectInput(
        "plot_type",
        "Select Plot Type",
        choices = c("Scatter Plot", "Histogram", "Boxplot")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Scatter Plot'",
        selectInput("x_col", "X-Axis", choices = NULL),
        selectInput("y_col", "Y-Axis", choices = NULL)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Histogram'",
        selectInput("hist_col", "Column for Histogram", choices = NULL)
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Boxplot'",
        selectInput("box_col", "Column for Boxplot", choices = NULL),
        selectInput("group_col", "Group by", choices = NULL, selected = NULL)
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Data", tableOutput("data")),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive expression to read the uploaded data
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Dynamically update column selection inputs
  observe({
    req(dataset())
    updateSelectInput(session, "x_col", choices = names(dataset()))
    updateSelectInput(session, "y_col", choices = names(dataset()))
    updateSelectInput(session, "hist_col", choices = names(dataset()))
    updateSelectInput(session, "box_col", choices = names(dataset()))
    updateSelectInput(session, "group_col", choices = c("None", names(dataset())))
  })
  
  # Display summary statistics
  output$summary <- renderPrint({
    req(dataset())
    summary(dataset())
  })
  
  # Display the data
  output$data <- renderTable({
    req(dataset())
    dataset()
  })
  
  # Render plots
  output$plot <- renderPlot({
    req(dataset())
    data <- dataset()
    
    if (input$plot_type == "Scatter Plot") {
      req(input$x_col, input$y_col)
      ggplot(data, aes_string(x = input$x_col, y = input$y_col)) +
        geom_point() +
        theme_minimal()
      
    } else if (input$plot_type == "Histogram") {
      req(input$hist_col)
      ggplot(data, aes_string(x = input$hist_col)) +
        geom_histogram(binwidth = 1, fill = "blue", color = "white") +
        theme_minimal()
      
    } else if (input$plot_type == "Boxplot") {
      req(input$box_col)
      p <- ggplot(data, aes_string(y = input$box_col)) +
        geom_boxplot() +
        theme_minimal()
      
      if (input$group_col != "None") {
        p <- p + aes_string(x = input$group_col)
      }
      p
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
