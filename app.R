library(shiny)
library(readxl)
library(dplyr)

ui <- fluidPage(
  titlePanel(title = "Statistiniai testai per minute"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      fileInput(
        inputId = "file",
        label = "Choose a file to upload:",
        accept = ".xlsx"
      ),
      br(),
      selectInput(
        inputId = "column1",
        label = "Select a column for X:",
        choices = "None",
        width = "100%"
      ),
      selectInput(
        inputId = "column2",
        label = "Select a column for Y:",
        choices = "None",
        width = "100%"
      ),
      selectInput(
        inputId = "model_type",
        label = "Select a model type:",
        choices = c("ANOVA", "Stjudento t-testas", "Kruskal-Wallis"),
        selected = "ANOVA"
      )
    ),
    mainPanel = mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Column Data",
          dataTableOutput(outputId = "data"),
          gt_output(outputId = "gt_data"),
          plotOutput(outputId = "histogram")
        ),
        tabPanel(
          title = "Model Results",
          verbatimTextOutput(outputId = "model"),
          p("This table displays the summary statistics of the selected model.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Read the uploaded file into a reactive dataframe
  uploaded_data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath)
    df
  })

  # Update the column dropdowns with the column names of the uploaded file
  observe({
    updateSelectInput(session, "column1", choices = colnames(uploaded_data()), selected = NULL)
    updateSelectInput(session, "column2", choices = colnames(uploaded_data()), selected = NULL)
  })

  # Output the selected columns data
  output$data <- renderDataTable({
    req(input$file)
    req(input$column1)
    req(input$column2)

    data.frame(X = uploaded_data()[[input$column1]], Y = uploaded_data()[[input$column2]])

  })

  # Output the selected columns data
  output$gt_data <- render_gt(

    data.frame(X2 = uploaded_data()[[input$column1]], Y2 = uploaded_data()[[input$column2]]) %>%
      tbl_summary(by = X2,
                  type = Y2 ~ "continuous") %>%
      as_gt() %>%
      tab_header(md("**Table 1. Patient Characteristics**"))
  )

  # Create a histogram of the selected column
  output$histogram <- renderPlot({
    req(input$file)
    req(input$column1)
    hist(uploaded_data()[[input$column1]], main = "Histogram", xlab = input$column1)
  })

  # Output the model results of the selected columns
  output$model <- renderPrint({
    req(input$file)
    req(input$column1)
    req(input$column2)
    if (input$model_type == "Linear Regression") {
      model <- lm(uploaded_data()[[input$column2]] ~ uploaded_data()[[input$column1]])
      summary(model)
    }

    if (input$model_type == "ANOVA") {
      model <- aov(uploaded_data()[[input$column2]] ~ uploaded_data()[[input$column1]])
      model <- summary(model)
    }

    if (input$model_type == "Kruskal-Wallis") {
      model <- kruskal.test(uploaded_data()[[input$column2]] ~ uploaded_data()[[input$column1]])
      model
    }

    return(model)


  })
}

shinyApp(ui = ui, server = server)
