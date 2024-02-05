library(shiny)
library(gtsummary)
library(dplyr)
library(officer)
library(shinyjs)
library(flextable)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  title = "Characteristic Table Generator",
  div(style = "text-align: center;", 
      h1("Characteristics Table Generator")),
  h5("Conceptualized by:", 
     tags$a(href = "https://www.linkedin.com/in/suhrudp/", "Suhrud Panchawagh"),
     "Designed by:",
     tags$a(href = "https://www.linkedin.com/in/pradhanhitesh/", "Hitesh Pradhan"),
     id = "bottom_text",
     style = "color: red; text-align: center;"),
  h5("Hello! Thank you for using the extension.", align = 'center'),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose a CSV file", accept = ".csv"),
      uiOutput("target_selector"),
      uiOutput("variable_selector"),
      uiOutput("exclude_selector"),
      downloadButton("download_tables", "Download Tables", disabled = TRUE)
    ),
    mainPanel(
      textOutput("file_info"),
      textOutput("target_info"),
      textOutput("variable_var_info"),
      textOutput("exclude_info"),
      textOutput("excluded_vars_info"),  # New output for excluded variables
      tableOutput("summary_table")
    )
  )
)

server <- function(input, output, session) {
  observe({
    if (!is.null(input$file) && !is.null(input$target_var) && !is.null(input$variable_var) && !is.null(input$exclude_vars)) {
      shinyjs::enable("download_tables")
    } else {
      shinyjs::disable("download_tables")
    }
  })
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  target_var_factor <- reactive({
    req(input$target_var, input$variable_var)
    df <- data()
    df[[input$target_var]] <- as.factor(df[[input$target_var]])
    
    # Handle multiple variable_var
    selected_vars <- input$variable_var
    df <- df %>%
      mutate(across(all_of(selected_vars), as.factor))
    
    df
  })
  
  output$target_selector <- renderUI({
    selectInput("target_var", "Select Categorical Grouping Variable", choices = names(data()))
  })
  
  observe({
    file <- input$file
    if (!is.null(file)) {
      output$file_info <- renderText({
        paste("You've uploaded", file$name)
      })
    }
  })
  
  observe({
    req(input$target_var)
    available_choices <- setdiff(names(data()), input$target_var)
    output$variable_selector <- renderUI({
      checkboxGroupInput("variable_var", "Select Categorical Variables", choices = available_choices)
    })
  })
  
  observe({
    output$target_info <- renderText({
      if (!is.null(input$target_var)) {
        paste("Selected Target Variable:", input$target_var)
      }
    })
  })
  
  observe({
    output$variable_var_info <- renderText({
      if (!is.null(input$variable_var)) {
        paste("Selected Categorical Variables:", paste(input$variable_var, collapse = ", "))
      }
    })
  })
  
  observe({
    req(input$target_var, input$variable_var)
    all_columns <- names(data())
    exclude_choices <- setdiff(all_columns, c(input$target_var, input$variable_var))
    output$exclude_selector <- renderUI({
      checkboxGroupInput("exclude_vars", "Exclude Variables", choices = exclude_choices)
    })
  })
  
  
  # Display the excluded variables in the UI
  output$excluded_vars_info <- renderText({
    if (!is.null(input$exclude_vars)) {
      paste("Excluded Variables:", paste(input$exclude_vars, collapse = ", "))
    } 
  })
  
  output$download_tables <- downloadHandler(
    filename = function(){
      paste0("Flextable_", Sys.Date(), Sys.time(), ".docx")
    },
    content = function(file){
      withProgress(
        message = "Creating and Downloading Document...",
        detail = "Please wait...",
        value = 0,
        {
          for (i in 1:12) {
            incProgress(1/12, detail = sprintf("Time elapsed: %d", i))
            Sys.sleep(1)
          }
          
          req(data(), input$target_var, input$variable_var, input$exclude_vars)
          
          # Transform the target variable and selected variables to factors
          df <- target_var_factor()
          
          Target <- input$target_var
          
          # Exclude selected variables
          data_excluded <- df[, setdiff(names(df), c(input$exclude_vars))]
          
          # Create a gtsummary table
          tbl <- data_excluded %>%
            tbl_summary(
              by = Target,
              type = list(where(is.numeric) ~ "continuous"),
              missing = "no",
              statistic = list(all_continuous() ~ "{mean} ({sd})")
            ) %>%
            add_p(test = list(where(is.numeric) ~ "t.test")) %>%
            add_overall() 
          
          # Convert the gtsummary table to a flextable
          flex_table <- as_flex_table(tbl)
          
          # Create a Word document
          doc <- read_docx()
          
          # Add the flextable to the Word document
          doc <- body_add_flextable(
            doc,
            value = flex_table,
          )
          fileout <- tempfile(fileext = ".docx")
          
          # Save the Word document
          print(doc, target = fileout)
          file.copy(fileout, file)
        }
      )
    }
  )
}

shinyApp(ui, server)