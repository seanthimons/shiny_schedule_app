# A Shiny web app to summarize schedule data from a CSV or Excel file.
# The app takes a file with names, home cities, and schedule blocks (AM, MID, PM)
# and generates an interactive summary table of counts per day.

# 1. Load necessary libraries
library(shiny)
library(tidyverse)
library(readxl)
library(DT)
library(janitor)
library(lubridate)

# 2. Define the UI
ui <- fluidPage(
  # App title
  titlePanel("Daily Schedule Summary Generator"),

  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      # Input: Select a file
      fileInput(
        "file1",
        "Choose CSV or Excel File",
        multiple = FALSE,
        accept = c(
          "text/csv",
          ".csv", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          ".xlsx", "application/vnd.ms-excel", ".xls"
        )
      ),

      # Placeholder for the dynamic city filter
      uiOutput("cityFilterUI"),

      # Placeholder for the dynamic date range filter
      uiOutput("dateRangeUI"),

      # Checkbox to toggle totals
      checkboxInput("showTotals", "Show Row & Column Summaries", value = FALSE),
      # Help text
      helpText(
        "Note: The file must contain columns named 'date', 'name', 'home_city', and 'schedule_block' (e.g., AM, MID, PM)."
      ),

      # Horizontal line
      tags$hr(),

      # Download button for sample data
      downloadButton("downloadSample", "Download Sample CSV")
    ),

    # Main panel for displaying outputs
    mainPanel(
      # Output: Data table
      DT::dataTableOutput("summaryTable")
    )
  )
)

# 3. Define the server logic
server <- function(input, output) {
  # Reactive expression to read and validate the uploaded file
  raw_data <- reactive({
    # Require a file to be uploaded
    req(input$file1)

    # Get file path
    inFile <- input$file1

    # Read the file based on its extension
    df <- tryCatch(
      {
        ext <- tools::file_ext(tolower(inFile$name))
        switch(
          ext,
          csv = read_csv(inFile$datapath, show_col_types = FALSE),
          xlsx = read_excel(inFile$datapath),
          xls = read_excel(inFile$datapath),
          validate(
            "Invalid file type. Please upload a .csv, .xlsx, or .xls file."
          )
        )
      },
      error = function(e) {
        # Return a user-friendly error message
        validate(paste("Error reading file:", e$message))
      }
    )

    # --- Data Validation and Processing ---
    required_cols <- c("date", "name", "home_city", "schedule_block")
    if (!all(required_cols %in% names(df))) {
      missing_cols <- setdiff(required_cols, names(df))
      validate(paste(
        "Error: The uploaded file is missing required columns:",
        paste(missing_cols, collapse = ", ")
      ))
    }

    # --- Type Conversion and Final Validation ---
    df <- df %>%
      mutate(date = as_date(date)) %>%
      filter(!is.na(date)) # Remove rows where date could not be parsed

    if (nrow(df) == 0) {
      validate("No valid data rows found. Please check date formats (e.g., YYYY-MM-DD).")
    }

    df
  })

  # Dynamically create the city filter UI based on the uploaded data
  output$cityFilterUI <- renderUI({
    df <- raw_data()
    req(df) # Ensure data is loaded before rendering UI

    # Get unique city names and add an "All" option
    city_choices <- c("All Cities", sort(unique(df$home_city)))

    selectInput("cityFilter", "Filter by Home City:",
                choices = city_choices,
                selected = "All Cities")
  })

  # Dynamically create the date range filter UI based on the uploaded data
  output$dateRangeUI <- renderUI({
    df <- raw_data()
    req(df)

    min_date <- min(df$date, na.rm = TRUE)
    max_date <- max(df$date, na.rm = TRUE)

    dateRangeInput("dateRange",
                   "Filter by Date Range:",
                   start = min_date,
                   end = max_date,
                   min = min_date,
                   max = max_date)
  })

  # Reactive expression to filter and summarize the data
  summary_data <- reactive({
    df <- raw_data()
    # Require all filter inputs to be available
    req(df, input$cityFilter, input$dateRange)

    # Filter data based on selections
    filtered_df <- df %>%
      filter(
        (input$cityFilter == "All Cities" | home_city == input$cityFilter),
        (date >= input$dateRange[1] & date <= input$dateRange[2])
      )

    # If no data remains after filtering, return an empty tibble.
    # This prevents errors and allows DT to show a "No data" message.
    if (nrow(filtered_df) == 0) {
      return(tibble())
    }

    # Process the data to generate the summary
    summary_df <- filtered_df %>%
      # Count occurrences for each date and schedule block
      count(date, schedule_block, name = "count") %>%
      # Ensure all schedule blocks are present as columns, even if count is 0
      tidyr::complete(date, schedule_block, fill = list(count = 0)) %>%
      # Pivot the table to have schedule blocks as columns
      pivot_wider(names_from = schedule_block, values_from = count, values_fill = 0) %>%
      # Arrange by date
      arrange(date)

    # Conditionally add totals if the checkbox is ticked and data exists
    if (isTRUE(input$showTotals) && nrow(summary_df) > 0) {
      summary_df <- summary_df %>%
        adorn_totals(where = c("row", "col"), name = "Total")
    }
    summary_df
  })

  # Render the summary table
  output$summaryTable <- DT::renderDataTable({
    # Ensure there is data to display
    req(summary_data())

    caption_text <- if (is.null(input$cityFilter) || input$cityFilter == "All Cities") {
      "Summary for all cities."
    } else {
      paste("Summary for", input$cityFilter)
    }

    DT::datatable(summary_data(),
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: left;',
                    'Daily Schedule Summary: ', htmltools::em(caption_text)
                  ))
  })

  # Handler for downloading sample data
  output$downloadSample <- downloadHandler(
    filename = "sample_schedule_data.csv",
    content = function(file) {
      sample_data <- tibble(
        date = rep(seq(as_date("2023-11-01"), by = "day", length.out = 5), 10),
        name = sample(paste("Person", LETTERS[1:20]), 50, replace = TRUE),
        home_city = sample(
          c("New York", "London", "Tokyo", "Paris", "Sydney"),
          50,
          replace = TRUE
        ),
        schedule_block = sample(c("AM", "MID", "PM"), 50, replace = TRUE)
      ) %>%
        arrange(date, name)
      write_csv(sample_data, file)
    }
  )
}

# 4. Run the application
shinyApp(ui = ui, server = server)
