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
          ".csv",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          ".xlsx",
          "application/vnd.ms-excel",
          ".xls"
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
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Schedule Summary",
          # Output: Data table
          DT::dataTableOutput("summaryTable")
        ),
        tabPanel(
          "Employee Registration",
          h4("Register New Employee"),
          textInput("new_name", "Full Name:"),
          textInput("new_city", "Home City:"),
          checkboxGroupInput(
            "new_availability",
            "Availability (Days of Week):",
            choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
            inline = TRUE
          ),
          checkboxGroupInput(
            "new_schedule",
            "Preferred Schedule Block:",
            choices = c("AM", "MID", "PM"),
            inline = TRUE
          ),
          actionButton(
            "register_employee",
            "Register Employee",
            class = "btn-primary"
          ),
          hr(),
          h5("Registration Status"),
          verbatimTextOutput("registration_status")
        ),
        tabPanel(
          "Employee Roster",
          fluidRow(
            column(
              12,
              p(
                "Click on a row in the table to select an employee for editing or deletion."
              ),
              actionButton(
                "edit_employee_modal_btn",
                "Edit Selected",
                class = "btn-info",
                icon = icon("user-edit")
              ),
              actionButton(
                "delete_employee",
                "Delete Selected",
                class = "btn-danger",
                icon = icon("user-minus")
              )
            )
          ),
          hr(),
          DT::dataTableOutput("rosterTable")
        )
      )
    )
  )
)

# 3. Define the server logic
server <- function(input, output, session) {
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
      validate(
        "No valid data rows found. Please check date formats (e.g., YYYY-MM-DD)."
      )
    }

    df
  })

  # Dynamically create the city filter UI based on the uploaded data
  output$cityFilterUI <- renderUI({
    df <- raw_data()
    req(df) # Ensure data is loaded before rendering UI

    # Get unique city names and add an "All" option
    city_choices <- c("All Cities", sort(unique(df$home_city)))

    selectInput(
      "cityFilter",
      "Filter by Home City:",
      choices = city_choices,
      selected = "All Cities"
    )
  })

  # Dynamically create the date range filter UI based on the uploaded data
  output$dateRangeUI <- renderUI({
    df <- raw_data()
    req(df)

    min_date <- min(df$date, na.rm = TRUE)
    max_date <- max(df$date, na.rm = TRUE)

    dateRangeInput(
      "dateRange",
      "Filter by Date Range:",
      start = min_date,
      end = max_date,
      min = min_date,
      max = max_date
    )
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
      pivot_wider(
        names_from = schedule_block,
        values_from = count,
        values_fill = 0
      ) %>%
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

    caption_text <- if (
      is.null(input$cityFilter) || input$cityFilter == "All Cities"
    ) {
      "Summary for all cities."
    } else {
      paste("Summary for", input$cityFilter)
    }

    DT::datatable(
      summary_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left;',
        'Daily Schedule Summary: ',
        htmltools::em(caption_text)
      )
    )
  })

  # --- New Feature: Employee Registration ---

  # Define the path for the persistent roster file
  roster_file_path <- "employee_roster.csv"

  # Reactive flag to track if the initial roster has been set by a file upload
  # in a session where no roster file existed initially.
  initial_roster_imported <- reactiveVal(file.exists(roster_file_path))

  # Reactive value to store the roster.
  # It now loads from a file if it exists, otherwise it starts empty.
  employee_roster <- reactiveVal({
    if (file.exists(roster_file_path)) {
      read_csv(roster_file_path, show_col_types = FALSE)
    } else {
      tibble(
        name = character(),
        home_city = character(),
        availability_days = character(),
        preferred_schedule = character()
      )
    }
  })

  # Observer to automatically save the roster to a file whenever it's modified.
  # This is more robust than onSessionEnded, which may not execute reliably
  # when running the app locally and stopping the R process directly.
  # `ignoreInit = TRUE` prevents it from running on startup, only on changes.
  observeEvent(
    employee_roster(),
    {
      write_csv(employee_roster(), roster_file_path)
    },
    ignoreInit = TRUE
  )

  # Observer to auto-populate roster from uploaded file
  observeEvent(raw_data(), {
    req(raw_data())

    # If a roster file did not exist at startup, the first file upload will
    # define the entire roster.
    if (!initial_roster_imported()) {
      new_roster <- raw_data() %>%
        distinct(name, home_city) %>%
        mutate(availability_days = "", preferred_schedule = "") %>%
        arrange(name)

      employee_roster(new_roster)

      showNotification(
        paste(nrow(new_roster), "employees imported to create initial roster."),
        type = "message",
        duration = 5
      )

      # Set flag so subsequent uploads in this session will append, not overwrite.
      initial_roster_imported(TRUE)
    } else {
      # If a roster already exists, append only new employees from the file.
      current_roster <- employee_roster()

      new_from_file <- raw_data() %>%
        distinct(name, home_city) %>%
        # Filter out employees who are already in the roster
        filter(!name %in% current_roster$name)

      if (nrow(new_from_file) > 0) {
        new_employees <- new_from_file %>%
          mutate(availability_days = "", preferred_schedule = "")

        # Add to the roster and keep it sorted by name
        employee_roster(
          bind_rows(current_roster, new_employees) %>% arrange(name)
        )

        showNotification(
          paste(nrow(new_employees), "new employee(s) added to roster."),
          type = "message"
        )
      }
    }
  })

  # Observer for the registration button
  observeEvent(input$register_employee, {
    # Basic validation
    name_val <- trimws(input$new_name)
    city_val <- trimws(input$new_city)

    if (name_val == "" || city_val == "") {
      output$registration_status <- renderText({
        "Error: Full Name and Home City cannot be empty."
      })
      return()
    }

    # Check for duplicates
    current_roster <- employee_roster()
    if (name_val %in% current_roster$name) {
      output$registration_status <- renderText({
        paste(
          "Error: An employee named '",
          name_val,
          "' is already registered.",
          sep = ""
        )
      })
      return()
    }

    # Create new employee record
    new_employee <- tibble(
      name = name_val,
      home_city = city_val,
      availability_days = paste(input$new_availability, collapse = ", "),
      preferred_schedule = paste(input$new_schedule, collapse = ", ")
    )

    # Add to the roster
    employee_roster(bind_rows(current_roster, new_employee) %>% arrange(name))

    # Provide feedback and clear inputs
    output$registration_status <- renderText({
      paste("Successfully registered:", name_val)
    })

    updateTextInput(session, "new_name", value = "")
    updateTextInput(session, "new_city", value = "")
    updateCheckboxGroupInput(
      session,
      "new_availability",
      selected = character(0)
    )
    updateCheckboxGroupInput(session, "new_schedule", selected = character(0))
  })

  # Observer for the delete button
  observeEvent(input$delete_employee, {
    selected_row <- input$rosterTable_rows_selected
    req(selected_row) # Ensure a row is selected

    current_roster <- employee_roster()
    employee_to_remove <- current_roster$name[selected_row]

    # Filter out the selected employee
    updated_roster <- current_roster %>%
      filter(name != employee_to_remove)

    employee_roster(updated_roster)

    showNotification(
      paste("Employee '", employee_to_remove, "' has been deleted."),
      type = "warning"
    )
  })

  # --- New Feature: Employee Editing ---

  # Observer to show the edit modal when the "Edit Selected" button is clicked
  observeEvent(input$edit_employee_modal_btn, {
    selected_row_index <- input$rosterTable_rows_selected
    req(selected_row_index) # Require a row to be selected

    roster <- employee_roster()
    selected_employee <- roster[selected_row_index, ]

    # Convert comma-separated strings to vectors for checkboxGroupInput
    current_availability <- str_split(
      selected_employee$availability_days,
      ", "
    )[[1]]
    current_schedule <- str_split(selected_employee$preferred_schedule, ", ")[[
      1
    ]]

    showModal(modalDialog(
      title = "Edit Employee Information",
      textInput("edit_name", "Full Name:", value = selected_employee$name),
      textInput("edit_city", "Home City:", value = selected_employee$home_city),
      checkboxGroupInput(
        "edit_availability",
        "Availability (Days of Week):",
        choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
        selected = current_availability,
        inline = TRUE
      ),
      checkboxGroupInput(
        "edit_schedule",
        "Preferred Schedule Block:",
        choices = c("AM", "MID", "PM"),
        selected = current_schedule,
        inline = TRUE
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_changes_btn", "Save Changes", class = "btn-primary")
      )
    ))
  })

  # Observer to save the edited employee data
  observeEvent(input$save_changes_btn, {
    selected_row_index <- req(input$rosterTable_rows_selected)

    current_roster <- employee_roster()
    original_name <- current_roster$name[selected_row_index]

    # Update the roster data frame
    updated_roster <- current_roster
    updated_roster[selected_row_index, "name"] <- trimws(input$edit_name)
    updated_roster[selected_row_index, "home_city"] <- trimws(input$edit_city)
    updated_roster[selected_row_index, "availability_days"] <- paste(
      input$edit_availability,
      collapse = ", "
    )
    updated_roster[selected_row_index, "preferred_schedule"] <- paste(
      input$edit_schedule,
      collapse = ", "
    )

    employee_roster(updated_roster %>% arrange(name))

    removeModal()
    showNotification(
      paste("Employee '", original_name, "' has been updated."),
      type = "warning"
    )
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

  # Render the roster table
  output$rosterTable <- DT::renderDataTable({
    DT::datatable(
      employee_roster(),
      options = list(pageLength = 10, scrollX = TRUE),
      selection = 'single', # Allow single row selection
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left;',
        'Employee Roster and Preferences'
      )
    )
  })
}

# 4. Run the application
shinyApp(ui = ui, server = server)
