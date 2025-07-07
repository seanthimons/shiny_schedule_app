# A Shiny web app to summarize schedule data from a CSV or Excel file.
# The app takes a file with names, home cities, and schedule blocks (AM, MID, PM)
# and generates an interactive summary table of counts per day.
# It also includes employee roster management and a weekly schedule generator.

# 1. Load necessary libraries
{
library(shiny)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(readxl)
library(DT)
library(janitor)
library(lubridate)
}

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
          # *** MODIFIED: Reverted to a single dataTableOutput for simplicity ***
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
              p("Click on a row in the table to select an employee for editing or deletion."),
              actionButton("edit_employee_modal_btn", "Edit Selected", class = "btn-info", icon = icon("user-edit")),
              actionButton("delete_employee", "Delete Selected", class = "btn-danger", icon = icon("user-minus"))
            )
          ),
          hr(),
          DT::dataTableOutput("rosterTable")
        ),
        tabPanel(
          "Schedule Generator",
          h4("Weekly Schedule Generation"),
          p("Generate a 7-day schedule based on the current employee roster. Edit cells directly to add 'PTO', 'OFF', etc."),
          hr(),
          fluidRow(
            column(3, dateInput("schedule_start_date", "Select Week Start Date:", value = Sys.Date())),
            column(3, numericInput("shifts_am", "AM Shifts/Day", 2, min = 0, step = 1)),
            column(3, numericInput("shifts_mid", "MID Shifts/Day", 2, min = 0, step = 1)),
            column(3, numericInput("shifts_pm", "PM Shifts/Day", 1, min = 0, step = 1))
          ),
          fluidRow(
            column(
              3,
              actionButton("generate_schedule", "Generate New Schedule", class = "btn-success", icon = icon("calendar-alt"))
            ),
            column(
              3,
              downloadButton("downloadSchedule", "Export Schedule as CSV")
            )
          ),
          hr(),
          DT::dataTableOutput("scheduleTable")
        )
      )
    )
  )
)

# 3. Define the server logic
server <- function(input, output, session) {
  # --- Existing Logic for Data Summary ---

  raw_data <- reactive({
    req(input$file1)
    inFile <- input$file1
    df <- tryCatch({
      ext <- tools::file_ext(tolower(inFile$name))
      switch(
        ext,
        csv = read_csv(inFile$datapath, show_col_types = FALSE),
        xlsx = read_excel(inFile$datapath),
        xls = read_excel(inFile$datapath),
        validate("Invalid file type. Please upload a .csv, .xlsx, or .xls file.")
      )
    }, error = function(e) {
      validate(paste("Error reading file:", e$message))
    })
    required_cols <- c("date", "name", "home_city", "schedule_block")
    if (!all(required_cols %in% names(df))) {
      missing_cols <- setdiff(required_cols, names(df))
      validate(paste("Error: The uploaded file is missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    df <- df %>%
      mutate(date = as_date(date)) %>%
      filter(!is.na(date))
    if (nrow(df) == 0) {
      validate("No valid data rows found. Please check date formats (e.g., YYYY-MM-DD).")
    }
    df
  })

  output$cityFilterUI <- renderUI({
    df <- raw_data()
    req(df)
    city_choices <- c("All Cities", sort(unique(df$home_city)))
    selectInput("cityFilter", "Filter by Home City:", choices = city_choices, selected = "All Cities")
  })

  output$dateRangeUI <- renderUI({
    df <- raw_data()
    req(df)
    min_date <- min(df$date, na.rm = TRUE)
    max_date <- max(df$date, na.rm = TRUE)
    dateRangeInput("dateRange", "Filter by Date Range:", start = min_date, end = max_date, min = min_date, max = max_date)
  })

  # --- MODIFIED: Consolidated summary logic into a single reactive expression ---
  summary_data <- reactive({
    df <- raw_data()
    req(df, input$cityFilter, input$dateRange)

    filtered_df <- df %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])

    if (input$cityFilter != "All Cities") {
      # --- Logic for SINGLE CITY view ---
      summary_df <- filtered_df %>%
        filter(home_city == input$cityFilter) %>%
        count(date, schedule_block) %>%
        tidyr::complete(date, schedule_block, fill = list(n = 0)) %>%
        pivot_wider(names_from = schedule_block, values_from = n, values_fill = 0)
    } else {
      # --- Logic for ALL CITIES view (pivoted wide by city-shift) ---
      summary_df <- filtered_df %>%
        count(date, home_city, schedule_block) %>%
        # Arrange to ensure column order is consistent (e.g., CityA-AM, CityA-MID...)
        arrange(home_city, schedule_block) %>%
        # Unite city and shift to create new column names
        unite(city_shift, home_city, schedule_block, sep = " - ") %>%
        pivot_wider(names_from = city_shift, values_from = n, values_fill = 0)
    }

    # Ensure final table is sorted by date
    summary_df <- summary_df %>% arrange(date)

    # Conditionally add totals if the checkbox is ticked and data exists
    if (isTRUE(input$showTotals) && nrow(summary_df) > 0) {
      summary_df <- summary_df %>%
        adorn_totals(where = c("row", "col"), name = "Total")
    }
    summary_df
  })

  # Render the single summary table
  output$summaryTable <- DT::renderDataTable({
    req(summary_data())
    
    caption_text <- if (is.null(input$cityFilter) || input$cityFilter == "All Cities") {
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
        'Daily Schedule Summary: ', htmltools::em(caption_text)
      )
    )
  })

  # --- Existing Logic for Employee Roster Management ---

  roster_file_path <- "employee_roster.csv"
  initial_roster_imported <- reactiveVal(file.exists(roster_file_path))

  employee_roster <- reactiveVal({
    if (file.exists(roster_file_path)) {
      read_csv(roster_file_path, show_col_types = FALSE)
    } else {
      tibble(name = character(), home_city = character(), availability_days = character(), preferred_schedule = character())
    }
  })

  observeEvent(employee_roster(), {
    write_csv(employee_roster(), roster_file_path)
  }, ignoreInit = TRUE)

  observeEvent(raw_data(), {
    req(raw_data())
    if (!initial_roster_imported()) {
      new_roster <- raw_data() %>%
        distinct(name, home_city) %>%
        mutate(availability_days = "", preferred_schedule = "") %>%
        arrange(name)
      employee_roster(new_roster)
      showNotification(paste(nrow(new_roster), "employees imported to create initial roster."), type = "message", duration = 5)
      initial_roster_imported(TRUE)
    } else {
      current_roster <- employee_roster()
      new_from_file <- raw_data() %>%
        distinct(name, home_city) %>%
        filter(!name %in% current_roster$name)
      if (nrow(new_from_file) > 0) {
        new_employees <- new_from_file %>% mutate(availability_days = "", preferred_schedule = "")
        employee_roster(bind_rows(current_roster, new_employees) %>% arrange(name))
        showNotification(paste(nrow(new_employees), "new employee(s) added to roster."), type = "message")
      }
    }
  })

  observeEvent(input$register_employee, {
    name_val <- trimws(input$new_name)
    city_val <- trimws(input$new_city)
    if (name_val == "" || city_val == "") {
      output$registration_status <- renderText({"Error: Full Name and Home City cannot be empty."})
      return()
    }
    current_roster <- employee_roster()
    if (name_val %in% current_roster$name) {
      output$registration_status <- renderText({paste("Error: An employee named '", name_val, "' is already registered.", sep = "")})
      return()
    }
    new_employee <- tibble(name = name_val, home_city = city_val, availability_days = paste(input$new_availability, collapse = ", "), preferred_schedule = paste(input$new_schedule, collapse = ", "))
    employee_roster(bind_rows(current_roster, new_employee) %>% arrange(name))
    output$registration_status <- renderText({paste("Successfully registered:", name_val)})
    updateTextInput(session, "new_name", value = "")
    updateTextInput(session, "new_city", value = "")
    updateCheckboxGroupInput(session, "new_availability", selected = character(0))
    updateCheckboxGroupInput(session, "new_schedule", selected = character(0))
  })

  observeEvent(input$delete_employee, {
    selected_row <- input$rosterTable_rows_selected
    req(selected_row)
    current_roster <- employee_roster()
    employee_to_remove <- current_roster$name[selected_row]
    updated_roster <- current_roster %>% filter(name != employee_to_remove)
    employee_roster(updated_roster)
    showNotification(paste("Employee '", employee_to_remove, "' has been deleted."), type = "warning")
  })

  observeEvent(input$edit_employee_modal_btn, {
    selected_row_index <- input$rosterTable_rows_selected
    req(selected_row_index)
    roster <- employee_roster()
    selected_employee <- roster[selected_row_index, ]
    current_availability <- str_split(selected_employee$availability_days, ", ")[[1]]
    current_schedule <- str_split(selected_employee$preferred_schedule, ", ")[[1]]
    showModal(modalDialog(
      title = "Edit Employee Information",
      textInput("edit_name", "Full Name:", value = selected_employee$name),
      textInput("edit_city", "Home City:", value = selected_employee$home_city),
      checkboxGroupInput("edit_availability", "Availability (Days of Week):", choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), selected = current_availability, inline = TRUE),
      checkboxGroupInput("edit_schedule", "Preferred Schedule Block:", choices = c("AM", "MID", "PM"), selected = current_schedule, inline = TRUE),
      footer = tagList(modalButton("Cancel"), actionButton("save_changes_btn", "Save Changes", class = "btn-primary"))
    ))
  })

  observeEvent(input$save_changes_btn, {
    selected_row_index <- req(input$rosterTable_rows_selected)
    current_roster <- employee_roster()
    original_name <- current_roster$name[selected_row_index]
    updated_roster <- current_roster
    updated_roster[selected_row_index, "name"] <- trimws(input$edit_name)
    updated_roster[selected_row_index, "home_city"] <- trimws(input$edit_city)
    updated_roster[selected_row_index, "availability_days"] <- paste(input$edit_availability, collapse = ", ")
    updated_roster[selected_row_index, "preferred_schedule"] <- paste(input$edit_schedule, collapse = ", ")
    employee_roster(updated_roster %>% arrange(name))
    removeModal()
    showNotification(paste("Employee '", original_name, "' has been updated."), type = "warning")
  })

  output$downloadSample <- downloadHandler(
    filename = "sample_schedule_data.csv",
    content = function(file) {
      sample_data <- tibble(
        date = rep(seq(as_date("2023-11-01"), by = "day", length.out = 5), 10),
        name = sample(paste("Person", LETTERS[1:20]), 50, replace = TRUE),
        home_city = sample(c("New York", "London", "Tokyo", "Paris", "Sydney"), 50, replace = TRUE),
        schedule_block = sample(c("AM", "MID", "PM"), 50, replace = TRUE)
      ) %>% arrange(date, name)
      write_csv(sample_data, file)
    }
  )

  output$rosterTable <- DT::renderDataTable({
    DT::datatable(
      employee_roster(),
      options = list(pageLength = 10, scrollX = TRUE),
      selection = 'single',
      rownames = FALSE,
      caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;', 'Employee Roster and Preferences')
    )
  })

  # --- SERVER LOGIC FOR SCHEDULE GENERATOR ---

  schedule_data <- reactiveVal(NULL)

  observeEvent(input$generate_schedule, {
    roster <- employee_roster()
    req(input$schedule_start_date, nrow(roster) > 0)
    dates <- seq(input$schedule_start_date, by = "day", length.out = 7)
    date_colnames <- format(dates, "%a %Y-%m-%d")
    schedule <- roster %>%
      select(name, home_city) %>%
      arrange(name)
    schedule[, date_colnames] <- ""
    for (i in seq_along(date_colnames)) {
      shifts_needed <- c(rep("AM", as.integer(input$shifts_am)), rep("MID", as.integer(input$shifts_mid)), rep("PM", as.integer(input$shifts_pm)))
      num_to_assign <- min(length(shifts_needed), nrow(roster))
      if (num_to_assign <= 0) next
      assigned_employees <- sample(roster$name, size = num_to_assign, replace = FALSE)
      schedule[match(assigned_employees, schedule$name), i + 2] <- shifts_needed[1:num_to_assign]
    }
    schedule_data(schedule)
    showNotification("New schedule has been generated.", type = "message")
  })

  display_schedule <- reactive({
    req(schedule_data())
    df <- schedule_data()
    shift_counts <- df %>%
      select(-name, -home_city) %>%
      rowwise() %>%
      mutate(`Shifts Worked` = sum(c_across() %in% c("AM", "MID", "PM"))) %>%
      pull(`Shifts Worked`)
    df$`Shifts Worked` <- shift_counts
    df
  })

  output$scheduleTable <- DT::renderDataTable({
    req(display_schedule())
    df <- display_schedule()
    non_editable_cols <- c(0, 1, ncol(df) - 1)
    DT::datatable(df, editable = list(target = 'cell', disable = list(columns = non_editable_cols)), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE, caption = "Weekly Schedule - Click a cell to edit (e.g., add 'PTO').")
  })

  observeEvent(input$scheduleTable_cell_edit, {
    info <- input$scheduleTable_cell_edit
    current_data <- schedule_data()
    current_data[info$row, info$col] <- info$value
    schedule_data(current_data)
  })

  output$downloadSchedule <- downloadHandler(
    filename = function() {
      req(input$schedule_start_date)
      start_date <- input$schedule_start_date
      end_date <- start_date + 6
      paste0("Schedule_", format(start_date, "%Y-%m-%d"), "_to_", format(end_date, "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      req(display_schedule())
      long_schedule <- display_schedule() %>%
        select(-`Shifts Worked`) %>%
        pivot_longer(cols = -c(name, home_city), names_to = "date_raw", values_to = "schedule_block") %>%
        filter(schedule_block %in% c("AM", "MID", "PM")) %>%
        mutate(date = as_date(str_extract(date_raw, "\\d{4}-\\d{2}-\\d{2}"))) %>%
        select(date, name, home_city, schedule_block) %>%
        arrange(date, name)
      write_csv(long_schedule, file)
    }
  )
}

# 4. Run the application
shinyApp(ui = ui, server = server)