library(shiny)

# Academic Performance Explorer - Binary and Fuzzy FCA
# Required packages: shiny, readxl, fcaR

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

ui <- fluidPage(
  titlePanel("Academic Performance Explorer (FCA / Fuzzy FCA)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Upload marks file", accept = c(".csv", ".xlsx", ".xls")),
      uiOutput("sheet_ui"),
      checkboxInput("header", "First row contains column names", TRUE),
      tags$hr(),
      uiOutput("id_ui"),
      uiOutput("subjects_ui"),
      numericInput("max_mark", "Maximum mark", value = 20, min = 1),
      numericInput("pass_mark", "Pass threshold (binary analysis)", value = 10, min = 0),
      actionButton("prepare", "Prepare selected data", class = "btn-primary"),
      tags$hr(),
      helpText("The app anonymizes identifiers in its FCA contexts. FCA results are descriptive associations, not causal conclusions.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("1. Data check",
          br(),
          verbatimTextOutput("data_status"),
          tableOutput("data_preview")
        ),
        tabPanel("2. Binary FCA",
          br(),
          p("Binary FCA represents each selected course as passed (1) or not passed (0)."),
          actionButton("run_binary", "Extract binary implication rules"),
          br(), br(),
          verbatimTextOutput("binary_status"),
          verbatimTextOutput("binary_rules")
        ),
        tabPanel("3. Fuzzy profile query",
          br(),
          p("Set a target degree and select courses. The result ranks students by how strongly they match the requested profile."),
          sliderInput("fuzzy_target", "Target degree", min = 0, max = 1, value = 0.75, step = 0.05),
          uiOutput("query_subjects_ui"),
          actionButton("run_fuzzy", "Evaluate fuzzy profile"),
          br(), br(),
          verbatimTextOutput("fuzzy_status"),
          verbatimTextOutput("fuzzy_extent")
        ),
        tabPanel("4. Interpretation guide",
          br(),
          tags$h4("How to read the results"),
          tags$ul(
            tags$li("A binary implication A -> B means that, in this cohort, every student passing all courses in A also passes all courses in B."),
            tags$li("An implication is an association within the uploaded cohort. It does not establish that one subject causes performance in another."),
            tags$li("A fuzzy extent assigns each student a degree between 0 and 1 indicating the strength of match with the requested course profile."),
            tags$li("Use results with teachers and programme coordinators before making student-support or curriculum decisions.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  raw_data <- reactive({
    req(input$data_file)
    ext <- tolower(tools::file_ext(input$data_file$name))
    if (ext == "csv") {
      read.csv(input$data_file$datapath, header = input$header, check.names = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      req(requireNamespace("readxl", quietly = TRUE))
      readxl::read_excel(input$data_file$datapath, sheet = input$sheet %||% 1, col_names = input$header)
    } else {
      stop("Please upload a CSV, XLS, or XLSX file.")
    }
  })

  output$sheet_ui <- renderUI({
    req(input$data_file)
    ext <- tolower(tools::file_ext(input$data_file$name))
    if (ext %in% c("xlsx", "xls")) {
      req(requireNamespace("readxl", quietly = TRUE))
      selectInput("sheet", "Excel sheet", choices = readxl::excel_sheets(input$data_file$datapath))
    }
  })

  output$id_ui <- renderUI({
    req(raw_data())
    selectInput("id_column", "Student identifier column", choices = c("Auto-generate" = "", names(raw_data())))
  })

  output$subjects_ui <- renderUI({
    req(raw_data())
    numeric_names <- names(raw_data())[vapply(raw_data(), is.numeric, logical(1))]
    checkboxGroupInput("subjects", "Course columns", choices = names(raw_data()), selected = numeric_names)
  })

  prepared <- eventReactive(input$prepare, {
    req(raw_data(), input$subjects)
    dat <- as.data.frame(raw_data())
    keep <- input$subjects
    marks <- as.data.frame(lapply(dat[keep], function(x) suppressWarnings(as.numeric(x))))
    names(marks) <- keep
    validate(need(all(vapply(marks, function(x) !all(is.na(x)), logical(1))), "Each selected course must contain numeric marks."))
    validate(need(!anyNA(marks), "Missing or non-numeric marks were found. Please clean the selected columns before analysis."))
    validate(need(all(marks >= 0 & marks <= input$max_mark), "Marks must lie between 0 and the selected maximum mark."))
    ids <- if (nzchar(input$id_column)) as.character(dat[[input$id_column]]) else paste0("student_", seq_len(nrow(marks)))
    validate(need(!anyDuplicated(ids), "Student identifiers must be unique."))
    rownames(marks) <- paste0("x_", seq_len(nrow(marks)))
    list(marks = marks, original_ids = ids, anonymous_ids = rownames(marks))
  })

  output$data_status <- renderText({
    if (is.null(prepared())) return("Upload a file, select the course columns, then click 'Prepare selected data'.")
    x <- prepared()
    paste("Prepared", nrow(x$marks), "students and", ncol(x$marks), "courses. Student labels in FCA analyses are anonymized as x_1, x_2, ...")
  })

  output$data_preview <- renderTable({
    req(prepared())
    head(cbind(Student = prepared()$anonymous_ids, prepared()$marks), 10)
  }, rownames = FALSE)

  binary_result <- eventReactive(input$run_binary, {
    req(prepared())
    validate(need(requireNamespace("fcaR", quietly = TRUE), "Install fcaR first: install.packages('fcaR')"))
    binary <- 1L * (as.matrix(prepared()$marks) >= input$pass_mark)
    dimnames(binary) <- dimnames(as.matrix(prepared()$marks))
    fc <- fcaR::FormalContext$new(binary)
    fc$find_implications(save_concepts = FALSE, verbose = FALSE)
    list(context = fc, binary = binary)
  })

  output$binary_status <- renderText({
    req(binary_result())
    x <- binary_result()
    paste("Binary context created with pass threshold", input$pass_mark, "/", input$max_mark,
          "(", round(input$pass_mark / input$max_mark, 2), ").")
  })

  output$binary_rules <- renderText({
    req(binary_result())
    paste(capture.output(binary_result()$context$implications), collapse = "\n")
  })

  output$query_subjects_ui <- renderUI({
    req(prepared())
    checkboxGroupInput("query_subjects", "Courses in the requested profile", choices = colnames(prepared()$marks), selected = colnames(prepared()$marks)[seq_len(min(4, ncol(prepared()$marks)))])
  })

  fuzzy_result <- eventReactive(input$run_fuzzy, {
    req(prepared(), input$query_subjects)
    validate(need(requireNamespace("fcaR", quietly = TRUE), "Install fcaR first: install.packages('fcaR')"))
    fuzzy <- as.matrix(prepared()$marks) / input$max_mark
    dimnames(fuzzy) <- dimnames(as.matrix(prepared()$marks))
    fc <- fcaR::FormalContext$new(fuzzy)
    profile <- fcaR::Set$new(attributes = fc$attributes)
    do.call(profile$assign, as.list(stats::setNames(rep(input$fuzzy_target, length(input$query_subjects)), input$query_subjects)))
    list(context = fc, profile = profile, extent = fc$extent(profile))
  })

  output$fuzzy_status <- renderText({
    req(fuzzy_result())
    paste("Target profile:", paste(input$query_subjects, "=", input$fuzzy_target, collapse = ", "),
          "(equivalent to", input$fuzzy_target * input$max_mark, "/", input$max_mark, "per selected course).")
  })

  output$fuzzy_extent <- renderText({
    req(fuzzy_result())
    paste(capture.output(fuzzy_result()$extent), collapse = "\n")
  })
}

shinyApp(ui, server)
