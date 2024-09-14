# Load necessary libraries
library(shiny)
library(DT)
library(openxlsx)  # To handle Excel file reading and writing
library(janitor)   # For round_half_up function - get lukas to do his rounding magic instead later

# Function to compute pooled standard deviation
pooled_sd <- function(subgroup_sds, subgroup_ns) {
  if (length(subgroup_sds) != length(subgroup_ns)) {
    return(NA)
  }
  
  numerator <- sum((subgroup_ns - 1) * subgroup_sds^2)
  denominator <- sum(subgroup_ns - 1)
  
  if (denominator == 0) return(NA)
  
  sqrt(numerator / denominator)
}

# Define UI for the application
ui <- fluidPage(
  # Application title
  titlePanel("ANCHOR: Assessing Numerical Consistency between wHole sample and subgROups"),
  # or maybe: WAVES: Weighted Analysis and Verification of Estimated Statistics
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      
      h4("Upload and Download"),
      
      # File upload input
      fileInput("file_upload", "Upload Summary Statistics From a .xlsx File (download results to see required structure)", accept = c(".xlsx")),
      
      downloadButton("downloadResults", "Download Results"), 
      
      br(),
      br(),
      
      h4("Settings"),
      
      # Input for the number of decimal places
      numericInput("decimals", "Number of Decimal Places Reported:", value = 2, min = 0, max = 5, step = 1),
      
      # Input for the number of subgroups
      numericInput("n_groups", "Number of Subgroups:", value = 2, min = 2, step = 1),
      
      # Checkbox to enable SD comparison
      checkboxInput("compare_sds", "Compare Standard Deviations (SD)?", value = TRUE),
      
      br(),
      
      h4("Overall sample"),
      
      # Input for the overall sample mean, N, and SD
      numericInput("overall_n", "Overall Sample Size (N):", value = 32, step = 1),
      numericInput("overall_mean", "Overall Sample Mean:", value = 4.7, step = 0.1),
      
      # Conditional input for the overall SD (if SD comparison is enabled)
      conditionalPanel(
        condition = "input.compare_sds == true",
        numericInput("overall_sd", "Overall Sample Standard Deviation (SD):", value = 2.4, step = 0.1)
      ),
      
      br(),
      
      # UI to dynamically add subgroup inputs
      uiOutput("subgroup_input")
    ),
    
    # Main panel for displaying results
    mainPanel(
      DTOutput("summary_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Dynamically generate inputs for subgroups
  output$subgroup_input <- renderUI({
    n <- input$n_groups
    input_list <- lapply(1:n, function(i) {
      tagList(
        h4(paste("Subgroup", i)),
        numericInput(paste0("subgroup_n_", i), paste("Sample Size (N) for Subgroup", i), value = 16, step = 1),
        numericInput(paste0("subgroup_mean_", i), paste("Mean for Subgroup", i), value = 4.7, step = 0.1),
        
        # Conditional input for subgroup SDs if SD comparison is enabled
        conditionalPanel(
          condition = "input.compare_sds == true",
          numericInput(paste0("subgroup_sd_", i), paste("Standard Deviation (SD) for Subgroup", i), value = 2.4, step = 0.1)
        )
      )
    })
    do.call(tagList, input_list)
  })
  
  # Helper function to compare rounded values
  is_consistent <- function(overall_value, recalculated_value, decimals) {
    rounded_overall <- janitor::round_half_up(overall_value, digits = decimals)
    rounded_recalc <- janitor::round_half_up(recalculated_value, digits = decimals)
    return(rounded_overall == rounded_recalc)
  }
  
  # Reactive expression to calculate the weighted mean of subgroups
  weighted_mean <- reactive({
    req(input$n_groups)  # Ensure the number of groups is available
    
    n <- input$n_groups
    
    # Create vectors of input IDs
    mean_inputIds <- paste0("subgroup_mean_", 1:n)
    n_inputIds <- paste0("subgroup_n_", 1:n)
    
    # Collect all subgroup means and Ns
    mean_i_list <- sapply(mean_inputIds, function(x) input[[x]])
    n_i_list <- sapply(n_inputIds, function(x) input[[x]])
    
    # Check for valid inputs
    if (any(is.null(mean_i_list)) || any(is.null(n_i_list)) || any(n_i_list == 0)) {
      return(NA)
    }
    
    # Calculate weighted mean
    weighted_sum <- sum(mean_i_list * n_i_list)
    total_weight <- sum(n_i_list)
    
    if (total_weight == 0) return(NA)
    
    # Return numeric value
    mean_value <- weighted_sum / total_weight
    mean_value
  })
  
  # Reactive expression to calculate the pooled SD of subgroups (if SD comparison is enabled)
  pooled_sd_val <- reactive({
    if (input$compare_sds) {
      n <- input$n_groups
      
      # Create vectors of input IDs
      n_inputIds <- paste0("subgroup_n_", 1:n)
      sd_inputIds <- paste0("subgroup_sd_", 1:n)
      
      # Collect subgroup Ns and SDs
      subgroup_ns <- sapply(n_inputIds, function(x) input[[x]])
      subgroup_sds <- sapply(sd_inputIds, function(x) input[[x]])
      
      # Check for valid inputs
      if (any(is.null(subgroup_sds)) || any(is.null(subgroup_ns)) || any(subgroup_ns <= 1)) {
        return(NA)
      }
      
      psd <- pooled_sd(subgroup_sds, subgroup_ns)
      if (is.na(psd)) return(NA)
      
      # Return numeric value
      psd
    } else {
      return(NA)
    }
  })
  
  # Reactive expression to generate the summary table
  res_reactive <- reactive({
    req(input$overall_n, input$overall_mean)  # Ensure necessary inputs are available
    
    overall_n <- input$overall_n
    overall_mean <- input$overall_mean
    overall_sd <- if (input$compare_sds) input$overall_sd else NA
    
    # Format overall_mean and overall_sd to ensure consistent decimal places
    decimals <- input$decimals
    overall_mean_formatted <- sprintf(paste0("%.", decimals, "f"), overall_mean)
    overall_sd_formatted <- if (!is.na(overall_sd)) sprintf(paste0("%.", decimals, "f"), overall_sd) else NA
    
    weighted_mean_val <- weighted_mean()
    pooled_sd_value <- pooled_sd_val()
    
    n <- input$n_groups
    n_inputIds <- paste0("subgroup_n_", 1:n)
    mean_inputIds <- paste0("subgroup_mean_", 1:n)
    sd_inputIds <- paste0("subgroup_sd_", 1:n)
    
    subgroup_ns <- sapply(n_inputIds, function(x) input[[x]])
    subgroup_means <- sapply(mean_inputIds, function(x) input[[x]])
    subgroup_sds <- if (input$compare_sds) sapply(sd_inputIds, function(x) input[[x]]) else rep(NA, n)
    
    # Total N from subgroups
    total_subgroup_n <- sum(subgroup_ns)
    
    # Format total_subgroup_n to zero decimal places
    total_subgroup_n_formatted <- sprintf("%.0f", total_subgroup_n)
    
    # Round overall N to zero decimal places
    overall_n_formatted <- sprintf("%.0f", overall_n)
    
    # Format weighted_mean_val and pooled_sd_value
    weighted_mean_formatted <- sprintf(paste0("%.", decimals, "f"), weighted_mean_val)
    pooled_sd_formatted <- if (!is.na(pooled_sd_value)) sprintf(paste0("%.", decimals, "f"), pooled_sd_value) else NA
    
    # Initialize lists
    statistic_list <- c("N", "Mean")
    overall_list <- c(overall_n_formatted, overall_mean_formatted)
    recalc_list <- c(total_subgroup_n_formatted, weighted_mean_formatted)
    result_list <- c(
      ifelse(
        abs(as.numeric(overall_n_formatted) - as.numeric(total_subgroup_n_formatted)) <= 1,
        "Consistent", "Inconsistent"
      ),
      ifelse(
        is_consistent(overall_mean, weighted_mean_val, decimals),
        "Consistent", "Inconsistent"
      )
    )
    
    if (input$compare_sds) {
      statistic_list <- c(statistic_list, "SD")
      overall_list <- c(overall_list, overall_sd_formatted)
      recalc_list <- c(recalc_list, pooled_sd_formatted)
      result_list <- c(result_list,
                       ifelse(
                         is_consistent(overall_sd, pooled_sd_value, decimals),
                         "Consistent", "Inconsistent"
                       )
      )
    }
    
    res <- data.frame(
      Statistic = statistic_list,
      Overall = overall_list,
      Recalculated.Combined.Subgroups = recalc_list,
      Result = result_list,
      stringsAsFactors = FALSE
    )
    res
  })
  
  # Render the summary table using the reactive expression
  output$summary_table <- renderDT({
    res <- res_reactive()
    datatable(res, options = list(dom = 't'), rownames = FALSE)
  })
  
  # Download handler for the Excel file with one table per sheet
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("ANCHOR_results_", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Collect overall inputs
      overall_inputs <- data.frame(
        Statistic = c("Overall Sample Size (N)", "Overall Sample Mean", if (input$compare_sds) "Overall Sample SD" else NULL),
        Value = c(
          sprintf("%.0f", input$overall_n),
          sprintf(paste0("%.", input$decimals, "f"), input$overall_mean),
          if (input$compare_sds) sprintf(paste0("%.", input$decimals, "f"), input$overall_sd) else NULL
        )
      )
      
      # Collect subgroup inputs
      n <- input$n_groups
      n_inputIds <- paste0("subgroup_n_", 1:n)
      mean_inputIds <- paste0("subgroup_mean_", 1:n)
      sd_inputIds <- paste0("subgroup_sd_", 1:n)
      
      # Initialize subgroup data
      subgroup_data <- data.frame(
        Subgroup = paste0("Subgroup ", 1:n),
        Sample_Size_N = sapply(n_inputIds, function(x) sprintf("%.0f", input[[x]])),
        Mean = sapply(mean_inputIds, function(x) sprintf(paste0("%.", input$decimals, "f"), input[[x]]))
      )
      
      # Add SD column if compare_sds is TRUE
      if (input$compare_sds) {
        subgroup_data$SD <- sapply(sd_inputIds, function(x) sprintf(paste0("%.", input$decimals, "f"), input[[x]]))
      }
      
      # Prepare the data frames
      res <- res_reactive()
      
      # Create a new Excel workbook
      wb <- createWorkbook()
      
      # Add worksheets
      addWorksheet(wb, "Overall Inputs")
      addWorksheet(wb, "Subgroup Inputs")
      addWorksheet(wb, "ANCHOR Results")
      
      # Write data to worksheets
      writeData(wb, "Overall Inputs", overall_inputs)
      writeData(wb, "Subgroup Inputs", subgroup_data)
      writeData(wb, "ANCHOR Results", res)
      
      # Save the workbook to the file
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Observe file upload and update inputs
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    # Read the uploaded Excel file
    uploaded_file <- input$file_upload$datapath
    
    # Read data from the first two sheets
    overall_inputs <- read.xlsx(uploaded_file, sheet = 1)
    subgroup_data <- read.xlsx(uploaded_file, sheet = 2)
    
    # Update overall inputs
    updateNumericInput(session, "overall_n", value = as.numeric(overall_inputs$Value[overall_inputs$Statistic == "Overall Sample Size (N)"]))
    updateNumericInput(session, "overall_mean", value = as.numeric(overall_inputs$Value[overall_inputs$Statistic == "Overall Sample Mean"]))
    
    # Check if SD comparison is enabled based on the presence of "Overall Sample SD" in overall_inputs
    sd_present <- "Overall Sample SD" %in% overall_inputs$Statistic
    updateCheckboxInput(session, "compare_sds", value = sd_present)
    
    if (sd_present) {
      updateNumericInput(session, "overall_sd", value = as.numeric(overall_inputs$Value[overall_inputs$Statistic == "Overall Sample SD"]))
    }
    
    # Update the number of subgroups
    n_subgroups <- nrow(subgroup_data)
    updateNumericInput(session, "n_groups", value = n_subgroups)
    
    # Use session$onFlushed to ensure that UI has updated before updating subgroup inputs
    session$onFlushed(function() {
      # Update subgroup inputs
      for (i in 1:n_subgroups) {
        updateNumericInput(session, paste0("subgroup_n_", i), value = as.numeric(subgroup_data$Sample_Size_N[i]))
        updateNumericInput(session, paste0("subgroup_mean_", i), value = as.numeric(subgroup_data$Mean[i]))
        if (sd_present && "SD" %in% colnames(subgroup_data)) {
          updateNumericInput(session, paste0("subgroup_sd_", i), value = as.numeric(subgroup_data$SD[i]))
        }
      }
    }, once = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
