library(shiny)

# Create UI with side panel on the left and main panel on the right
ui <- fluidPage(
  # Title of the UI
  titlePanel("R Shiny Assessment"),
  
  # fileInput for CSV and tail value slider on sidebar
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV file"),
      br(),
      sliderInput("tail", "Tail Value", min = 0.0, max = 2.0, value = 1.1, step = 0.05),
    ),
    
    # Display the table and the number of years
    mainPanel(
      tableOutput("filled_table"),  # Output for table with results
      plotOutput("graph")
    )
  )
)



server <- function(input, output, session) {
  # Code to read the CSV file
  data <- reactiveVal()
  
  observeEvent(input$file, {
    if (!is.null(input$file)) {
      uploaded_data <- read.csv(input$file$datapath, header = TRUE)
      uploaded_data <- uploaded_data[-1, ]
      data(uploaded_data)
    }
  })
  
  # Create a reactive expression for the new table
  new_table <- reactive({
    req(data())
    claims <- gsub(",", "", data()[, 3])
    claims <- as.numeric(claims)
    
    # Handling any NA values that might have been introduced during conversion
    if (any(is.na(claims))) {
      stop("Non-numeric values found in 'claims' which cannot be converted to numeric")
    }
    
    # Determine n from the length of claims
    total_elements <- length(claims)
    n <- (sqrt(8*total_elements + 1) - 1) / 2
    
    if (n != floor(n)) {
      stop("Total elements in claims do not match a triangular number sequence")
    }
    
    # Loop to split and store claims for each year
    claims_list <- list()
    start_index <- 1
    for (i in n:1) {
      end_index <- start_index + i - 1
      claims_list[[length(claims_list) + 1]] <- claims[start_index:end_index]
      start_index <- end_index + 1
    }
    
    # Create new dataframe
    df <- do.call(cbind, lapply(claims_list, `length<-`, max(sapply(claims_list, length))))
    df[is.na(df)] <- NA
    df <- data.frame(apply(df, 2, cumsum))
    
    #Create functions to simulate filling the empty cells
    for (r in 1:n) {
      for (c in 1:n) {
        if (r > 1 && c > 1) {
          agg_fac <- sum(df[r, 1:(c-1)]) / sum(df[r-1, 1:(c-1)])
        } 
        
        if (is.na(df[r, c])) {
          df[r, c] <- df[r-1, c] * agg_fac
        }
      }
    }
    
    #add the forecasted year values as rows below
    new_yr <- df[n, ] * input$tail
    df <- rbind(df, new_yr)
    
    return(df)
  })
  
  # Output the new_table using renderTable
  output$filled_table <- renderTable({
    df <- t(round(new_table()))
    colnames(df) <- paste("Development Year ", 1:ncol(df))
    yr_col <- c(unique(data()[, 1]))
    df <- cbind(yr_col, df)
    colnames(df)[1] <- "Loss Year"
    df <- as.data.frame(df)
    
  })
  
  #Output the graph
  output$graph <- renderPlot({
    req(new_table())
    df <- new_table()
    n <- ncol(df)
    colours <- rainbow(n)
    
    plot(df[, 1], type = "b", 
         xlab = "Development Year", ylab = "Amount",
         main = "Cumulative Claim Amounts", 
         ylim = range(df), xlim = range(0:n+1),
         col = colours[1])
    for (i in 2:n) {
      lines(df[, i], type = "b", col = colours[i])
    }
    
    legend("bottom", legend = c(unique((data()[, 1]))), col = colours, lty = 1, cex = 0.8)
  })
  
}

shinyApp(ui = ui, server = server)
