# Demonstration of Integration Phase of the C-I Model

library(shiny)
library(ggplot2)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Integration Phase of the C-I Model"),
  
  fluidRow(
    column(12,
           div(style = "display: inline-block; width: 100px;",
               textInput("seed", "Seed", value = "24", width = "80px")
           ),
           div(style = "display: inline-block; width: 100px;",
               textInput("criterion", "Criterion", value = "0.1", width = "80px")
           ),
           div(style = "display: inline-block; width: 100px;",
               textInput("N", "N", value = "4", width = "80px")
           ),           div(style = "display: inline-block; width: 100px;",
                            textInput("S", "S", value = "3", width = "80px")
           ),
           
           actionButton("step", "Step"),
           actionButton("repeat_button", "Full Cycle")
    )
  ),
  
  fluidRow(
    column(5, 
           h3("Coherence Matrix C"),
           div(style = "overflow-x: auto; ",
               tableOutput("matrixC")
           )
    ),
    column(4, 
           fluidRow(
             h3("Activation Vector", style = "text-align: center;"),
             column(5, h4("A ", style = "text-align: center; margin-top: 5px; margin-bottom: 4px;"), tableOutput("vectorA")),
             column(3, h4("C*A", style = "text-align: center; margin-top: 5px; margin-bottom: 4px;"), tableOutput("vectorA_prime")),
             column(2, h4(" A'", style = "text-align: center; margin-top: 5px; margin-bottom: 4px;"), tableOutput("vectorA_prime_normalized")),
             column(2, h4("S", style = "text-align: center; margin-top: 5px; margin-bottom: 4px;"), tableOutput("vectorS_vec"))
           )
    ),
    column(3,
           h3("Delta for Activation"),
           plotOutput("deltaPlot", height = "200px")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Step 1: Initialize the matrix C and vectors
  C <- reactiveVal()
  
  observeEvent({input$seed; input$N}, {
    set.seed(as.numeric(input$seed))
    N_val <- as.numeric(input$N)
    C_val <- diag(1, N_val)
    upper_values <- sample(0:1, (N_val * (N_val - 1)) / 2, replace = TRUE)
    C_val[upper.tri(C_val)] <- upper_values
    C_val[lower.tri(C_val)] <- t(C_val)[lower.tri(C_val)]
    C(C_val)
    
    # Update A_original and reactive values based on new N
    A_original <- rep(1, N_val)
    A(A_original)
    A_prev(A_original)
    A_prime(A_original)
    A_prime_normalized(A_original)
    S_vec(rep(".", N_val))
    Delta(Inf)
    deltas(c())
  }, ignoreNULL = FALSE)
  
  A <- reactiveVal(rep(1, 4))
  A_prev <- reactiveVal(rep(1, 4))  # Store previous value of A
  A_prime <- reactiveVal(rep(1, 4))
  A_prime_normalized <- reactiveVal(rep(1, 4))
  S_vec <- reactiveVal(rep("", 4))  # Reactive vector containing empty strings
  Delta <- reactiveVal(Inf)
  deltas <- reactiveVal(c())
  
  # Display Matrix C
  output$matrixC <- renderTable({
    C_val <- C()
    if (is.null(C_val)) {
      return(NULL)
    }
    colnames(C_val) <- paste0("P", 1:ncol(C_val))
    C_val
  })
  
  # Display the previous vector A
  
  output$vectorA <- renderTable({
    data.frame(A_prev(), row.names = paste0("<b>P", 1:length(A_prev()), "</b>"))
  }, rownames = TRUE, colnames = FALSE, sanitize.rownames.function = identity)
  
  # Display vector A' (after multiplication)
  output$vectorA_prime <- renderTable({
    if (length(deltas()) == 0) {
      return(NULL)
    }
    data.frame(A_prime())
  }, colnames = FALSE)
  
  # Display normalized vector A'
  output$vectorA_prime_normalized <- renderTable({
    if (length(deltas()) == 0) {
      return(NULL)
    }
    data.frame(A_prime_normalized())
  }, colnames = FALSE)
  
  # Update S_vec based on highest values in A'
  observe({
    A_prime_norm <- A_prime_normalized()
    S_val <- as.numeric(input$S)
    if (length(A_prime_norm) > 0) {
      indices <- order(A_prime_norm, decreasing = TRUE)[1:S_val]
      S_vec_val <- rep(".", length(A_prime_norm))
      S_vec_val[indices] <- "**"
      S_vec(S_vec_val)
    }
  })
  
  # Display S_vec
  output$vectorS_vec <- renderTable({
    data.frame(S_vec())
  }, colnames = FALSE)
  
  # Display Delta plot
  output$deltaPlot <- renderPlot({
    if (length(deltas()) > 0) {
      data <- data.frame(Iteration = seq_along(deltas()), Delta = deltas())
      ggplot(data, aes(x = Iteration, y = Delta)) +
        geom_line() +
        geom_point() +
        labs(title = "Delta = change from A to A'", x = "Iteration", y = "Delta") +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_line(color = "grey80")) +
        xlim(1, max(4, length(deltas()))) +
        ylim(0, max(deltas(), na.rm = TRUE))
    }
  })
  
  # Define the iteration function
  iterate <- function() {
    A_prev(A())  # Store current A as previous value
    A_val <- A()
    C_val <- C()
    A_prime_val <- C_val %*% A_val
    A_prime_normalized_val <- A_prime_val / max(A_prime_val)
    Delta_val <- sum(abs(A_val - A_prime_normalized_val))
    
    # Update reactive values
    A_prime(A_prime_val)
    A_prime_normalized(A_prime_normalized_val)
    deltas(c(deltas(), Delta_val))
    
    # Update vector A with normalized A'
    A(A_prime_normalized_val)
    Delta(Delta_val)
  }
  
  # Step action
  observeEvent(input$step, {
    if (Delta() >= as.numeric(input$criterion)) {
      iterate()
    }
  })
  
  # Repeat action
  observeEvent(input$repeat_button, {
    while (Delta() >= as.numeric(input$criterion)) {
      iterate()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
