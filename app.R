library(shiny)
library(ggplot2)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Matrix-Vector Iteration in R"),
  
  fluidRow(
    column(4, 
           actionButton("step", "Step"),
           actionButton("repeat_button", "Repeat"),
           h3("Coherence Matrix C"),
           tableOutput("matrixC")
    ),
    column(4, 
           fluidRow(
             column(4, h3("A"), tableOutput("vectorA")),
             column(4, h3("C * A"), tableOutput("vectorA_prime")),
             column(4, h3("A'"), tableOutput("vectorA_prime_normalized"))
           )
    ),
    column(4,
           h3("Delta Plot"),
           plotOutput("deltaPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Step 1: Initialize the matrix C and vectors
  set.seed(48)
  C <- diag(1, 4)
  upper_values <- sample(0:1, 6, replace = TRUE)
  C[upper.tri(C)] <- upper_values
  C[lower.tri(C)] <- t(C)[lower.tri(C)]
  
  A_original <- rep(1, 4)
  A <- reactiveVal(A_original)
  A_prev <- reactiveVal(A_original)  # Store previous value of A
  A_prime <- reactiveVal(A_original)
  A_prime_normalized <- reactiveVal(A_original)
  Delta <- reactiveVal(Inf)
  deltas <- reactiveVal(c())
  
  # Display Matrix C
  output$matrixC <- renderTable({
    colnames(C) <- paste0("P", 1:ncol(C))
    C
  })
  
  # Display the previous vector A
  output$vectorA <- renderTable({
    data.frame(A_prev(), row.names = paste0("P", 1:length(A_prev())))
  }, colnames = FALSE)
  
  # Display vector A' (after multiplication)
  output$vectorA_prime <- renderTable({
    data.frame(A_prime(), row.names = paste0("P", 1:length(A_prime())))
  }, colnames = FALSE)
  
  # Display normalized vector A'
  output$vectorA_prime_normalized <- renderTable({
    data.frame(A_prime_normalized(), row.names = paste0("P", 1:length(A_prime_normalized())))
  }, colnames = FALSE)
  
  # Display Delta plot
  output$deltaPlot <- renderPlot({
    if (length(deltas()) > 1) {
      data <- data.frame(Iteration = seq_along(deltas()), Delta = deltas())
      ggplot(data, aes(x = Iteration, y = Delta)) +
        geom_line() +
        geom_point() +
        labs(title = "Delta Over Iterations", x = "Iteration", y = "Delta") +
        theme_minimal() +
        ylim(0, 1)
    }
  })
  
  # Define the iteration function
  iterate <- function() {
    A_prev(A())  # Store current A as previous value
    A_val <- A()
    A_prime_val <- C %*% A_val
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
    if (Delta() >= 0.1) {
      iterate()
    }
  })
  
  # Repeat action
  observeEvent(input$repeat_button, {
    while (Delta() >= 0.1) {
      iterate()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
