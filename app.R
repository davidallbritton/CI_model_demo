library(shiny)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Matrix-Vector Iteration in R"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("step", "Step"),
      actionButton("repeat_button", "Repeat")  # Renamed the button
    ),
    
    mainPanel(
      h3("Matrix C"),
      tableOutput("matrixC"),
      
      h3("Current Vector A"),
      tableOutput("vectorA"),
      
      h3("Vector A' (After Multiplication)"),
      tableOutput("vectorA_prime"),
      
      h3("Normalized Vector A'"),
      tableOutput("vectorA_prime_normalized"),
      
      h3("Delta Values"),
      tableOutput("deltaValues")
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
  A_prime <- reactiveVal(A_original)
  A_prime_normalized <- reactiveVal(A_original)
  Delta <- reactiveVal(Inf)
  deltas <- reactiveVal(c())
  
  # Display Matrix C
  output$matrixC <- renderTable({
    C
  })
  
  # Display the current vector A
  output$vectorA <- renderTable({
    t(A())
  })
  
  # Display vector A' (after multiplication)
  output$vectorA_prime <- renderTable({
    t(A_prime())
  })
  
  # Display normalized vector A'
  output$vectorA_prime_normalized <- renderTable({
    t(A_prime_normalized())
  })
  
  # Display all Delta values
  output$deltaValues <- renderTable({
    deltas()
  })
  
  # Define the iteration function
  iterate <- function() {
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
  observeEvent(input$repeat_button, {  # Renamed here too
    while (Delta() >= 0.1) {
      iterate()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
