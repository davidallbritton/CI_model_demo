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
  ), 
  
  fluidRow(
    column(
      width = 12,
      HTML('
        <h4>Conceptual Explanation of the Demonstration</h4>
        <p>This is a demonstration of the “integration” phase of Kintsch’s (1988; 1998) Construction-Integration Model of text comprehension. Assuming the “construction” phase has already produced a set of propositions (P1, P2, P3...) and the connections among those propositions are already represented in the Coherence Matrix C, you can then “Step” through single iterations of the integration process or click “Full Cycle” to see the results at the end of the cycle. A “cycle” refers to all the processing that happens when one new clause or sentence of the text is “read” by the model.</p>
        <p>The vector “A” is the activation values for each proposition at the beginning of a step in the integration cycle. In one step of the integration cycle, those values are multiplied by the Coherence Matrix C (“C*A”) and then normalized by dividing by the largest resulting value to produce the new activation vector “A’.” Delta is the sum of the absolute differences between the values in A and A’. If Delta is greater than the “Criterion” value, then A’ becomes the new A and another step of the cycle is computed. This continues until Delta < Criterion, indicating that the model has settled into a stable state.</p>
        <p>At the end of a processing cycle, the propositions with the highest values (denoted with “**” in column “S”) would be carried over to make the Coherence Matrix for the next cycle. You can think of the Coherence Matrix as a representation of the contents of Working Memory. The Coherence Matrix has a total of N propositions: S propositions carried over from the previous cycle; N2 propositions from the new clause or sentence that is being read; and N1 propositions that are retrieved from long term memory (LTM) because they are related to the propositions from the text.</p>

        <h4>Demonstration Details</h4>
        <ul>
          <li>“Seed” is just a seed value for the pseudo-random number generator so that you can reproduce the results.</li>
          <li>Coherence Matrix C has randomly generated 1’s and 0’s indicating whether pairs of propositions are related. The matrix C changes whenever “Seed” or “N” changes.</li>
          <li>“Criterion” is the stopping value for Delta.</li>
          <li>“N” is the number of propositions in the Coherence Matrix.</li>
          <li>“S” is the number of propositions carried over to the next cycle. In this demonstration, the S propositions are just marked with “**”; the demonstration only shows a single cycle of the integration process for a single set of propositions in the Coherence Matrix.</li>
          <li>Calculations and labels are based on Reichle (2021), pp. 320-321.</li>
        </ul>

        <h4>References</h4>
        <p>Kintsch, W. (1988). The role of knowledge in discourse comprehension: A construction-integration model. <i>Psychological Review, 95(2),</i> 163–182. https://doi.org/10.1037/0033-295X.95.2.163</p>
        <p>Kintsch, W. (1998). <i>Comprehension: A paradigm for cognition</i>. Cambridge University Press.  ISBN: 978-0521629867</p>
        <p>Reichle, E. D. (2021). <i>Computational models of reading: A handbook</i>. Oxford University Press. ISBN: 9780195370669</p>
      ')
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
