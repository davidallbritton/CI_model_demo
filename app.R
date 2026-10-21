# Demonstration of Integration Phase of the C-I Model

library(shiny)
library(ggplot2)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Integration Phase of the C-I Model"),
  
  fluidRow(
    column(12,
           div(style = "display: inline-block; width: 100px;",
               textInput("seed", "Seed", value = "37", width = "80px")
           ),
           div(style = "display: inline-block; width: 100px;",
               textInput("criterion", "Criterion", value = "0.1", width = "80px")
           ),
           div(style = "display: inline-block; width: 100px;",
               textInput("S", "S", value = "2", width = "80px")
           ),
           div(style = "display: inline-block; width: 100px;",
               textInput("N2", "N2", value = "3", width = "80px")
           ),
           div(style = "display: inline-block; width: 100px;",
               textInput("N1", "N1", value = "3", width = "80px")
           ),
           div(style = "display: inline-block; width: 100px; vertical-align: top;",
               tags$label("N"),
               div(style = "padding-top: 7px; font-weight: bold;", textOutput("N_total", inline = TRUE))
           ),

           actionButton("step", "Step"),
           actionButton("repeat_button", "Full Cycle")
    )
  ),
  
  fluidRow(
    column(5, 
           h3("Coherence Matrix C"),
           uiOutput("groupLegend"),
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
           plotOutput("deltaPlot", height = "200px"),
           h4("Propositions with the Highest Activations"),
           uiOutput("topPropositions")
    )
  ), 
  
  fluidRow(
    column(
      width = 12,
      HTML('
        <h4>Conceptual Explanation of the Demonstration</h4>
        <p>This is a demonstration of the “integration” phase of Kintsch’s (1988; 1998) Construction-Integration Model of text comprehension. Assuming the “construction” phase has already produced a set of propositions (P1, P2, P3...) and the connections among those propositions are already represented in the Coherence Matrix C, you can then “Step” through single iterations of the integration process or click “Full Cycle” to see the results at the end of the cycle. A “cycle” refers to all the processing that happens when one new clause or sentence of the text is “read” by the model.</p>
        <p>The vector “A” is the activation values for each proposition at the beginning of a step in the integration cycle. In one step of the integration cycle, those values are multiplied by the Coherence Matrix C (“C*A”) and then normalized by dividing by the largest resulting value to produce the new activation vector “A’.” Delta is the sum of the absolute differences between the values in A and A’. If Delta is greater than the “Criterion” value, then A’ becomes the new A and another step of the cycle is computed. This continues until Delta < Criterion, indicating that the model has settled into a stable state.</p>
        <p>At the end of a processing cycle, the propositions with the highest values (denoted with “**” in column “S”) would be carried over to make the Coherence Matrix for the next cycle. You can think of the Coherence Matrix as a representation of the contents of Working Memory. The Coherence Matrix has a total of N propositions: S propositions carried over from the previous cycle; N2 propositions from the new clause or sentence that is being read; and N1 propositions that are retrieved from long term memory (LTM) because they are related to the propositions from the text. The three groups are color-coded and start with different activations: propositions from the text start fully active (1), propositions retrieved from LTM start inactive (0), and carried-over propositions start with leftover activation from the previous cycle (a random value between 0.3 and 1).</p>
        <p>Watch how the rankings change from step to step. Early steps mostly reflect where activation started, so the text propositions tend to lead. As the integration process continues, activation spreads through the connections in the matrix, and a well-connected LTM or carried-over proposition can climb into the top S. The final stable state is determined by the pattern of connections in the Coherence Matrix rather than by the starting activations; the starting activations shape the path the model takes to get there.</p>

        <h4>Demonstration Details</h4>
        <ul>
          <li>“Seed” is just a seed value for the pseudo-random number generator so that you can reproduce the results.</li>
          <li>Coherence Matrix C has randomly generated 1’s and 0’s indicating whether pairs of propositions are related. Each LTM proposition is connected to at least one text proposition, since it was retrieved because of something in the text. The matrix C and the starting activations change whenever “Seed,” “S,” “N2,” or “N1” changes.</li>
          <li>“Criterion” is the stopping value for Delta.</li>
          <li>“S” is the size of the working-memory buffer: the number of propositions carried over from the previous cycle into this one, and also the number that will be carried over to the next cycle. At the end of the cycle the S most active propositions are marked with “**”; they need not be the same propositions that were carried in. The demonstration only shows a single cycle of the integration process for a single set of propositions in the Coherence Matrix.</li>
          <li>“N2” is the number of propositions from the new clause or sentence of text.</li>
          <li>“N1” is the number of propositions retrieved from LTM.</li>
          <li>“N” is the total number of propositions in the Coherence Matrix (S + N2 + N1).</li>
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
  # Read a text box as a number. If the box is blank, not a number, or out of
  # range (e.g. while the user is mid-edit), req() quietly pauses whatever is
  # using it instead of crashing the app; it resumes once a valid value is typed.
  num_input <- function(id, min = -Inf, integer = FALSE) {
    x <- suppressWarnings(as.numeric(trimws(input[[id]])))
    req(length(x) == 1, !is.na(x), x >= min, !integer || x == round(x))
    x
  }
  criterion_input <- function() {
    x <- num_input("criterion")
    req(x > 0)  # a criterion of 0 or less could make Full Cycle run forever
    x
  }

  # Proposition groups, in matrix order: S carried over, then N2 from the text,
  # then N1 retrieved from LTM. Colors are from the colorblind-safe Okabe-Ito palette.
  group_info <- list(
    S  = list(label = "Carried over (S)", start = "starts 0.3–1", color = "#D55E00"),
    N2 = list(label = "From text (N2)",   start = "starts at 1",  color = "#0072B2"),
    N1 = list(label = "From LTM (N1)",    start = "starts at 0",  color = "#009E73")
  )
  groups <- reactiveVal(rep(c("S", "N2", "N1"), c(2, 3, 3)))

  # Proposition names as bold, group-colored HTML (e.g. for table labels)
  prop_labels <- function(indices = seq_along(groups())) {
    cols <- vapply(groups()[indices], function(g) group_info[[g]]$color, "")
    sprintf('<span style="color: %s; font-weight: bold;">P%d</span>', cols, indices)
  }

  output$groupLegend <- renderUI({
    present <- intersect(names(group_info), unique(groups()))
    div(style = "margin-bottom: 8px;",
        lapply(present, function(g) {
          gi <- group_info[[g]]
          span(style = "display: inline-block; margin-right: 14px; white-space: nowrap;",
               span(style = sprintf("display: inline-block; width: 12px; height: 12px; background: %s; margin-right: 4px; vertical-align: middle;", gi$color)),
               span(style = sprintf("color: %s; font-weight: bold;", gi$color), gi$label),
               span(style = "color: #666;", paste0(" (", gi$start, ")")))
        }))
  })

  output$N_total <- renderText(length(groups()))

  # Step 1: Initialize the matrix C and vectors
  C <- reactiveVal()

  observeEvent({input$seed; input$S; input$N2; input$N1}, {
    seed_val <- num_input("seed")
    S_n  <- num_input("S",  min = 1, integer = TRUE)
    N2_n <- num_input("N2", min = 1, integer = TRUE)
    N1_n <- num_input("N1", min = 0, integer = TRUE)
    N_val <- S_n + N2_n + N1_n
    grp <- rep(c("S", "N2", "N1"), c(S_n, N2_n, N1_n))

    set.seed(seed_val)
    C_val <- diag(1, N_val)
    upper_values <- sample(0:1, (N_val * (N_val - 1)) / 2, replace = TRUE)
    C_val[upper.tri(C_val)] <- upper_values
    C_val[lower.tri(C_val)] <- t(C_val)[lower.tri(C_val)]

    # Each LTM proposition was retrieved because of something in the text, so
    # make sure it is linked to at least one text proposition
    text_idx <- which(grp == "N2")
    for (i in which(grp == "N1")) {
      if (all(C_val[i, text_idx] == 0)) {
        j <- text_idx[sample.int(length(text_idx), 1)]
        C_val[i, j] <- 1
        C_val[j, i] <- 1
      }
    }
    C(C_val)
    groups(grp)

    # Starting activations: carried over = leftover activation from the previous
    # cycle (random, 0.3-1); from text = 1; from LTM = 0
    A_original <- numeric(N_val)
    A_original[grp == "S"]  <- runif(S_n, 0.3, 1)
    A_original[grp == "N2"] <- 1
    A_original[grp == "N1"] <- 0
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
    req(ncol(C_val) == length(groups()))
    labels <- prop_labels()
    C_df <- as.data.frame(C_val, row.names = labels)
    colnames(C_df) <- labels
    C_df
  }, rownames = TRUE, sanitize.text.function = identity)

  # Display the previous vector A

  output$vectorA <- renderTable({
    req(length(A_prev()) == length(groups()))
    data.frame(A_prev(), row.names = prop_labels())
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
    S_val <- min(num_input("S", min = 1, integer = TRUE), length(A_prime_norm))
    if (length(deltas()) == 0) {
      # Nothing has been computed yet, so no proposition is marked
      S_vec(rep(".", length(A_prime_norm)))
    } else if (length(A_prime_norm) > 0) {
      indices <- order(A_prime_norm, decreasing = TRUE)[seq_len(S_val)]
      S_vec_val <- rep(".", length(A_prime_norm))
      S_vec_val[indices] <- "**"
      S_vec(S_vec_val)
    }
  })
  
  # Display S_vec
  output$vectorS_vec <- renderTable({
    data.frame(S_vec())
  }, colnames = FALSE)
  
  # List the S propositions with the highest activations (the "**" rows),
  # in proposition order, once at least one step has been run
  output$topPropositions <- renderUI({
    if (length(deltas()) == 0) {
      return(p(em("Click Step or Full Cycle to see results.")))
    }
    S_val <- min(num_input("S", min = 1, integer = TRUE), length(A_prime_normalized()))
    indices <- sort(order(A_prime_normalized(), decreasing = TRUE)[seq_len(S_val)])
    p(HTML(paste(prop_labels(indices), collapse = ", ")))
  })

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
    if (Delta() >= criterion_input()) {
      iterate()
    }
  })

  # Repeat action (capped at 100,000 iterations as a safety net against a cycle that never settles)
  # The loop works on plain local variables and writes the results back once at
  # the end; growing the reactive Delta history one value at a time gets very
  # slow over many iterations.
  observeEvent(input$repeat_button, {
    crit <- criterion_input()
    max_iter <- 100000
    C_val <- C()
    A_val <- A()
    Delta_val <- Delta()
    new_deltas <- numeric(max_iter)
    n <- 0
    while (Delta_val >= crit && n < max_iter) {
      A_prev_val <- A_val
      A_prime_val <- C_val %*% A_val
      A_val <- A_prime_val / max(A_prime_val)
      Delta_val <- sum(abs(A_prev_val - A_val))
      n <- n + 1
      new_deltas[n] <- Delta_val
    }
    if (n > 0) {
      A_prev(A_prev_val)
      A_prime(A_prime_val)
      A_prime_normalized(A_val)
      A(A_val)
      Delta(Delta_val)
      deltas(c(deltas(), new_deltas[seq_len(n)]))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
