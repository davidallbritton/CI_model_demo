# Demonstration of Integration Phase of the C-I Model

library(shiny)
library(ggplot2)

# Proposition group colors, from the colorblind-safe Okabe-Ito palette; used for
# the input labels and for the matrix labels and legend
group_colors <- c(S = "#D55E00", N2 = "#0072B2", N1 = "#009E73")
group_label <- function(g) span(style = sprintf("color: %s;", group_colors[[g]]), g)

# Define UI for the app
ui <- fluidPage(
  # Clicking a number in C*A or A' tells the server which one; the server sends
  # back how that number was calculated, shown in a box beside the clicked cell
  tags$head(
    tags$style(HTML("
      #vectorA_prime td, #vectorA_prime_normalized td { cursor: pointer; }
      #vectorA_prime td:hover, #vectorA_prime_normalized td:hover { background: #eef3fb; }
      td.calc-active { outline: 2px solid #333; outline-offset: -2px; background: #eef3fb; }
      #calc-box { display: none; position: absolute; z-index: 1000; width: 380px; max-width: 90vw;
                  background: #fff; border: 1px solid #999; border-radius: 6px;
                  box-shadow: 0 4px 14px rgba(0, 0, 0, 0.2); padding: 10px 14px; }
      #calc-box .calc-title { font-weight: bold; font-size: 110%; padding-right: 20px; }
      #calc-box .calc-close { position: absolute; top: 4px; right: 8px; border: none;
                              background: none; font-size: 20px; line-height: 1; cursor: pointer; }
      #calc-box .calc-body { margin-top: 8px; }
      .vector-row { display: flex; justify-content: center; gap: 6px; }
      .vector-row > div { min-width: 56px; }
      .vector-row h4 { text-align: center; margin-top: 5px; margin-bottom: 4px; }
      .vector-row table { margin-left: auto; margin-right: auto; }
      .vector-row > div:first-child h4 { padding-left: 34px; }
    ")),
    tags$script(HTML("
      var calcCell = null;
      function hideCalcBox() {
        $('#calc-box').hide();
        $('td.calc-active').removeClass('calc-active');
      }
      $(document).on('click', '#vectorA_prime td, #vectorA_prime_normalized td', function() {
        $('td.calc-active').removeClass('calc-active');
        calcCell = $(this).addClass('calc-active');
        Shiny.setInputValue('calc_click', {
          which: $(this).closest('.shiny-html-output').attr('id'),
          row: $(this).parent().index() + 1
        }, {priority: 'event'});
      });
      // Place the box to the right of the clicked cell, or to its left if it
      // would run off the right edge of the window
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('calc_explain', function(msg) {
          if (!calcCell) return;
          var box = $('#calc-box');
          box.find('.calc-title').html(msg.title);
          box.find('.calc-body').html(msg.body);
          box.show();
          var off = calcCell.offset();
          var left = off.left + calcCell.outerWidth() + 12;
          if (left + box.outerWidth() > $(window).scrollLeft() + $(window).width()) {
            left = Math.max(0, off.left - box.outerWidth() - 12);
          }
          box.css({ top: Math.max(0, off.top - 10), left: left });
        });
      });
      $(document).on('click', '#calc-box .calc-close', hideCalcBox);
      $(document).on('click', function(e) {
        if (!$(e.target).closest('#calc-box, #vectorA_prime td, #vectorA_prime_normalized td').length) {
          hideCalcBox();
        }
      });
      $(document).on('keydown', function(e) { if (e.key === 'Escape') hideCalcBox(); });

      // Line up each row of the vector tables with the same proposition's row
      // in matrix C, which has an extra header row and a legend above it
      function alignVectors() {
        var mrows = $('#matrixC tr').slice(1);
        if (!mrows.length) return;
        ['vectorA', 'vectorA_prime', 'vectorA_prime_normalized', 'vectorS_vec'].forEach(function(id) {
          var t = $('#' + id + ' table');
          if (!t.length) return;
          var rows = t.find('tr');
          rows.each(function(k) {
            if (k < mrows.length) $(this).css('height', mrows[k].getBoundingClientRect().height + 'px');
          });
          // Padding on the output's container, unlike a margin on the table,
          // does not merge with the heading's bottom margin
          var box = $('#' + id).css('padding-top', '0px');
          var shift = mrows.first().offset().top - rows.first().offset().top;
          box.css('padding-top', Math.max(0, shift) + 'px');
        });
      }
      // Realign whenever the matrix, its legend, or a vector table is redrawn
      var alignPending = false;
      function scheduleAlign() {
        if (alignPending) return;
        alignPending = true;
        requestAnimationFrame(function() { alignPending = false; alignVectors(); });
      }
      $(function() {
        var observer = new MutationObserver(scheduleAlign);
        ['matrixC', 'groupLegend', 'vectorA', 'vectorA_prime', 'vectorA_prime_normalized', 'vectorS_vec']
          .forEach(function(id) {
            var el = document.getElementById(id);
            if (el) observer.observe(el, { childList: true, subtree: true });
          });
      });
      $(window).on('resize', scheduleAlign);
    "))
  ),
  div(id = "calc-box",
      tags$button(class = "calc-close", title = "Close", HTML("&times;")),
      div(class = "calc-title"),
      div(class = "calc-body")),

  titlePanel("Integration Phase of the C-I Model"),

  # Default values. The group sizes stay within the ranges Kintsch used:
  # - Kintsch & van Dijk (1978, Psychological Review, 85, 363-394) processed
  #   their example text sentence by sentence, so the number of new text
  #   propositions per cycle "ranges between 7 and 12." They set the buffer at
  #   s = 4, calling that value arbitrary; s = 1, 2, or 3 fit the recall data
  #   almost as well, but s = 10 did not fit.
  # - Kintsch (1988, Psychological Review, 95, 163-182) describes cycles as
  #   "roughly corresponding to short sentences or phrases." In the second cycle
  #   of his worked example, 4 carried-over propositions were joined by 9 new
  #   text propositions and 11 propositions from the knowledge base, and he
  #   again carried over the four most strongly activated propositions.
  # S = 4 follows both papers. N2 = 5 stands for a short clause, at the low end
  # of those input sizes, which keeps the matrix small enough to read on screen.
  # N1 = 8 makes LTM propositions outnumber text propositions somewhat, as in the
  # 1988 example.
  # Seed 28074 was chosen by simulating seeds 1-50,000 with these sizes and a
  # criterion of 0.1: it takes 8 steps (the most any of those seeds took), the
  # top S changes four times, and three LTM propositions end in the top S.
  # If these defaults change, also change the starting groups in the server
  # (groups <- reactiveVal(...)).
  fluidRow(
    column(12,
           div(style = "display: inline-block; width: 100px;",
               textInput("seed", "Seed", value = "28074", width = "80px")
           ),
           div(style = "display: inline-block; width: 100px;",
               textInput("criterion", "Criterion", value = "0.1", width = "80px")
           ),
           div(style = "display: inline-block; width: 100px;",
               textInput("S", group_label("S"), value = "4", width = "80px")
           ),
           div(style = "display: inline-block; width: 100px;",
               textInput("N2", group_label("N2"), value = "5", width = "80px")
           ),
           div(style = "display: inline-block; width: 100px;",
               textInput("N1", group_label("N1"), value = "8", width = "80px")
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
           ),
           p(em("Matrix C represents the network of propositions built in the construction phase: a 1 means the propositions in that row and column are connected, and a 0 means they are not (each proposition counts as connected to itself)."),
             style = "color: #666;")
    ),
    column(4, 
           h3("Activation Vector", style = "text-align: center;"),
           # Each vector is a narrow column with its heading centered over its
           # numbers; A's column is wider because it also holds the row labels
           div(class = "vector-row",
               div(h4("A"), tableOutput("vectorA")),
               div(h4("C*A"), tableOutput("vectorA_prime")),
               div(h4("A'"), tableOutput("vectorA_prime_normalized")),
               div(h4("S"), tableOutput("vectorS_vec"))
           ),
           p(em("Click a number in C*A or A' to see how it was calculated."),
             style = "text-align: center; color: #666;")
    ),
    column(3,
           h3("Delta for Activation"),
           plotOutput("deltaPlot", height = "200px"),
           h4("Propositions with the Highest Activations"),
           p(em("After each step, most recent first (last 20 steps)"),
             style = "color: #666; margin-top: -4px;"),
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
          <li>“S” is the size of the working-memory buffer: the number of propositions carried over from the previous cycle into this one, and also the number that will be carried over to the next cycle. After each step, the S most active propositions are marked with “**”; at the end of the cycle, these are the ones carried over to the next cycle, and they need not be the same propositions that were carried in. The demonstration only shows a single cycle of the integration process for a single set of propositions in the Coherence Matrix.</li>
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
  # then N1 retrieved from LTM.
  group_info <- list(
    S  = list(label = "Carried over (S)", start = "starts 0.3–1", color = group_colors[["S"]]),
    N2 = list(label = "From text (N2)",   start = "starts at 1",  color = group_colors[["N2"]]),
    N1 = list(label = "From LTM (N1)",    start = "starts at 0",  color = group_colors[["N1"]])
  )
  groups <- reactiveVal(rep(c("S", "N2", "N1"), c(4, 5, 8)))

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
    top_history(list())
  }, ignoreNULL = FALSE)

  # The S most active propositions after each step, most recent last; only the
  # last max_history steps are kept
  top_history <- reactiveVal(list())
  max_history <- 20
  top_set <- function(a, S_n = sum(groups() == "S")) {
    sort(order(a, decreasing = TRUE)[seq_len(min(S_n, length(a)))])
  }
  add_history <- function(entries) {
    h <- c(top_history(), entries)
    top_history(tail(h, max_history))
  }
  
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
  }, rownames = TRUE, digits = 0, sanitize.text.function = identity)

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
  
  # List the S propositions with the highest activations (the "**" rows) after
  # each of the last max_history steps, most recent first, in proposition order
  output$topPropositions <- renderUI({
    h <- top_history()
    if (length(h) == 0) {
      return(p(em("Click Step or Full Cycle to see results.")))
    }
    lines <- vapply(rev(h), function(e) {
      sprintf("Step %d: %s", e$step, paste(prop_labels(e$props), collapse = ", "))
    }, "")
    div(style = "line-height: 1.6;", HTML(paste(lines, collapse = "<br>")))
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
    add_history(list(list(step = length(deltas()), props = top_set(A_prime_normalized_val))))

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

  # Show how a clicked number in C*A or A' was calculated from the current step
  observeEvent(input$calc_click, {
    i <- input$calc_click$row
    req(length(deltas()) > 0, i >= 1, i <= length(A_prime()))
    fmt <- function(x) sprintf("%.3f", x)
    label_i <- prop_labels(i)
    CA <- as.vector(A_prime())

    if (input$calc_click$which == "vectorA_prime") {
      # C*A for row i: sum over j of C[i, j] * A[j]; since C is 0/1, this is
      # the sum of A over the propositions connected to proposition i
      A_val <- as.vector(A_prev())
      linked <- which(C()[i, ] == 1)
      rows <- paste0("<tr><td style='padding: 2px 12px;'>", prop_labels(linked),
                     "</td><td style='text-align: right; padding: 2px 12px;'>",
                     fmt(A_val[linked]), "</td></tr>", collapse = "")
      body <- tagList(
        p(HTML(sprintf(
          "C*A for %s multiplies each proposition's activation in A by its connection to %s in row %s of matrix C (1 if connected, 0 if not), then adds the products. Only the %d propositions with a 1 in that row contribute, so C*A is the sum of their activations in A:",
          label_i, label_i, label_i, length(linked)))),
        HTML(paste0("<table style='margin-left: 12px;'>",
                    "<tr><th style='padding: 2px 12px;'>Proposition</th><th style='text-align: right; padding: 2px 12px;'>A</th></tr>",
                    rows,
                    "<tr style='border-top: 1px solid #999;'><td style='padding: 2px 12px;'><b>Sum = C*A</b></td>",
                    "<td style='text-align: right; padding: 2px 12px;'><b>", fmt(CA[i]), "</b></td></tr></table>"))
      )
      title <- HTML(paste("How C*A for", label_i, "was calculated"))
    } else {
      # A' for row i: C*A for row i divided by the largest value in C*A
      k <- which.max(CA)
      body <- tagList(
        p(HTML(sprintf(
          "A' for %s is its value in C*A divided by the largest value in C*A, which belongs to %s. This normalization keeps every activation between 0 and 1, and the most active proposition always gets 1.",
          label_i, prop_labels(k)))),
        p(HTML(sprintf("A' = %s ÷ %s = <b>%s</b>", fmt(CA[i]), fmt(CA[k]), fmt(CA[i] / CA[k]))),
          style = "margin-left: 12px; font-size: 115%;")
      )
      title <- HTML(paste("How A' for", label_i, "was calculated"))
    }

    body <- tagList(
      body,
      p(em("Values are rounded to 3 decimal places; the tables show 2."),
        style = "color: #666; margin: 10px 0 0 0;")
    )
    session$sendCustomMessage("calc_explain", list(title = as.character(title),
                                                   body = as.character(body)))
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
    # The top-S sets of the most recent max_history steps, kept in a ring
    recent_tops <- vector("list", max_history)
    S_n <- sum(groups() == "S")
    n <- 0
    while (Delta_val >= crit && n < max_iter) {
      A_prev_val <- A_val
      A_prime_val <- C_val %*% A_val
      A_val <- A_prime_val / max(A_prime_val)
      Delta_val <- sum(abs(A_prev_val - A_val))
      n <- n + 1
      new_deltas[n] <- Delta_val
      recent_tops[[(n - 1) %% max_history + 1]] <- top_set(A_val, S_n)
    }
    if (n > 0) {
      first_step <- length(deltas())
      kept <- max(1, n - max_history + 1):n
      add_history(lapply(kept, function(k) {
        list(step = first_step + k, props = recent_tops[[(k - 1) %% max_history + 1]])
      }))
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
