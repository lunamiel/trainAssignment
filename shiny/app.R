# Shiny app for the train lateness distribution
# shows dtrain, ptrain, qtrain and rtrain with live plots and numbers
# run with shiny::runApp("shiny") from the trainAssignment folder

library(shiny)
library(bslib)
library(ggplot2)

# use my package if it's installed, otherwise load the functions straight from its source file
if (requireNamespace("MATH4753LuciaF26", quietly = TRUE)) {
  library(MATH4753LuciaF26)
} else {
  src <- c("../MATH4753LuciaF26/R/train.R", "MATH4753LuciaF26/R/train.R")
  src <- src[file.exists(src)][1]
  if (is.na(src)) stop("Install MATH4753LuciaF26 or run the app from the trainAssignment folder")
  source(src)
}

# matcha green theme
matcha_theme <- bs_theme(
  version = 5,
  bg = "#F4F1E6", fg = "#2F3E26",
  primary = "#7BA05B", secondary = "#C5D8A4", success = "#4F6F3A",
  base_font = "'Segoe UI', system-ui, sans-serif",
  "card-border-color" = "#C5D8A4"
)

# turn "1, 2.5 -3" into a numeric vector, NULL if the box is empty
parse_nums <- function(txt) {
  txt <- trimws(txt)
  if (txt == "") return(NULL)
  parts <- strsplit(txt, "[,; ]+")[[1]]
  v <- suppressWarnings(as.numeric(parts))
  validate(need(!anyNA(v), paste0("Couldn't read \"", paste(parts[is.na(v)], collapse = ", "),
                                  "\" as a number. Use numbers separated by commas.")))
  v
}

# run one of the train functions and show its error message instead of crashing
safe <- function(expr) {
  tryCatch(expr, error = function(e) validate(need(FALSE, conditionMessage(e))))
}

# small table of the values from a train object
value_table <- function(res, label) {
  tab <- data.frame(res$input, res$value)
  names(tab) <- c(label, "value")
  tab
}

typed_box <- function(id, placeholder) {
  textInput(id, "Or type your own values (comma separated)", placeholder = placeholder)
}

ui <- page_navbar(
  title = "Train lateness: d, p, q, r",
  theme = matcha_theme,
  fillable = FALSE,                    # let cards take their natural height instead of stretching
  navbar_options = navbar_options(bg = "#4F6F3A"),

  nav_panel(
    "Density",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("x", "x (minutes late)", min = -7, max = 7, value = 0, step = 0.1),
        typed_box("x_txt", "-6, 0, 2.5"),
        helpText("f(y) = 3/500 (25 - y^2) on (-5, 5), and 0 elsewhere.")
      ),
      layout_columns(
        value_box("f(x)", textOutput("d_val"), theme = "primary"),
        value_box("E(Y) and V(Y)", "0 and 5", theme = "secondary")
      ),
      card(card_header("dtrain(x)"), plotOutput("d_plot", height = 330)),
      card(card_header("Values"), tableOutput("d_tab"))
    )
  ),

  nav_panel(
    "CDF",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("q", "q (minutes late)", min = -7, max = 7, value = 2, step = 0.1),
        checkboxInput("q_upper", "Upper tail P(Y > q)", value = FALSE),
        typed_box("q_txt", "1, 3"),
        helpText("The shaded area is the probability for the first q.")
      ),
      layout_columns(
        value_box("P(Y <= q)", textOutput("cdf_lower"), theme = "primary"),
        value_box("P(Y > q)", textOutput("cdf_upper"), theme = "secondary")
      ),
      card(card_header("ptrain(q)"), plotOutput("p_plot", height = 330)),
      card(card_header("CDF F(q) over (-10, 10)"), plotOutput("p_cdf", height = 250)),
      card(card_header("Values"), tableOutput("p_tab"))
    )
  ),

  nav_panel(
    "Quantile",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("p", "p (probability)", min = 0, max = 1, value = 0.75, step = 0.005),
        checkboxInput("p_upper", "p is an upper tail probability", value = FALSE),
        typed_box("p_txt", "0.025, 0.975"),
        helpText("Try 0.025, 0.975 to see the 95% critical values. Values outside [0, 1] are rejected.")
      ),
      layout_columns(
        value_box("Quantile q", textOutput("q_val"), theme = "primary"),
        value_box("Check: F(q)", textOutput("q_check"), theme = "secondary")
      ),
      card(card_header("qtrain(p)"), plotOutput("q_plot", height = 330)),
      card(card_header("Values"), tableOutput("q_tab"))
    )
  ),

  nav_panel(
    "Random sample",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("n", "n (sample size)", min = 1, max = 20000, value = 10000, step = 1),
        numericInput("seed", "Seed (blank for random)", value = 4753),
        actionButton("resample", "Resample", class = "btn-primary"),
        helpText("Uniform(0, 1) values pushed through qtrain (the w-F theorem).")
      ),
      layout_columns(
        value_box("Sample mean", textOutput("r_mean"), "true value 0", theme = "primary"),
        value_box("Sample variance", textOutput("r_var"), "true value 5", theme = "secondary"),
        value_box("P(Y > 2): sample vs true", textOutput("r_tail"), theme = "secondary")
      ),
      card(card_header("rtrain(n)"), plotOutput("r_plot", height = 360))
    )
  ),

  nav_spacer(),
  nav_item(tags$span(style = "color: #F4F1E6;", "MATH 4753"))
)

server <- function(input, output, session) {

  # ---- density ----
  d_res <- reactive({
    req(!is.null(input$x))
    x <- parse_nums(input$x_txt)
    if (is.null(x)) x <- input$x
    safe(dtrain(x))
  })
  output$d_val  <- renderText(paste(signif(d_res()$value, 4), collapse = ", "))
  output$d_plot <- renderPlot(d_res()$plot)
  output$d_tab  <- renderTable(value_table(d_res(), "x"), digits = 4)

  # ---- cdf ----
  p_res <- reactive({
    q <- parse_nums(input$q_txt)
    if (is.null(q)) q <- input$q
    safe(ptrain(q, lower.tail = !input$q_upper))
  })
  output$cdf_lower <- renderText(paste(round(p_res()$lower, 4), collapse = ", "))
  output$cdf_upper <- renderText(paste(round(p_res()$upper, 4), collapse = ", "))
  output$p_plot  <- renderPlot(p_res()$plot)
  output$p_cdf   <- renderPlot({
    res <- p_res()
    grid <- data.frame(x = seq(-10, 10, length.out = 500))
    grid$F <- ptrain(grid$x, plot = FALSE)$value
    pts <- data.frame(x = res$input, F = res$lower)[is.finite(res$input), , drop = FALSE]
    ggplot(grid, aes(x, F)) +
      geom_line(colour = "#4F6F3A", linewidth = 1.1) +
      geom_point(data = pts, colour = "#7BA05B", size = 3) +
      geom_segment(data = pts, aes(x = x, xend = x, y = 0, yend = F), linetype = 2, colour = "#7BA05B") +
      labs(x = "q (minutes late)", y = "F(q)") +
      theme_minimal(base_size = 12)
  })
  output$p_tab <- renderTable({
    res <- p_res()
    data.frame(q = res$input, `P(Y <= q)` = res$lower, `P(Y > q)` = res$upper, check.names = FALSE)
  }, digits = 4)

  # ---- quantile ----
  q_res <- reactive({
    p <- parse_nums(input$p_txt)
    if (is.null(p)) p <- input$p
    safe(qtrain(p, lower.tail = !input$p_upper))
  })
  output$q_val   <- renderText(paste(round(q_res()$value, 4), collapse = ", "))
  output$q_check <- renderText({
    res <- q_res()
    paste(round(ptrain(res$value, lower.tail = res$lower.tail, plot = FALSE)$value, 4), collapse = ", ")
  })
  output$q_plot <- renderPlot(q_res()$plot)
  output$q_tab  <- renderTable(value_table(q_res(), "p"), digits = 4)

  # ---- random sample ----
  r_res <- reactive({
    input$resample                     # pressing the button makes a new sample
    n <- input$n
    validate(need(is.numeric(n) && n >= 1, "n must be at least 1"))
    seed <- input$seed
    # the seed is only used for the first sample; resample gives fresh draws
    if (!is.na(seed) && is.numeric(seed) && input$resample == 0) set.seed(seed)
    safe(rtrain(n))
  })
  output$r_mean <- renderText(sprintf("%.4f", r_res()$sample_mean))
  output$r_var  <- renderText(ifelse(is.na(r_res()$sample_variance), "needs n > 1",
                                     sprintf("%.4f", r_res()$sample_variance)))
  output$r_tail <- renderText(sprintf("%.4f vs %.4f", mean(r_res()$value > 2),
                                      ptrain(2, lower.tail = FALSE, plot = FALSE)$value))
  output$r_plot <- renderPlot(r_res()$plot)
}

shinyApp(ui, server)
