# Train lateness distribution: f(y) = 3/500 * (25 - y^2) on (-5, 5)

# matcha green colours used in all the plots
matcha <- c(dark = "#4F6F3A", mid = "#7BA05B", light = "#C5D8A4", cream = "#F4F1E6")

# true summaries of the distribution (E(Y) = 0, V(Y) = 5)
train_moments <- list(mean = 0, variance = 5, sd = sqrt(5), median = 0, mode = 0)

# stop with a clear message if x is not a usable numeric vector
check_numeric <- function(x, name) {
  if (is.logical(x) && length(x) > 0 && all(is.na(x))) return(invisible(TRUE)) # a plain NA just gives NA back
  if (!is.numeric(x)) stop("`", name, "` must be numeric, not ", class(x)[1], call. = FALSE)
  if (length(x) == 0) stop("`", name, "` must have at least one value", call. = FALSE)
  invisible(TRUE)
}

# stop unless x is a single TRUE or FALSE
check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) stop("`", name, "` must be TRUE or FALSE", call. = FALSE)
  invisible(TRUE)
}

# the density curve that every plot starts from
train_base_plot <- function(xlim = c(-7, 7)) {
  grid <- data.frame(x = seq(xlim[1], xlim[2], length.out = 600))
  grid$y <- ifelse(grid$x > -5 & grid$x < 5, 3 / 500 * (25 - grid$x^2), 0)
  ggplot2::ggplot(grid, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_line(colour = matcha[["dark"]], linewidth = 1.1) +
    ggplot2::labs(x = "y (minutes late)", y = "f(y)") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = matcha[["cream"]], colour = NA),
      plot.title = ggplot2::element_text(face = "bold", colour = matcha[["dark"]])
    )
}

# shade the density between a and b
shade <- function(a, b, fill = matcha[["mid"]]) {
  a <- max(a, -5)
  b <- min(b, 5)
  if (a >= b) return(NULL)
  s <- data.frame(x = seq(a, b, length.out = 300))
  s$y <- 3 / 500 * (25 - s$x^2)
  ggplot2::geom_area(data = s, ggplot2::aes(x = .data$x, y = .data$y), fill = fill, alpha = 0.6)
}

#' Density of the train lateness distribution
#'
#' Evaluates the density \eqn{f(y) = \frac{3}{500}(25 - y^2)} for
#' \eqn{-5 < y < 5} (and 0 elsewhere) of the time \eqn{Y}, in minutes, that a
#' commuter train is late. Negative values mean the train is early.
#'
#' @param x Numeric vector of values at which to evaluate the density. Any real
#'   value is allowed; values outside \eqn{(-5, 5)} have density 0.
#' @param plot Logical. If `TRUE` (default) a ggplot2 plot of the density with
#'   the points \eqn{(x, f(x))} marked is stored in the result.
#'
#' @return An object of class `"train"`: a list with
#'   \describe{
#'     \item{type}{`"density"`.}
#'     \item{input}{The `x` values supplied.}
#'     \item{value}{The density values \eqn{f(x)}.}
#'     \item{mean, variance, sd, median, mode}{True summaries of the distribution.}
#'     \item{plot}{A ggplot object (only when `plot = TRUE`).}
#'   }
#'   Printing it shows a table of values, the summaries and the plot.
#'
#' @seealso [ptrain()], [qtrain()], [rtrain()]
#' @export
#'
#' @examples
#' dtrain(0)
#' dtrain(c(-6, -2.5, 0, 2.5))
#' dtrain(seq(-5, 5, by = 1), plot = FALSE)$value
dtrain <- function(x, plot = TRUE) {
  check_numeric(x, "x")
  check_flag(plot, "plot")
  d <- ifelse(x > -5 & x < 5, 3 / 500 * (25 - x^2), 0)

  out <- c(list(type = "density", input = x, value = d), train_moments)
  if (plot) {
    pts <- data.frame(x = x, y = d)[is.finite(x), , drop = FALSE]
    out$plot <- train_base_plot(range(c(-7, 7, pts$x))) +
      ggplot2::geom_segment(
        data = pts, ggplot2::aes(x = .data$x, xend = .data$x, y = 0, yend = .data$y),
        colour = matcha[["mid"]], linetype = 2
      ) +
      ggplot2::geom_point(data = pts, ggplot2::aes(x = .data$x, y = .data$y),
                          colour = matcha[["dark"]], size = 3) +
      ggplot2::labs(title = "Train lateness density f(y)",
                    subtitle = "points show f(x) at the values you gave")
  }
  structure(out, class = "train")
}

#' Distribution function of the train lateness distribution
#'
#' Computes \eqn{P(Y \le q) = \frac{75q - q^3 + 250}{500}} for
#' \eqn{-5 < q < 5} (0 below, 1 above), or the upper tail \eqn{P(Y > q)}.
#'
#' @param q Numeric vector of quantiles. Any real value (including `Inf`) is
#'   allowed.
#' @param lower.tail Logical. If `TRUE` (default) `value` is \eqn{P(Y \le q)},
#'   otherwise \eqn{P(Y > q)}.
#' @param plot Logical. If `TRUE` (default) a ggplot2 plot of the density with
#'   the tail area for the first finite `q` shaded is stored in the result.
#'
#' @return An object of class `"train"`: a list with
#'   \describe{
#'     \item{type}{`"cdf"`.}
#'     \item{input}{The `q` values supplied.}
#'     \item{value}{The requested tail probability.}
#'     \item{lower.tail}{The `lower.tail` argument.}
#'     \item{lower, upper}{Both tail probabilities \eqn{P(Y \le q)} and \eqn{P(Y > q)}.}
#'     \item{mean, variance, sd, median, mode}{True summaries of the distribution.}
#'     \item{plot}{A ggplot object (only when `plot = TRUE`).}
#'   }
#'
#' @seealso [dtrain()], [qtrain()], [rtrain()]
#' @export
#'
#' @examples
#' ptrain(0)
#' ptrain(2)
#' ptrain(2, lower.tail = FALSE)$value
#' # probability the train is between 1 and 3 minutes late
#' diff(ptrain(c(1, 3), plot = FALSE)$value)
ptrain <- function(q, lower.tail = TRUE, plot = TRUE) {
  check_numeric(q, "q")
  check_flag(lower.tail, "lower.tail")
  check_flag(plot, "plot")
  qc <- pmin(pmax(q, -5), 5) # clamp to the support so +-Inf works too
  lower <- (75 * qc - qc^3 + 250) / 500
  upper <- 1 - lower

  out <- c(list(type = "cdf", input = q,
                value = if (lower.tail) lower else upper,
                lower.tail = lower.tail, lower = lower, upper = upper),
           train_moments)
  if (plot) {
    # shade the area for the first finite q
    i <- which(is.finite(q))[1]
    g <- train_base_plot()
    if (!is.na(i)) {
      g <- g + (if (lower.tail) shade(-5, q[i]) else shade(q[i], 5)) +
        ggplot2::geom_vline(xintercept = q[i], colour = matcha[["dark"]], linetype = 2)
    }
    out$plot <- g +
      ggplot2::labs(
        title = if (lower.tail) "P(Y <= q)" else "P(Y > q)",
        subtitle = if (is.na(i)) NULL else
          sprintf("shaded area for q = %.3f is %.4f", q[i], out$value[i])
      )
  }
  structure(out, class = "train")
}

#' Quantile function of the train lateness distribution
#'
#' Finds the quantile \eqn{q} with \eqn{F(q) = p} exactly. Setting
#' \eqn{F(q) = p} gives the cubic \eqn{q^3 - 75q + (500p - 250) = 0}, whose
#' root in \eqn{[-5, 5]} is
#' \deqn{q = 10\cos\left(\frac{\arccos(1 - 2p) - 2\pi}{3}\right).}
#' This is exact and vectorised, so no root finding is needed.
#'
#' @param p Numeric vector of probabilities, all in \eqn{[0, 1]}. `NA` values
#'   give `NA`.
#' @param lower.tail Logical. If `TRUE` (default) probabilities are
#'   \eqn{P(Y \le q)}, otherwise \eqn{P(Y > q)}.
#' @param plot Logical. If `TRUE` (default) a ggplot2 plot with the quantiles
#'   marked and labelled is stored in the result. With one quantile its tail is
#'   shaded; with two or more, both critical tails are shaded.
#'
#' @return An object of class `"train"`: a list with
#'   \describe{
#'     \item{type}{`"quantile"`.}
#'     \item{input}{The `p` values supplied.}
#'     \item{value}{The quantiles.}
#'     \item{lower.tail}{The `lower.tail` argument.}
#'     \item{mean, variance, sd, median, mode}{True summaries of the distribution.}
#'     \item{plot}{A ggplot object (only when `plot = TRUE`).}
#'   }
#'
#' @seealso [dtrain()], [ptrain()], [rtrain()]
#' @export
#'
#' @examples
#' qtrain(0.75)
#' qtrain(c(0.25, 0.5, 0.75))
#' # critical values for the middle 95%
#' qtrain(c(0.025, 0.975))
#' # the quantile undoes the cdf
#' ptrain(qtrain(0.3, plot = FALSE)$value, plot = FALSE)$value
qtrain <- function(p, lower.tail = TRUE, plot = TRUE) {
  check_numeric(p, "p")
  check_flag(lower.tail, "lower.tail")
  check_flag(plot, "plot")
  if (any(p < 0 | p > 1, na.rm = TRUE)) stop("p must be between 0 and 1", call. = FALSE)
  pl <- if (lower.tail) p else 1 - p
  qv <- 10 * cos((acos(1 - 2 * pl) - 2 * pi) / 3)

  out <- c(list(type = "quantile", input = p, value = qv, lower.tail = lower.tail),
           train_moments)
  if (plot) {
    ok <- qv[!is.na(qv)]
    g <- train_base_plot()
    if (length(ok) == 1) {
      # one quantile: shade the tail that has probability p
      g <- g + (if (lower.tail) shade(-5, ok) else shade(ok, 5))
    } else if (length(ok) > 1) {
      # several quantiles: shade both critical tails and the middle lightly
      g <- g + shade(min(ok), max(ok), fill = matcha[["light"]]) +
        shade(-5, min(ok)) + shade(max(ok), 5)
    }
    if (length(ok) > 0) {
      g <- g +
        ggplot2::geom_vline(xintercept = ok, colour = matcha[["dark"]], linetype = 2) +
        ggplot2::annotate("label", x = ok, y = 0.165, label = sprintf("%.3f", ok),
                          colour = matcha[["dark"]], size = 3.5)
    }
    out$plot <- g +
      ggplot2::coord_cartesian(ylim = c(0, 0.175)) +
      ggplot2::labs(title = "Train lateness quantiles",
                    subtitle = "dashed lines are the quantiles, shaded areas are the tails")
  }
  structure(out, class = "train")
}

#' Random generation for the train lateness distribution
#'
#' Draws a random sample using the w-F (probability integral transform)
#' theorem: uniform(0, 1) values are passed through the exact quantile
#' function [qtrain()].
#'
#' @param n A single whole number, the sample size (at least 1).
#' @param plot Logical. If `TRUE` (default) a ggplot2 density-scaled histogram
#'   of the sample with the true density overlaid is stored in the result.
#'
#' @return An object of class `"train"`: a list with
#'   \describe{
#'     \item{type}{`"sample"`.}
#'     \item{input}{The sample size `n`.}
#'     \item{value}{The random sample.}
#'     \item{sample_mean, sample_variance}{Sample summaries (variance is `NA` when `n = 1`).}
#'     \item{mean, variance, sd, median, mode}{True summaries, for comparison.}
#'     \item{plot}{A ggplot object (only when `plot = TRUE`).}
#'   }
#'
#' @seealso [dtrain()], [ptrain()], [qtrain()]
#' @export
#'
#' @examples
#' set.seed(4753)
#' r <- rtrain(10000)
#' r$sample_mean
#' r$sample_variance
#' r
rtrain <- function(n, plot = TRUE) {
  if (!is.numeric(n) || length(n) != 1 || is.na(n) || !is.finite(n) || n < 1 || n != round(n)) {
    stop("n must be a single whole number >= 1", call. = FALSE)
  }
  check_flag(plot, "plot")
  s <- qtrain(stats::runif(n), plot = FALSE)$value
  sv <- if (n > 1) stats::var(s) else NA_real_

  out <- c(list(type = "sample", input = n, value = s,
                sample_mean = mean(s), sample_variance = sv),
           train_moments)
  if (plot) {
    out$plot <- train_base_plot() +
      ggplot2::geom_histogram(
        data = data.frame(x = s),
        ggplot2::aes(x = .data$x, y = ggplot2::after_stat(.data$density)),
        inherit.aes = FALSE, binwidth = 0.25, boundary = -5,
        fill = matcha[["light"]], colour = "white"
      ) +
      ggplot2::geom_line(colour = matcha[["dark"]], linewidth = 1.1) + # redraw the curve on top
      ggplot2::labs(
        title = sprintf("Random sample of n = %d", as.integer(n)),
        subtitle = sprintf("sample mean %.3f (true 0), sample variance %.3f (true 5)",
                           mean(s), sv)
      )
  }
  structure(out, class = "train")
}

#' Print a train result
#'
#' Shows the values, the tail probabilities (for [ptrain()]), the true
#' summaries of the distribution and, if present, the plot.
#'
#' @param x An object of class `"train"` from [dtrain()], [ptrain()],
#'   [qtrain()] or [rtrain()].
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @export
#'
#' @examples
#' print(ptrain(1, plot = FALSE))
print.train <- function(x, ...) {
  cat("Train lateness distribution:", x$type, "\n")
  if (x$type == "sample") {
    cat("  n =", x$input, "\n  first values:", format(utils::head(x$value, 6), digits = 4), "\n")
    cat(sprintf("  sample mean %.4f, sample variance %.4f\n", x$sample_mean, x$sample_variance))
  } else {
    tab <- data.frame(x$input, x$value)
    names(tab) <- c(switch(x$type, density = "x", cdf = "q", quantile = "p"), "value")
    print(tab, row.names = FALSE)
  }
  if (x$type == "cdf") {
    cat(sprintf("  lower tail P(Y <= q): %s\n  upper tail P(Y > q):  %s\n",
                paste(round(x$lower, 4), collapse = ", "),
                paste(round(x$upper, 4), collapse = ", ")))
  }
  cat(sprintf("  E(Y) = %g, V(Y) = %g, SD(Y) = %.4f\n", x$mean, x$variance, x$sd))
  if (!is.null(x$plot)) print(x$plot)
  invisible(x)
}
