test_that("dtrain matches the formula and is 0 off the support", {
  expect_equal(dtrain(0, plot = FALSE)$value, 0.15)
  expect_equal(dtrain(c(-6, 5, 6, Inf), plot = FALSE)$value, c(0, 0, 0, 0))
  expect_equal(integrate(function(x) dtrain(x, plot = FALSE)$value, -5, 5)$value, 1)
})

test_that("ptrain gives tails that add to 1", {
  expect_equal(ptrain(0, plot = FALSE)$value, 0.5)
  expect_equal(ptrain(c(-Inf, -5, 5, Inf), plot = FALSE)$value, c(0, 0, 1, 1))
  r <- ptrain(c(-2, 1, 3), plot = FALSE)
  expect_equal(r$lower + r$upper, c(1, 1, 1))
  expect_equal(ptrain(2, lower.tail = FALSE, plot = FALSE)$value, 1 - ptrain(2, plot = FALSE)$value)
})

test_that("qtrain inverts ptrain exactly", {
  p <- seq(0, 1, by = 0.01)
  expect_equal(ptrain(qtrain(p, plot = FALSE)$value, plot = FALSE)$value, p)
  expect_equal(qtrain(c(0, 0.5, 1), plot = FALSE)$value, c(-5, 0, 5))
  expect_equal(qtrain(0.75, plot = FALSE)$value, 1.736482, tolerance = 1e-6)
  expect_equal(qtrain(0.1, lower.tail = FALSE, plot = FALSE)$value,
               qtrain(0.9, plot = FALSE)$value)
})

test_that("rtrain gives a sample from the right distribution", {
  set.seed(4753)
  r <- rtrain(10000, plot = FALSE)
  expect_length(r$value, 10000)
  expect_true(all(r$value >= -5 & r$value <= 5))
  expect_equal(r$sample_mean, 0, tolerance = 0.1)
  expect_equal(r$sample_variance, 5, tolerance = 0.05)
})

test_that("bad input gives clear errors", {
  expect_error(dtrain("five"), "must be numeric")
  expect_error(qtrain(1.5), "between 0 and 1")
  expect_error(qtrain(-0.1), "between 0 and 1")
  expect_error(rtrain(-3), "whole number")
  expect_error(rtrain(2.5), "whole number")
  expect_error(rtrain(c(5, 6)), "whole number")
  expect_error(ptrain(1, lower.tail = "yes"), "TRUE or FALSE")
  expect_error(dtrain(numeric(0)), "at least one")
})

test_that("results have the train class, summaries and plots", {
  for (o in list(dtrain(1), ptrain(1), qtrain(c(0.025, 0.975)), rtrain(50))) {
    expect_s3_class(o, "train")
    expect_equal(o$variance, 5)
    expect_s3_class(o$plot, "ggplot")
    expect_output(print(o), "E(Y) = 0", fixed = TRUE)
  }
  expect_equal(qtrain(NA, plot = FALSE)$value, NA_real_)
})
