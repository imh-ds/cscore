
# ---------------------------------------------------------------------------
# Single-indicator composites
# ---------------------------------------------------------------------------

test_that("single-indicator composite returns the indicator unchanged (average)", {
  df <- data.frame(x1 = c(1.5, 2.5, 3.5), x2 = 1:3)
  cl <- composite_list(a = "x1", b = "x2")
  result <- average_score(data = df, composite_list = cl)
  expect_equal(result$a, df$x1)
  expect_equal(result$b, as.numeric(df$x2))
})

test_that("single-indicator composite returns the indicator unchanged (correlation)", {
  df <- data.frame(x1 = c(1.5, 2.5, 3.5))
  cl <- composite_list(a = "x1")
  result <- correlation_score(data = df, composite_list = cl, weight = "correlation")
  expect_equal(result$a, df$x1)
})

# ---------------------------------------------------------------------------
# Missing data incidence warnings (> 50 % of items)
# ---------------------------------------------------------------------------

test_that("warning fires when > 50 % of items are missing per respondent", {
  # 3-item composite; row 1 has 2/3 items missing (> 50 %)
  df <- data.frame(x1 = c(NA, 2), x2 = c(NA, 3), x3 = c(3, 4))
  cl <- composite_list(y = c("x1", "x2", "x3"))
  expect_warning(
    composite_score(data = df, composite_list = cl, weight = "average"),
    regexp = "50%"
  )
})

# ---------------------------------------------------------------------------
# Composite score structure
# ---------------------------------------------------------------------------

test_that("composite_score returns a data frame with the composite appended", {
  df <- data.frame(x1 = 1:5, x2 = 2:6, x3 = 3:7)
  cl <- composite_list(y = c("x1", "x2", "x3"))
  result <- composite_score(data = df, composite_list = cl, weight = "average")
  expect_true(is.data.frame(result))
  expect_true("y" %in% names(result))
  expect_equal(nrow(result), 5)
})

# ---------------------------------------------------------------------------
# Higher-order composites
# ---------------------------------------------------------------------------

test_that("higher-order composite is computed from lower-order scores", {
  df <- data.frame(x1 = 1:5, x2 = 2:6, y1 = 3:7, y2 = 4:8)
  cl <- composite_list(
    a  = c("x1", "x2"),
    b  = c("y1", "y2"),
    ab = c("a", "b")     # detected as higher-order: its indicators are composite names
  )
  result <- average_score(data = df, composite_list = cl)
  expect_true("ab" %in% names(result))
  expect_equal(result$ab, rowMeans(result[, c("a", "b")]), tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Reverse-keyed items produce negative weight warnings
# ---------------------------------------------------------------------------

test_that("negative correlation weight triggers warning", {
  df <- data.frame(
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    x2 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    x3 = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)  # reverse-keyed
  )
  cl <- composite_list(y = c("x1", "x2", "x3"))
  expect_warning(
    correlation_score(data = df, composite_list = cl, weight = "correlation"),
    regexp = "reverse-keyed"
  )
})

# ---------------------------------------------------------------------------
# AVE and rhoc are in [0, 1]
# ---------------------------------------------------------------------------

test_that("validity metrics are bounded in [0, 1]", {
  df <- data.frame(x1 = 1:10, x2 = 2:11, x3 = 3:12)
  cl <- composite_list(y = c("x1", "x2", "x3"))
  result <- average_score(data = df, composite_list = cl, return_metrics = TRUE)
  v <- result$validity
  expect_true(v$ave >= 0 && v$ave <= 1)
  expect_true(v$rhoc >= 0 && v$rhoc <= 1)
})
