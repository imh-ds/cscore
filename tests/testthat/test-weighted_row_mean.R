test_that("weighted_row_mean equals rowMeans with equal weights", {
  df <- data.frame(x1 = 1:4, x2 = 2:5, x3 = 3:6)
  result <- cscore:::weighted_row_mean(df, c(1, 1, 1))
  expect_equal(result, rowMeans(df))
})

test_that("weighted_row_mean respects unequal weights", {
  df <- data.frame(x1 = c(0, 0), x2 = c(0, 0), x3 = c(1, 2))
  # weights: x3 gets all the weight
  result <- cscore:::weighted_row_mean(df, c(0, 0, 1))
  expect_equal(result, c(1, 2))
})

test_that("weighted_row_mean handles NA by row-wise renormalization", {
  df <- data.frame(x1 = c(1, NA), x2 = c(NA, 2), x3 = c(3, 3))
  result <- cscore:::weighted_row_mean(df, c(1, 1, 1))
  # row 1: mean(1, 3) = 2; row 2: mean(2, 3) = 2.5
  expect_equal(result, c(2, 2.5))
})

test_that("weighted_row_mean returns NA for all-missing rows", {
  df <- data.frame(x1 = c(NA, 1), x2 = c(NA, 2), x3 = c(NA, 3))
  result <- cscore:::weighted_row_mean(df, c(1, 1, 1))
  expect_true(is.na(result[1]))
  expect_false(is.na(result[2]))
})

test_that("weighted_row_mean is stable with mixed-sign weights", {
  df <- data.frame(x1 = c(3), x2 = c(1), x3 = c(2))
  # weights = c(1, -1, 0): row_abs_sum = 2; w_norm = c(0.5, -0.5, 0)
  # result = 3*0.5 + 1*(-0.5) + 2*0 = 1.0
  result <- cscore:::weighted_row_mean(df, c(1, -1, 0))
  expect_equal(result, 1.0)
})
