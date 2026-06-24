test_that("safe_normalize scales by mean absolute value", {
  x <- c(1, 2, 3)
  result <- cscore:::safe_normalize(x)
  expect_equal(mean(abs(result)), 1)
  expect_equal(result, x / mean(abs(x)))
})

test_that("safe_normalize handles mixed-sign weights", {
  x <- c(-1, 1)
  result <- cscore:::safe_normalize(x)
  # mean(abs(c(-1,1))) = 1, so result unchanged
  expect_equal(result, c(-1, 1))
  expect_equal(mean(abs(result)), 1)
})

test_that("safe_normalize falls back to equal weights when all-zero", {
  result <- cscore:::safe_normalize(c(0, 0, 0))
  expect_equal(result, c(1, 1, 1))
})

test_that("safe_normalize falls back when near-zero denominator", {
  # mixed signs that nearly cancel
  x <- c(1e-320, -1e-320, 0)
  result <- cscore:::safe_normalize(x)
  expect_equal(result, c(1, 1, 1))
})
