context("input_validation")

test_that("median composite stops on non-numeric inputs", {
  df <- data.frame(x1 = letters[1:5], x2 = 1:5, x3 = 1:5, stringsAsFactors = FALSE)
  cl <- composite_list(y = c("x1", "x2", "x3"))
  expect_error(
    median_score(data = df, composite_list = cl, weight = "median"),
    regexp = "Non-numeric"
  )
})

test_that("mutual info composite stops on non-numeric inputs", {
  df <- data.frame(x1 = letters[1:5], x2 = 1:5, x3 = 1:5, stringsAsFactors = FALSE)
  cl <- composite_list(y = c("x1", "x2", "x3"))
  expect_error(
    information_score(data = df, composite_list = cl),
    regexp = "Non-numeric"
  )
})

test_that("discriminant composite stops on non-numeric inputs", {
  df <- data.frame(x1 = letters[1:5], x2 = 1:5, x3 = 1:5, stringsAsFactors = FALSE)
  cl <- composite_list(y = c("x1", "x2", "x3"))
  expect_error(
    discriminant_score(data = df, composite_list = cl, weight = "pca"),
    regexp = "Non-numeric"
  )
})

test_that("mutual info warns on zero variance items", {
  df <- data.frame(x1 = rep(3, 10), x2 = 1:10, x3 = 1:10)
  cl <- composite_list(y = c("x1", "x2", "x3"))
  expect_warning(
    information_score(data = df, composite_list = cl),
    regexp = "Zero-variance"
  )
})

test_that("discriminant PCA warns on zero-variance items and handles them", {
  skip_if_not_installed("psych")
  df <- data.frame(x1 = rep(3, 10), x2 = 1:10, x3 = 1:10, x4 = 1:10)
  cl <- composite_list(y = c("x1", "x2", "x3", "x4"))
  expect_warning(
    discriminant_score(data = df, composite_list = cl, weight = "pca"),
    regexp = "Zero-variance"
  )
})
