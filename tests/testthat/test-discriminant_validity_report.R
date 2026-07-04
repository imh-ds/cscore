test_that("reporting set keeps higher-order and independent lower-order constructs", {
  df <- data.frame(
    x1 = c(1, 2, 3, 4, 5, 6),
    x2 = c(1, 2, 3, 4, 5, 6),
    x3 = c(2, 3, 4, 5, 6, 7),
    x4 = c(6, 5, 4, 3, 2, 1),
    x5 = c(5, 4, 3, 2, 1, 0)
  )

  cl <- composite_list(
    lower_a = c("x1", "x2"),
    lower_b = "x3",
    lower_c = c("x4", "x5"),
    higher_g = c("lower_a", "lower_b")
  )

  result <- average_score(
    data = df,
    composite_list = cl,
    return_metrics = TRUE
  )

  expect_equal(result$validity$composite, c("lower_c", "higher_g"))
  expect_equal(rownames(result$fornell_larcker), c("lower_c", "higher_g"))
  expect_equal(colnames(result$htmt), c("lower_c", "higher_g"))
})

test_that("Fornell-Larcker diagonal and PASS/FAIL align with validity summary", {
  df <- data.frame(
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    x2 = c(1, 2, 3, 4, 5, 6, 7, 8),
    x3 = c(8, 7, 6, 5, 4, 3, 2, 1),
    x4 = c(8, 7, 6, 5, 4, 3, 2, 1)
  )

  cl <- composite_list(
    first = c("x1", "x2"),
    second = c("x3", "x4")
  )

  result <- average_score(
    data = df,
    composite_list = cl,
    return_metrics = TRUE
  )

  expect_equal(unname(diag(result$fornell_larcker)),
               result$validity$sqrt_ave,
               tolerance = 1e-10)

  expected_corr <- abs(stats::cor(result$data$first, result$data$second))
  expect_equal(result$validity$max_interconstruct_corr,
               c(expected_corr, expected_corr),
               tolerance = 1e-10)
  expect_true(all(result$validity$fornell_larcker %in% c("PASS", "FAIL")))
})

test_that("HTMT cutoff can be overridden", {
  df <- data.frame(
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    x2 = c(1, 2, 3, 4, 5, 6, 7, 8),
    x3 = c(1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1),
    x4 = c(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2, 8.2)
  )

  cl <- composite_list(
    first = c("x1", "x2"),
    second = c("x3", "x4")
  )

  default_result <- average_score(
    data = df,
    composite_list = cl,
    return_metrics = TRUE
  )

  relaxed_result <- average_score(
    data = df,
    composite_list = cl,
    htmt_cutoff = 1.50,
    return_metrics = TRUE
  )

  expect_true(all(default_result$validity$htmt == "FAIL"))
  expect_true(all(relaxed_result$validity$htmt == "PASS"))
})

test_that("single-indicator constructs retain NA HTMT fields", {
  df <- data.frame(
    x1 = c(1, 2, 3, 4, 5, 6),
    x2 = c(1, 2, 3, 4, 5, 6),
    x3 = c(2, 2, 3, 3, 4, 4)
  )

  cl <- composite_list(
    multi = c("x1", "x2"),
    single = "x3"
  )

  result <- average_score(
    data = df,
    composite_list = cl,
    return_metrics = TRUE
  )

  single_row <- result$validity[result$validity$composite == "single", ]
  expect_true(is.na(single_row$max_htmt))
  expect_true(is.na(single_row$htmt))
  expect_true("single" %in% rownames(result$fornell_larcker))
})

test_that("export_metrics writes validity appendix sheets", {
  skip_if_not_installed("openxlsx")

  df <- data.frame(
    x1 = c(1, 2, 3, 4, 5, 6),
    x2 = c(1, 2, 3, 4, 5, 6),
    x3 = c(6, 5, 4, 3, 2, 1),
    x4 = c(6, 5, 4, 3, 2, 1)
  )

  cl <- composite_list(
    first = c("x1", "x2"),
    second = c("x3", "x4")
  )

  result <- average_score(
    data = df,
    composite_list = cl,
    return_metrics = TRUE
  )

  path <- tempfile(fileext = ".xlsx")
  export_metrics(result, digits = 3, file = path)

  expect_true(file.exists(path))
  expect_equal(
    openxlsx::getSheetNames(path),
    c("Metrics", "Validity", "Fornell-Larcker", "HTMT")
  )
})
