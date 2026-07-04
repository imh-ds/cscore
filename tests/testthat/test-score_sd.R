composite_list_simple <- composite_list(y = c("x1", "x2", "x3"))

# Equal-SD items → equal weights → composite equals rowMeans
equal_sd_df <- data.frame(
  x1 = c(1, 2, 3, 4, 5),
  x2 = c(2, 3, 4, 5, 6),
  x3 = c(3, 4, 5, 6, 7)
)

test_that("sd_upweight equals rowMeans when all SDs are equal", {
  result <- sd_score(
    data = equal_sd_df,
    composite_list = composite_list_simple,
    weight = "sd_upweight"
  )
  expect_equal(result$y, rowMeans(equal_sd_df[, c("x1", "x2", "x3")]),
               tolerance = 1e-10)
})

test_that("sd_downweight equals rowMeans when all SDs are equal", {
  result <- sd_score(
    data = equal_sd_df,
    composite_list = composite_list_simple,
    weight = "sd_downweight"
  )
  expect_equal(result$y, rowMeans(equal_sd_df[, c("x1", "x2", "x3")]),
               tolerance = 1e-10)
})

test_that("non-numeric input stops with an informative error", {
  df <- data.frame(x1 = letters[1:5], x2 = 1:5, x3 = 1:5,
                   stringsAsFactors = FALSE)
  expect_error(
    sd_score(data = df, composite_list = composite_list_simple,
             weight = "sd_upweight"),
    regexp = "Non-numeric"
  )
})

test_that("zero-variance item in sd_downweight stops with an informative error", {
  df <- data.frame(x1 = rep(3, 5), x2 = 1:5, x3 = 1:5)
  expect_error(
    sd_score(data = df, composite_list = composite_list_simple,
             weight = "sd_downweight"),
    regexp = "Zero-variance"
  )
})

test_that("zero-variance item in sd_upweight warns and receives zero weight", {
  df <- data.frame(x1 = rep(3, 5), x2 = c(1, 2, 3, 4, 5),
                   x3 = c(2, 3, 4, 5, 6))
  expect_warning(
    sd_score(data = df, composite_list = composite_list_simple,
             weight = "sd_upweight"),
    regexp = "Zero-variance"
  )
})

test_that("sd composite returns metrics when requested", {
  result <- sd_score(
    data = equal_sd_df,
    composite_list = composite_list_simple,
    weight = "sd_upweight",
    return_metrics = TRUE
  )
  expect_named(result, c("data", "metrics", "validity",
                         "discriminant_summary", "fornell_larcker", "htmt"))
})
