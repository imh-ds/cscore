test_that("is_discrete_variable honors the threshold for integer-like doubles", {
  f <- function(x, th = 10) cscore:::is_discrete_variable(x, threshold = th)

  # Factors / ordered / integer-typed vectors are discrete by type.
  expect_true(f(factor(c("a", "b", "c"))))
  expect_true(f(ordered(c("lo", "hi"))))
  expect_true(f(1:500L))

  # Low-cardinality integer-like double: discrete.
  expect_true(f(as.numeric(c(1, 2, 3, 2, 1))))

  # High-cardinality integer-like double (counts): NOT discrete, so callers
  # bin it rather than treating hundreds of values as categories.
  expect_false(f(as.numeric(1:300)))

  # Threshold is respected exactly.
  expect_true(f(as.numeric(1:10), th = 10))
  expect_false(f(as.numeric(1:11), th = 10))

  # Continuous and non-integer-like values are not discrete.
  expect_false(f(rnorm(1000)))
  expect_false(f(c(1.5, 2.5, 3.5)))

  # Empty / all-NA input.
  expect_false(f(as.numeric(c(NA, NA))))
})

test_that("NMI uses one entropy estimator: identical variables give NMI = 1", {
  skip_if_not_installed("infotheo")

  set.seed(1)
  v <- sample(1:4, 300, replace = TRUE)

  for (est in c("emp", "mm", "shrink", "sg")) {
    mi <- infotheo::mutinformation(v, v, method = est)
    e  <- infotheo::entropy(v, method = est)
    nmi_geo <- mi / sqrt(e * e)
    nmi_avg <- 2 * mi / (e + e)
    expect_equal(nmi_geo, 1, tolerance = 1e-8,
                 info = paste("geometric NMI, estimator", est))
    expect_equal(nmi_avg, 1, tolerance = 1e-8,
                 info = paste("average NMI, estimator", est))
  }
})

test_that("mutual_info scoring runs with a high-cardinality integer indicator", {
  skip_if_not_installed("infotheo")

  set.seed(3)
  n <- 200
  df <- data.frame(
    i1 = as.numeric(sample(1:5, n, replace = TRUE)),
    i2 = as.numeric(sample(1:5, n, replace = TRUE)),
    i3 = as.numeric(sample(1:5, n, replace = TRUE)),
    count = as.numeric(rpois(n, 50))     # high-cardinality integer-like double
  )
  cl <- composite_list(m = c("i1", "i2", "i3", "count"))

  res <- suppressWarnings(
    composite_score(df, cl, weight = "mutual_info", entropy = "mm")
  )
  expect_true("m" %in% names(res))
  expect_true(all(is.finite(res$m)))
})
