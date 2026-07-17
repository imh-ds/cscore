skip_if_not_installed("psych")

composite_list_simple <- composite_list(y = c("x1", "x2", "x3", "x4", "x5"))

# Moderately correlated 5-item Likert-style scale
set.seed(42)
n <- 50
latent <- rnorm(n)
disc_df <- data.frame(
  x1 = round(pmin(pmax(latent + rnorm(n, 0, 0.5) + 3, 1), 5)),
  x2 = round(pmin(pmax(latent + rnorm(n, 0, 0.5) + 3, 1), 5)),
  x3 = round(pmin(pmax(latent + rnorm(n, 0, 0.5) + 3, 1), 5)),
  x4 = round(pmin(pmax(latent + rnorm(n, 0, 0.5) + 3, 1), 5)),
  x5 = round(pmin(pmax(latent + rnorm(n, 0, 0.5) + 3, 1), 5))
)

test_that("PCA discriminant composite returns finite scores", {
  result <- discriminant_score(
    data = disc_df,
    composite_list = composite_list_simple,
    weight = "pca"
  )
  expect_true(all(is.finite(result$y)))
  expect_equal(length(result$y), n)
})

test_that("PCA discriminant composite returns metrics when requested", {
  result <- discriminant_score(
    data = disc_df,
    composite_list = composite_list_simple,
    weight = "pca",
    return_metrics = TRUE
  )
  expect_named(result, c("data", "metrics", "validity",
                         "discriminant_summary", "fornell_larcker", "htmt"))
  expect_true("y" %in% names(result$data))
})

test_that("IRT discriminant composite runs when mirt is available", {
  skip_if_not_installed("mirt")
  result <- discriminant_score(
    data = disc_df,
    composite_list = composite_list_simple,
    weight = "irt",
    seed = 42
  )
  expect_true(all(is.finite(result$y)))
})

test_that("discriminant family errors informatively when mirt is absent", {
  skip_if(requireNamespace("mirt", quietly = TRUE),
          "mirt is installed — cannot test missing-package guard")
  expect_error(
    discriminant_score(
      data = disc_df,
      composite_list = composite_list_simple,
      weight = "irt"
    ),
    regexp = "mirt"
  )
})

test_that("outcome-informed weighting warns about circularity", {
  skip_if_not_installed("glmnet")

  set.seed(7)
  nn <- 80
  f1 <- rnorm(nn); f2 <- 0.5 * f1 + rnorm(nn)
  mk <- function(l) round(pmin(pmax(3 + l + rnorm(nn, 0, 0.8), 1), 5))
  mdf <- data.frame(
    a1 = mk(f1), a2 = mk(f1), a3 = mk(f1),
    b1 = mk(f2), b2 = mk(f2), b3 = mk(f2),
    c1 = mk(f2), c2 = mk(f2), c3 = mk(f2)
  )
  cl <- composite_list(
    A = c("a1", "a2", "a3"),
    B = c("b1", "b2", "b3"),
    C = c("c1", "c2", "c3")
  )
  cm <- composite_model(link(from = c("A", "B"), to = "C"))

  # A model that triggers predictive reweighting must warn about circularity.
  expect_warning(
    suppressMessages(
      discriminant_score(mdf, cl, composite_model = cm, weight = "pca", seed = 1)
    ),
    regexp = "circular"
  )
})

test_that("no circularity warning without a composite_model", {
  expect_no_warning(
    suppressMessages(
      discriminant_score(
        data = disc_df,
        composite_list = composite_list_simple,
        weight = "pca"
      )
    )
  )
})

# Shared fixture for the composite_model bug-fix tests below.
make_model_data <- function() {
  set.seed(42)
  n <- 150
  f1 <- rnorm(n); f2 <- 0.5 * f1 + rnorm(n)
  mk <- function(l) round(pmin(pmax(3 + l + rnorm(n, sd = 0.8), 1), 5))
  df <- data.frame(
    a1 = mk(f1), a2 = mk(f1), a3 = mk(f1),
    b1 = mk(f2), b2 = mk(f2), b3 = mk(f2),
    c1 = mk(f2), c2 = mk(f2), c3 = mk(f2)
  )
  cl <- composite_list(
    A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3"), C = c("c1", "c2", "c3"),
    HI = c("A", "B")
  )
  list(df = df, cl = cl, cm = composite_model(link(from = "HI", to = "C")))
}

test_that("composite_model + return_metrics does not clobber the analysis name", {
  skip_if_not_installed("glmnet")
  fx <- make_model_data()

  captured <- NULL
  local_mocked_bindings(
    export_metrics = function(metrics, digits, name = NULL, file) {
      captured <<- name
    },
    .package = "cscore"
  )

  suppressWarnings(suppressMessages(
    discriminant_score(fx$df, fx$cl, composite_model = fx$cm, weight = "pca",
                       return_metrics = TRUE, file = "ignored.xlsx",
                       name = "MY STUDY", seed = 1)
  ))

  expect_equal(captured, "MY STUDY")
})

test_that("higher-order predictors in a model are PCA-weighted, not IRT", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("mirt")
  fx <- make_model_data()

  seen <- list()
  orig <- cscore:::calc_discriminant_composite
  local_mocked_bindings(
    calc_discriminant_composite = function(data, var, weight, ...) {
      seen[[length(seen) + 1L]] <<- list(var = var, weight = weight)
      orig(data = data, var = var, weight = weight, ...)
    },
    .package = "cscore"
  )

  suppressWarnings(suppressMessages(
    discriminant_score(fx$df, fx$cl, composite_model = fx$cm, weight = "irt",
                       seed = 1)
  ))

  # The higher-order composite HI is defined by composite names c("A", "B").
  hi_calls <- Filter(function(x) identical(sort(x$var), c("A", "B")), seen)
  expect_gt(length(hi_calls), 0)
  expect_true(all(vapply(hi_calls, function(x) x$weight == "pca", logical(1))))

  # Lower-order composites built from items keep the user-requested IRT weight.
  lo_calls <- Filter(function(x) all(x$var %in% c("a1", "a2", "a3")), seen)
  expect_true(all(vapply(lo_calls, function(x) x$weight == "irt", logical(1))))
})

test_that("GLM predictive weighting handles family = 'multinomial'", {
  skip_if_not_installed("glmnet")
  fx <- make_model_data()
  dat <- fx$df
  dat$grp <- factor(sample(c("x", "y", "z"), nrow(dat), replace = TRUE))

  score <- suppressWarnings(
    cscore:::calc_discriminant_composite(
      data = dat, var = c("a1", "a2", "a3"), weight = "pca",
      outcomes = "grp", pred_type = "glm", family = "multinomial",
      alpha = 0.5, nfolds = 5, seed = 1, return_metrics = FALSE
    )
  )

  expect_length(score, nrow(dat))
  expect_true(all(is.finite(score)))
})
