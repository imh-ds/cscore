#' Calculate Discriminant Validity Report
#'
#' @description Compute reporting-set discriminant-validity summaries,
#' Fornell-Larcker matrix output, and HTMT ratios after all composite scores
#' have already been calculated.
#'
#' @param data A data frame containing the final composite-score columns.
#' @param composite_list A \code{composite_list} object.
#' @param validity A data frame containing at least \code{composite} and
#'   \code{ave} columns.
#' @param htmt_cutoff Numeric HTMT threshold used for PASS/FAIL classification.
#'
#' @returns A list containing the reporting set, a construct-level
#'   discriminant-validity summary table, a Fornell-Larcker matrix, and an HTMT
#'   matrix.
#'
#' @keywords internal
calc_discriminant_validity_report <- function(
    data,
    composite_list,
    validity,
    htmt_cutoff = 0.90
) {

  reporting_set <- get_reporting_constructs(composite_list)

  if (length(reporting_set) == 0) {
    empty_summary <- data.frame(
      composite = character(),
      sqrt_ave = numeric(),
      max_interconstruct_corr = numeric(),
      fornell_larcker = character(),
      max_htmt = numeric(),
      htmt = character(),
      discriminant_validity = character()
    )

    return(list(
      reporting_set = reporting_set,
      discriminant_summary = empty_summary,
      fornell_larcker = matrix(numeric(), nrow = 0, ncol = 0),
      htmt = matrix(numeric(), nrow = 0, ncol = 0)
    ))
  }

  score_df <- data[, reporting_set, drop = FALSE]
  score_cor <- stats::cor(score_df, use = "pairwise.complete.obs")
  score_cor <- as.matrix(score_cor)

  if (length(reporting_set) == 1) {
    score_cor <- matrix(score_cor, nrow = 1, ncol = 1)
  }

  dimnames(score_cor) <- list(reporting_set, reporting_set)
  abs_score_cor <- abs(score_cor)

  ave_map <- stats::setNames(validity$ave, validity$composite)
  sqrt_ave <- sqrt(ave_map[reporting_set])

  fornell_larcker <- abs_score_cor
  diag(fornell_larcker) <- sqrt_ave

  max_interconstruct_corr <- vapply(
    seq_along(reporting_set),
    function(i) {
      if (ncol(abs_score_cor) <= 1) {
        return(NA_real_)
      }

      values <- abs_score_cor[i, -i]
      values <- values[is.finite(values)]
      if (length(values) == 0) NA_real_ else max(values)
    },
    numeric(1)
  )

  fornell_status <- vapply(
    seq_along(reporting_set),
    function(i) {
      classify_discriminant_check(
        lhs = sqrt_ave[[i]],
        rhs = max_interconstruct_corr[[i]],
        comparator = "greater"
      )
    },
    character(1)
  )

  htmt_matrix <- matrix(
    NA_real_,
    nrow = length(reporting_set),
    ncol = length(reporting_set),
    dimnames = list(reporting_set, reporting_set)
  )

  for (i in seq_along(reporting_set)) {
    for (j in seq_along(reporting_set)) {
      if (i < j) {
        htmt_value <- calc_htmt_pair(
          get_construct_indicators(data, composite_list, reporting_set[[i]]),
          get_construct_indicators(data, composite_list, reporting_set[[j]])
        )
        htmt_matrix[i, j] <- htmt_value
        htmt_matrix[j, i] <- htmt_value
      }
    }
  }

  max_htmt <- vapply(
    seq_along(reporting_set),
    function(i) {
      values <- htmt_matrix[i, -i]
      values <- values[is.finite(values)]
      if (length(values) == 0) NA_real_ else max(values)
    },
    numeric(1)
  )

  htmt_status <- vapply(
    seq_along(reporting_set),
    function(i) {
      classify_discriminant_check(
        lhs = max_htmt[[i]],
        rhs = htmt_cutoff,
        comparator = "less"
      )
    },
    character(1)
  )

  overall_status <- vapply(
    seq_along(reporting_set),
    function(i) {
      combine_discriminant_statuses(
        c(fornell_status[[i]], htmt_status[[i]])
      )
    },
    character(1)
  )

  discriminant_summary <- data.frame(
    composite = reporting_set,
    sqrt_ave = unname(sqrt_ave),
    max_interconstruct_corr = max_interconstruct_corr,
    fornell_larcker = empty_to_na(fornell_status),
    max_htmt = max_htmt,
    htmt = empty_to_na(htmt_status),
    discriminant_validity = empty_to_na(overall_status)
  )

  list(
    reporting_set = reporting_set,
    discriminant_summary = discriminant_summary,
    fornell_larcker = fornell_larcker,
    htmt = htmt_matrix
  )
}

#' @keywords internal
get_reporting_constructs <- function(composite_list) {
  higher_names <- names(composite_list[["higher"]])
  absorbed_lower <- unique(unname(unlist(composite_list[["higher"]])))
  independent_lower <- setdiff(names(composite_list[["lower"]]), absorbed_lower)
  reporting_candidates <- c(independent_lower, higher_names)

  unique(
    composite_list[["order"]][
      composite_list[["order"]] %in% reporting_candidates
    ]
  )
}

#' @keywords internal
get_construct_indicators <- function(data, composite_list, construct_name) {
  if (construct_name %in% names(composite_list[["higher"]])) {
    vars <- composite_list[["higher"]][[construct_name]]
  } else {
    vars <- composite_list[["lower"]][[construct_name]]
  }

  data[, vars, drop = FALSE]
}

#' @keywords internal
calc_htmt_pair <- function(x, y) {
  heterotrait <- abs(stats::cor(x, y, use = "pairwise.complete.obs"))
  numerator <- mean_finite(as.vector(heterotrait))

  monotrait_x <- mean_abs_monotrait_cor(x)
  monotrait_y <- mean_abs_monotrait_cor(y)

  if (!is.finite(numerator) ||
      !is.finite(monotrait_x) ||
      !is.finite(monotrait_y)) {
    return(NA_real_)
  }

  denominator <- sqrt(monotrait_x * monotrait_y)

  if (!is.finite(denominator) || denominator <= 0) {
    return(NA_real_)
  }

  numerator / denominator
}

#' @keywords internal
mean_abs_monotrait_cor <- function(df) {
  if (ncol(df) < 2) {
    return(NA_real_)
  }

  cor_mat <- abs(stats::cor(df, use = "pairwise.complete.obs"))
  values <- cor_mat[upper.tri(cor_mat)]
  mean_finite(values)
}

#' @keywords internal
mean_finite <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

#' @keywords internal
classify_discriminant_check <- function(lhs, rhs, comparator = c("greater", "less")) {
  comparator <- match.arg(comparator)

  if (!is.finite(lhs) || !is.finite(rhs)) {
    return("")
  }

  passed <- switch(
    comparator,
    greater = lhs > rhs,
    less = lhs < rhs
  )

  if (isTRUE(passed)) "PASS" else "FAIL"
}

#' @keywords internal
combine_discriminant_statuses <- function(statuses) {
  statuses <- statuses[nzchar(statuses)]

  if (length(statuses) == 0) {
    return("")
  }

  if (any(statuses == "FAIL")) {
    return("FAIL")
  }

  if (all(statuses == "PASS")) {
    return("PASS")
  }

  ""
}

#' @keywords internal
empty_to_na <- function(x) {
  x[x == ""] <- NA_character_
  x
}

