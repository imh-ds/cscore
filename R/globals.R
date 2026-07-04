# Silence R CMD check notes for tidy-eval column references used in dplyr verbs.
utils::globalVariables(c(
  "discriminant_validity",
  "fornell_larcker",
  "htmt",
  "max_htmt",
  "max_interconstruct_corr",
  "sqrt_ave"
))
