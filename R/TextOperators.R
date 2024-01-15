
# Default Operator Class
DefaultOperator <- function(value) {
  res <- list(
    value = value
    )

  structure(res, class = c("DefaultOperator", class(res)))

}

# Exact Match Operator Class
ExactMatchOperator <- function(attribute, value) {
  res <- list(
    attribute = attribute,
    value = value,
    operator = "exact_match"
  )

  structure(res, class = c("ExactMatchOperator", class(res)))

}

# In Operator Class
InOperator <- function(attribute, value) {
  res <- list(
    attribute = attribute,
    operator = "in",
    value = value
  )

  structure(res, class = c("InOperator", class(res)))

}

# Contains Words Operator Class
ContainsWordsOperator <- function(attribute, value) {
  res <- list(
    attribute = attribute,
    operator = "contains_words",
    value = value
  )

  structure(res, class = c("ContainsWordsOperator", class(res)))

}

# Contains Phrase Operator Class
ContainsPhraseOperator <- function(attribute, value) {
  res <- list(
    attribute = attribute,
    operator = "contains_phrase",
    value = value
  )

  structure(res, class = c("ContainsPhraseOperator", class(res)))

}

# Comparison Operator Class
ComparisonOperator <- function(attribute, value, comparison_type) {
  # list(
  #   attribute = attribute,
  #   value = value,
  #   comparison_type = comparison_type,

      if (ComparisonType[[comparison_type]] == "not_equal") {
        param_dict <- list(operator = "equals", negation = TRUE)
      } else {
        param_dict <- list(operator = ComparisonType[[comparison_type]])
      }

      param_dict$attribute <- attribute
      param_dict$value <- value

      param_dict

      structure(param_dict, class = c("ComparisonOperator", class(param_dict)))

}

# Range Operator Class
RangeOperator <- function(attribute, from_value, to_value, include_lower = TRUE, include_upper = TRUE, negation = FALSE) {
  res <- list(
    operator = "range",
    attribute = attribute,
    # from_value = from_value,
    # to_value = to_value,
    # include_lower = include_lower,
    # include_upper = include_upper,
    negation = negation,
    value = list(from = from_value, to = to_value)
  )

  structure(res, class = c("RangeOperator", class(res)))

}

# Exists Operator Class
ExistsOperator <- function(attribute) {
  res <- list(
    attribute = attribute,
    operator = "exists"

  )

  structure(res, class = c("SequenceOperator", class(res)))

}


