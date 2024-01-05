# Comparison Types (similar to Enum in Python)
ComparisonType <- c(
  "GREATER" = "greater",
  "GREATER_OR_EQUAL" = "greater_or_equal",
  "EQUAL" = "equals",
  "NOT_EQUAL" = "not_equal",
  "LESS_OR_EQUAL" = "less_or_equal",
  "LESS" = "less"
)

# Default Operator Class
DefaultOperator <- function(value) {
  list(
    value = value
    )
}

# Exact Match Operator Class
ExactMatchOperator <- function(attribute, value) {
  list(
    attribute = attribute,
    value = value,
    operator = "exact_match"
  )
}

# In Operator Class
InOperator <- function(attribute, values) {
  list(
    attribute = attribute,
    operator = "in",
    values = values
  )
}

# Contains Words Operator Class
ContainsWordsOperator <- function(attribute, value) {
  list(
    attribute = attribute,
    operator = "contains_words",
    value = value
  )
}

# Contains Phrase Operator Class
ContainsPhraseOperator <- function(attribute, value) {
  list(
    attribute = attribute,
    operator = "contains_phrase",
    value = value
  )
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


}

# Range Operator Class
RangeOperator <- function(attribute, from_value, to_value, include_lower = TRUE, include_upper = TRUE, negation = FALSE) {
  list(
    operator = "range",
    attribute = attribute,
    # from_value = from_value,
    # to_value = to_value,
    # include_lower = include_lower,
    # include_upper = include_upper,
    negation = negation,
    value = list(from = from_value, to = to_value)
  )
}

# Exists Operator Class
ExistsOperator <- function(attribute) {
  list(
    attribute = attribute,
    operator = "exists"

  )
}

# Text Search Operator Classes - for type checking and compatibility
TextSearchOperator <- list(
  "default", "exact_match", "in", "contains_words",
  "contains_phrase", "ComparisonOperator", "range", "exists",
  "greater", "greater_or_equal",  "equals", "not_equal", "less_or_equal", "less"
)
