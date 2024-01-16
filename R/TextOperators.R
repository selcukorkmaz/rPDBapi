#' Create a Default Search Operator
#'
#' Constructs a DefaultOperator object for use in general search operations within the RCSB PDB.
#' This operator is a basic search operator with a single value.
#'
#' @param value The value to be used in the search operation.
#' @return An object of class 'DefaultOperator' representing the default search operator.
#' @export
DefaultOperator <- function(value) {
  res <- list(
    value = value
    )

  structure(res, class = c("DefaultOperator", class(res)))

}

#' Create an Exact Match Search Operator
#'
#' Constructs an ExactMatchOperator object for precise search operations in the RCSB PDB.
#' It matches an exact attribute value.
#'
#' @param attribute The attribute to match.
#' @param value The exact value to search for.
#' @return An object of class 'ExactMatchOperator'.
#' @export
ExactMatchOperator <- function(attribute, value) {
  res <- list(
    attribute = attribute,
    value = value,
    operator = "exact_match"
  )

  structure(res, class = c("ExactMatchOperator", class(res)))

}

#' Create an Inclusion Search Operator
#'
#' Constructs an InOperator object for search operations where the attribute value must be within a specified set.
#'
#' @param attribute The attribute to be evaluated.
#' @param value The set of values to include in the search.
#' @return An object of class 'InOperator'.
#' @export
InOperator <- function(attribute, value) {
  res <- list(
    attribute = attribute,
    operator = "in",
    value = value
  )

  structure(res, class = c("InOperator", class(res)))

}

#' Create a Contains Words Search Operator
#'
#' Constructs a ContainsWordsOperator object for search operations that look for attributes containing certain words.
#'
#' @param attribute The attribute to be evaluated.
#' @param value The words to search for in the attribute.
#' @return An object of class 'ContainsWordsOperator'.
#' @export
ContainsWordsOperator <- function(attribute, value) {
  res <- list(
    attribute = attribute,
    operator = "contains_words",
    value = value
  )

  structure(res, class = c("ContainsWordsOperator", class(res)))

}

#' Create a Contains Phrase Search Operator
#'
#' Constructs a ContainsPhraseOperator object for search operations that look for attributes containing a specific phrase.
#'
#' @param attribute The attribute to be evaluated.
#' @param value The phrase to search for in the attribute.
#' @return An object of class 'ContainsPhraseOperator'.
#' @export
ContainsPhraseOperator <- function(attribute, value) {
  res <- list(
    attribute = attribute,
    operator = "contains_phrase",
    value = value
  )

  structure(res, class = c("ContainsPhraseOperator", class(res)))

}

#'Create a Comparison Search Operator
#'
#' Constructs a ComparisonOperator object for search operations that perform comparison checks on attribute values.
#'
#' @param attribute The attribute to be compared.
#' @param value The value to compare against.
#' @param comparison_type A string specifying the type of comparison (e.g., 'equal', 'greater_than').
#' @return An object of class 'ComparisonOperator'.
#' @export
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

#' Create a Range Search Operator
#'
#' Constructs a RangeOperator object for search operations that specify a range for attribute values.
#'
#' @param attribute The attribute to be evaluated within a range.
#' @param from_value The starting value of the range.
#' @param to_value The ending value of the range.
#' @param include_lower Boolean to include the lower bound in the range.
#' @param include_upper Boolean to include the upper bound in the range.
#' @param negation Boolean to negate the range condition.
#' @return An object of class 'RangeOperator'.
#' @export
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

#' Create an Existence Search Operator
#'
#' Constructs an ExistsOperator object for search operations to check the existence of an attribute.
#'
#' @param attribute The attribute whose existence is to be checked.
#' @return An object of class 'ExistsOperator'.
#' @export
ExistsOperator <- function(attribute) {
  res <- list(
    attribute = attribute,
    operator = "exists"

  )

  structure(res, class = c("SequenceOperator", class(res)))

}


