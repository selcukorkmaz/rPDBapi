#' Create a Default Search Operator
#'
#' Constructs a `DefaultOperator` object for use in general search operations within the RCSB PDB.
#' This operator is used when a simple, non-specific search operation is needed, based on a single value.
#' The `DefaultOperator` can be employed in scenarios where the search criteria do not require complex conditions
#' or additional logic.
#'
#' @param value The value to be used in the search operation. This is the key criterion for the search
#'              and should be a valid string or numeric value that the search will use as the matching term.
#' @return An object of class `DefaultOperator` representing the default search operator.
#'         This object can be used directly in query formulations or further manipulated within complex search logic.
#' @examples
#' # Create a basic search operator with a specific value
#' operator <- DefaultOperator("4HHB")
#' print(operator)
#' @export

DefaultOperator <- function(value) {
  res <- list(
    value = value
    )

  structure(res, class = c("DefaultOperator", class(res)))

}

#' Create an Exact Match Search Operator
#'
#' Constructs an `ExactMatchOperator` object for precise search operations within the RCSB PDB.
#' This operator is designed to match an exact attribute value, making it ideal for searches where
#' specificity is required. For example, if you need to find all entries that exactly match a certain
#' attribute value, this operator will ensure only those precise matches are returned.
#'
#' @param attribute The attribute to match. This should be the name of the field within the RCSB PDB
#'                  that you want to search against.
#' @param value The exact value to search for. This is the specific value of the attribute you are interested in.
#'              The search will return only those records where the attribute exactly matches this value.
#' @return An object of class `ExactMatchOperator`. This object can be used in search queries
#'         to retrieve precise matches within the RCSB PDB database.
#' @examples
#' # Search for entries with an exact match to a given attribute
#' operator <- ExactMatchOperator(attribute = "rcsb_entry_info.resolution_combined",
#'                                 value = "2.0")
#' print(operator)
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
#' Constructs an `InOperator` object for search operations where the attribute value must be within a specified set.
#' This operator is useful when the search criteria require the attribute to match one of several possible values.
#' It can handle multiple potential matches and is ideal for scenarios where multiple values are acceptable.
#'
#' @param attribute The attribute to be evaluated. This should be the field within the RCSB PDB
#'                  that you want to search against.
#' @param value The set of values to include in the search. This should be a vector of possible values
#'              that the attribute can match.
#' @return An object of class `InOperator` that can be used in search queries to retrieve entries
#'         where the attribute matches any of the specified values.
#' @examples
#' # Search for entries where the attribute matches one of several values
#' operator <- InOperator(attribute = "rcsb_entity_source_organism.taxonomy_lineage.name",
#'                        value = c("Homo sapiens", "Mus musculus"))
#' print(operator)
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
#' Constructs a `ContainsWordsOperator` object for search operations that look for attributes containing specific words.
#' This operator is particularly useful for text-based searches where the goal is to find entries that include
#' particular keywords or phrases within a specified attribute.
#'
#' @param attribute The attribute to be evaluated. This should be the text field within the RCSB PDB
#'                  that you want to search against.
#' @param value The words to search for in the attribute. This can be a single word or a set of words,
#'              and the search will return entries containing any of these words in the specified attribute.
#' @return An object of class `ContainsWordsOperator` that can be used in search queries to retrieve entries
#'         where the attribute contains the specified words.
#' @examples
#' # Search for entries containing specific words in an attribute
#' operator <- ContainsWordsOperator(attribute = "rcsb_primary_citation.title",
#'                                   value = "crystal structure")
#' print(operator)
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
#' Constructs a `ContainsPhraseOperator` object for search operations that look for attributes containing a specific phrase.
#' This operator is ideal for scenarios where the search needs to be more precise than just individual words,
#' such as finding an exact phrase within a text attribute.
#'
#' @param attribute The attribute to be evaluated. This should be the text field within the RCSB PDB
#'                  that you want to search against.
#' @param value The phrase to search for in the attribute. The search will look for this exact sequence of words
#'              within the specified attribute.
#' @return An object of class `ContainsPhraseOperator` that can be used in search queries to retrieve entries
#'         where the attribute contains the specified phrase.
#' @examples
#' # Search for entries containing a specific phrase in an attribute
#' operator <- ContainsPhraseOperator(attribute = "rcsb_primary_citation.title",
#'                                     value = "molecular dynamics")
#' print(operator)
#' @export

ContainsPhraseOperator <- function(attribute, value) {
  res <- list(
    attribute = attribute,
    operator = "contains_phrase",
    value = value
  )

  structure(res, class = c("ContainsPhraseOperator", class(res)))

}

#' Create a Comparison Search Operator
#'
#' Constructs a `ComparisonOperator` object for search operations that perform comparison checks on attribute values.
#' This operator allows for evaluating attributes using comparison operators such as 'equal', 'greater_than',
#' or 'less_than', making it suitable for numerical and date-based searches.
#'
#' @param attribute The attribute to be compared. This should be the field within the RCSB PDB that you want to evaluate.
#' @param value The value to compare against. This is the reference value for the comparison.
#' @param comparison_type A string specifying the type of comparison (e.g., 'equal', 'greater_than', 'less_than').
#'                        Supported comparison types are 'equal', 'not_equal', 'greater_than', 'less_than', etc.
#' @return An object of class `ComparisonOperator` that can be used in search queries to retrieve entries
#'         where the attribute meets the specified comparison criteria.
#' @examples
#' # Search for entries where an attribute equals a specific value
#' operator <- ComparisonOperator(attribute = "rcsb_entry_info.resolution_combined",
#'                                 value = 2.0, comparison_type = "EQUAL")
#' operator
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
#' Constructs a `RangeOperator` object for search operations that specify a range for attribute values.
#' This operator is particularly useful for filtering results based on numeric or date ranges,
#' such as finding entries with resolution between specific values or dates within a certain range.
#'
#' @param attribute The attribute to be evaluated within a range. This should be the numeric or date field
#'                  within the RCSB PDB that you want to search against.
#' @param from_value The starting value of the range. This is the lower bound of the range.
#' @param to_value The ending value of the range. This is the upper bound of the range.
#' @param include_lower Boolean indicating whether to include the lower bound in the range. Default is TRUE.
#' @param include_upper Boolean indicating whether to include the upper bound in the range. Default is TRUE.
#' @param negation Boolean indicating whether to negate the range condition. Default is FALSE.
#' @return An object of class `RangeOperator` that can be used in search queries to retrieve entries
#'         where the attribute falls within the specified range.
#' @examples
#' # Search for entries within a specific range of resolution
#' operator <- RangeOperator(attribute = "rcsb_entry_info.resolution_combined",
#'                           from_value = 1.5, to_value = 2.5)
#' print(operator)
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
#' Constructs an `ExistsOperator` object for search operations to check the existence of an attribute.
#' This operator is useful in queries where you need to ensure that a certain attribute is present
#' within the entries being searched, regardless of its value.
#'
#' @param attribute The attribute whose existence is to be checked. This should be the field within the RCSB PDB
#'                  that you want to ensure is present in the search results.
#' @return An object of class `ExistsOperator` that can be used in search queries to retrieve entries
#'         where the specified attribute exists.
#' @examples
#' # Search for entries where a specific attribute exists
#' operator <- ExistsOperator(attribute = "rcsb_primary_citation.doi")
#' print(operator)
#' @export

ExistsOperator <- function(attribute) {
  res <- list(
    attribute = attribute,
    operator = "exists"

  )

  structure(res, class = c("ExistsOperator", class(res)))

}


