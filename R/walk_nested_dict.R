#' Recursively Walk Through a Nested Dictionary
#'
#' This function performs a recursive search through a nested dictionary-like structure in R,
#' looking for a specific term and collecting its values. It's useful for extracting specific pieces
#' of data from complex, deeply nested results.
#'
#' @param my_result The nested dictionary-like structure to search through.
#' @param term The term to search for within the nested dictionary.
#' @param outputs An initially empty list to store the results of the search, default is an empty list.
#' @param depth The current depth of the recursion, default is 0.
#' @param maxdepth The maximum depth to recurse, default is 25.
#'   If exceeded, the function issues a warning and returns NULL.
#' @return A list of values associated with the term found in the nested dictionary.
#'   Returns NULL if the term is not found or if maximum recursion depth is exceeded.
#' @importFrom methods is

walk_nested_dict <- function(my_result, term, outputs = list(), depth = 0, maxdepth = 25) {
  if (depth > maxdepth) {
    warning('Maximum recursion depth exceeded. Returned NULL for the search results, try increasing the maxdepth argument.')
    return(NULL)
  }

  depth <- depth + 1

  if (is.list(my_result)) {
    if (term %in% names(my_result)) {
      outputs[[length(outputs) + 1]] <- my_result[[term]]
    } else {
      for (item in my_result) {
        outputs <- walk_nested_dict(item, term, outputs, depth, maxdepth)
      }
    }
  } else if (is(my_result, "list")) {
    for (item in my_result) {
      outputs <- walk_nested_dict(item, term, outputs, depth, maxdepth)
    }
  }

  if (length(outputs) > 0) {
    return(outputs)
  } else {
    return(NULL)
  }
}

# Example usage
# nested_dict <- list(a = 1, b = list(c = 2, d = list(e = 3, f = list(g = 4))))
# term <- "e"
# results <- walk_nested_dict(nested_dict, term)
# print(results)
