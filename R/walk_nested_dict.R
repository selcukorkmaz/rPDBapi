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
