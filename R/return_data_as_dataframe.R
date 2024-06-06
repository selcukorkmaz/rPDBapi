#' Transform Response Data into a Dataframe
#'
#' This function converts response data from the RCSB Protein Data Bank (PDB) into a structured dataframe.
#' It handles null or empty responses and flattens the data for dataframe conversion.
#'
#' @param response A list containing the response data from a PDB query.
#' @param data_type A string indicating the type of data contained in the response.
#'   This parameter is not directly used in the function but might be relevant for context.
#' @param ids A vector of identifiers corresponding to the response data.
#' @return A dataframe constructed from the response data.
#'   Returns NULL if the response is null or empty.
#' @importFrom purrr map map_df discard %||%
#' @importFrom dplyr select_if bind_rows as_tibble
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom stats setNames
return_data_as_dataframe <- function(response, data_type, ids) {

  if (is.null(response) || length(response) == 0) {
    return(NULL)
  }

  data <- response$data[[1]]


  if(any(duplicated(names(unlist(data))))){

  simplify_list <- function(lst) {
    # Create an empty list to store the results
    simplified_lst <- list()

    # Iterate through each top-level element
    for (name in names(lst)) {
      top_element <- lst[[name]]
      # Create a new list for the top-level element
      simplified_top_element <- list()

      # Iterate through each second-level element and keep only the first child
      for (second_name in names(top_element)) {
        child <- top_element[[second_name]]
        if (is.list(child) && length(child) > 0) {
          # Keep only the first child of the second-level element
          simplified_top_element[[second_name]] <- list(child[[1]])
        } else {
          # If it's not a list or an empty list, just copy it as is
          simplified_top_element[[second_name]] <- child
        }
      }

      # Add the simplified top-level element to the result list
      simplified_lst[[name]] <- simplified_top_element
    }

    return(simplified_lst)
  }

  # Apply the function to your data list
  simplified_data <- simplify_list(data)


  lst = lapply(simplified_data, function(x){

    t(data.frame(unlist(x)))


  })

  # Identify all unique column names
  all_columns <- unique(unlist(lapply(lst, colnames)))

  # Standardize list items to have the same columns
  df_standardized <- lapply(lst, function(x) {
    missing_columns <- setdiff(all_columns, names(x))
    if (length(missing_columns) > 0) {
      missing_df <- as.data.frame(matrix(ncol = length(missing_columns), nrow = nrow(x)))
      colnames(missing_df) <- missing_columns
      x <- cbind(x, missing_df)
    }
    x[all_columns]  # Ensure consistent column order
  })

  # Bind rows into a single DataFrame
  df <- bind_rows(df_standardized, .id = "ID") %>% as_tibble()


  }else{

    flattened_data <- map(data, ~flatten(.x))
    df_cleaned <- discard(flattened_data, ~ length(.x) == 0)


    # Flatten each item and collect the unique set of keys
    all_keys <- unique(unlist(lapply(df_cleaned, function(item) names(flatten(item)))))

    if(is.null(all_keys)){

      stop("No result fetched from the RCSB PDB for the selected properties...")

    }else{

    # Initialize an empty list for rows
    rows <- list()

    # Iterate over each item, populating values for the union of keys
    for (id in names(df_cleaned)) {
      # Flatten the current item
      flat_item <- flatten(df_cleaned[[id]])

      # Prepare row values, defaulting to NA for missing keys
      row_values <- map(all_keys, ~ flat_item[[.]] %||% NA)

      # Combine ID with row values
      row <- c(ID = id, setNames(row_values, all_keys))

      # Append to the list of rows
      rows[[id]] <- row
    }

    # Convert the list of rows into a data frame
    df <- bind_rows(rows)

    # Optionally clean up column names by removing hierarchical prefixes, if desired
    clean_column_names <- function(names) {
      sapply(names, function(name) {
        components <- strsplit(name, "\\.", fixed = TRUE)[[1]]
        if (length(components) > 1) {
          paste(tail(components, n = 2), collapse = "_")  # Join the last two components for clarity
        } else {
          name
        }
      })
    }

    names(df) <- clean_column_names(names(df))

    }



  }

  return(df)
}
