#' Convert RCSB PDB Response Data into a Dataframe
#'
#' The `return_data_as_dataframe` function transforms the response data obtained from a query to the RCSB Protein Data Bank (PDB) into a structured dataframe.
#' This function handles various scenarios, including responses with duplicate names, null or empty responses, and nested data structures.
#' It ensures that the resulting dataframe is consistently formatted and ready for downstream analysis.
#'
#' @param response A list containing the response data from a PDB query. This list is expected to be structured according to the RCSB PDB GraphQL or REST API specifications.
#' @param data_type A string indicating the type of data contained in the response (e.g., "ENTRY", "POLYMER_ENTITY"). This parameter is primarily used for contextual information and does not directly influence the function's operations.
#' @param ids A vector of identifiers corresponding to the response data. These IDs are used to label the resulting dataframe, ensuring that each row corresponds to a specific query identifier.
#'
#' @return A dataframe constructed from the response data, where each row corresponds to an identifier from the `ids` vector and each column represents a data field from the response.
#'         If the response is null or empty, the function returns `NULL`.
#'
#' @importFrom purrr map map_df discard %||%
#' @importFrom dplyr select_if bind_rows as_tibble pull select
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @importFrom stats setNames
#'
#' @details
#' The `return_data_as_dataframe` function is designed to provide a flexible and robust mechanism for converting PDB query responses into dataframes. It addresses several common challenges in handling API responses, such as:
#' \describe{
#'   \item{Null or Empty Responses}{If the response is null or contains no data, the function immediately returns `NULL`, avoiding unnecessary processing.}
#'   \item{Duplicate Names Handling}{The function detects and manages scenarios where the response contains duplicated names. It simplifies such lists by keeping only the first occurrence of each duplicated element, ensuring that the final dataframe has unique column names.}
#'   \item{Recursive Flattening}{The function flattens nested lists within the response, ensuring that all relevant data is captured in a single-level dataframe structure. This is particularly useful for complex responses that contain deeply nested data elements.}
#'   \item{Consistent Column Naming}{After processing the data, the function ensures that column names are consistent and do not retain unnecessary prefixes. This makes the resulting dataframe easier to interpret and work with in subsequent analyses.}
#' }
#'
#' @note
#' The function is equipped to handle responses with varying degrees of complexity. It is recommended to provide valid `ids` corresponding to the query to ensure that the dataframe rows are correctly labeled.
#'
#' @export

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

    if(!is.null(names(lst))){
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
    }else{

      for (name in length(lst)) {
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

    # Assuming the list 'data' is already defined as previously mentioned

    # Define a recursive flatten function
    recursive_flatten <- function(x) {
      if (is.list(x)) {
        return(unlist(lapply(x, recursive_flatten), recursive = FALSE))
      } else {
        return(x)
      }
    }

    # Flatten each item in the list recursively
    flattened_data <- map(data, recursive_flatten)

    # Discard empty items
    df_cleaned <- discard(flattened_data, ~ length(.x) == 0)

    # Flatten each item and collect the unique set of keys
    all_keys <- unique(unlist(lapply(df_cleaned, names)))

    if (length(all_keys) == 0) {
      stop("No result fetched from the RCSB PDB for the selected properties...")
    } else {
      # Initialize an empty list for rows
      rows <- list()

      # Iterate over each item, populating values for the union of keys
      # Check if df_cleaned has named elements or not
      if (!is.null(names(df_cleaned))) {
        for (id in names(df_cleaned)) {
          # Flatten the current item
          flat_item <- df_cleaned[[id]]

          # Convert all elements to character type
          flat_item <- lapply(flat_item, as.character)

          # Prepare row values, defaulting to NA for missing keys
          row_values <- map(all_keys, ~ purrr::pluck(flat_item, ., .default = NA))

          # Combine ID with row values
          row <- c(ID = id, setNames(row_values, all_keys))

          # Append to the list of rows
          rows[[id]] <- row
        }
      } else {
        for (id in 1:length(df_cleaned)) {
          # Flatten the current item
          flat_item <- df_cleaned[[id]]

          # Convert all elements to character type
          flat_item <- lapply(flat_item, as.character)

          # Prepare row values, defaulting to NA for missing keys
          row_values <- map(all_keys, ~ purrr::pluck(flat_item, ., .default = NA))

          # Combine ID with row values
          row <- c(ID = id, setNames(row_values, all_keys))

          # Append to the list of rows
          rows[[id]] <- row
        }
      }



      # Convert the list of rows into a data frame
      df <- bind_rows(rows)

      # names(df) <- clean_column_names(names(df), common_prefix)

      # # Display the dataframe
      # print(df)
    }




  }


  remove_prefix <- function(df) {
    new_names <- gsub("^[^.]+\\.", "", colnames(df))
    colnames(df) <- new_names
    return(df)
  }

  # Find the common prefix in the column names, excluding 'ID'
  df <- remove_prefix(df)

  if(identical(pull(df, 1), pull(df,2))){


    df <- df %>% select(-1)

  }


  return(df)
}
