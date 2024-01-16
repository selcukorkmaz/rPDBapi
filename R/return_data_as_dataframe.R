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
#' @importFrom purrr map map_df
#' @importFrom dplyr select_if
#' @importFrom magrittr %>%
#' @export
return_data_as_dataframe <- function(response, data_type, ids) {
  if (is.null(response) || length(response) == 0) {
    return(NULL)
  }

  data <- response$data[[1]]

  flattened_data <- map(data, ~flatten(.x))
  df <- map_df(flattened_data, ~as.data.frame(t(.x)), .id = "ID") %>% select_if(~ !any(is.na(.)))

  return(df)
}
