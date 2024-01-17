#' Fetch RCSB PDB Data Based on Specified Criteria
#'
#' This function fetches data based on a given identifier (ID), data type, and a set of properties.
#' It can return the data either in its original format or as a dataframe.
#' The function integrates several steps including validating IDs, generating a JSON query,
#' fetching the data, and formatting the response.
#'
#' @param id An identifier or a list of identifiers for the data to be fetched.
#' @param data_type A string specifying the type of data to fetch.
#'   Default is "ENTRY".
#' @param properties A list or dictionary of properties to be included in the data fetching process.
#' @param return_as_dataframe A boolean indicating whether to return the response as a dataframe.
#'   Default is TRUE.
#' @return Depending on the value of 'return_as_dataframe',
#'   this function returns either a dataframe or data in its original format.
#' @importFrom purrr flatten
#' @export
data_fetcher <- function(id = NULL, data_type = "ENTRY", properties = NULL, return_as_dataframe = TRUE) {

  properties <- add_property(properties)
  # print(properties)
  json_query <- generate_json_query(id, data_type, properties)
  # print(json_query)
  response <- fetch_data(json_query, data_type, id)
  # print(response)

  if(return_as_dataframe){

    response <- return_data_as_dataframe(response, data_type, id)

  }

  return(response)

}
