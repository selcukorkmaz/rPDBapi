library(httr)
library(jsonlite)

#' Search the Protein Data Bank using the REST API
#'
#' This function performs a search of the Protein Data Bank using the REST API. It handles JSON data and HTTP requests.
#' In case of a failed retrieval, it tries multiple attempts and respects API rate limits.
#'
#' @param scan_params Parameters for the search query in a list format.
#' @param url The URL of the Protein Data Bank API endpoint.
#' @param return_type Specifies the type of return value, e.g., "entry".
#' @param num_attempts Number of attempts to try again in case of a failed retrieval. Default is 1.
#' @param sleep_time Time to wait between requests in seconds. Default is 0.5.
#' @return Returns a list of identifiers if return_type is "entry", or the raw response value otherwise.
#' @importFrom httr POST
#' @importFrom jsonlite fromJSON toJSON
#' @export

search = function(num_attempts = 1, sleep_time = 0.5) {
  query_text <- toJSON(scan_params, auto_unbox = TRUE, pretty = TRUE)
  for (attempt in 1:num_attempts) {
    response <- POST(url, body = query_text, encode = "json")
    if (!is.null(response) && http_status(response)$category == "Success") {
      response_val <- fromJSON(content(response, "text"))

      if (return_type == "entry") {
        idlist <- walk_nested_dict(response_val, "identifier", maxdepth = 25, outputs = list())
        return(idlist)
      } else {
        return(response_val)
      }
    }
    Sys.sleep(sleep_time)
  }

  warning("Retrieval failed, returning NULL")
  return(NULL)
}

search <- function(scan_params, url, return_type, num_attempts = 1, sleep_time = 0.5) {

  query_text <- toJSON(scan_params, auto_unbox = TRUE, pretty = TRUE)

  if("url" %in% names(scan_params)){

    url = scan_params$url

  }

  for (attempt in 1:num_attempts) {
    response <- POST(url, body = query_text, encode = "json")
    if (http_status(response)$category != "Success") {
      if (attempt == num_attempts) {
        warning("Retrieval failed, returning NULL")
        return(NULL)
      }
      Sys.sleep(sleep_time)
    } else {
      break
    }
  }

  response_val <- fromJSON(content(response, "text"))

  if (return_type == "entry") {
    idlist <- walk_nested_dict(response_val, "identifier", maxdepth = 25, outputs = vector())
    return(idlist)
  } else {
    return(response_val)
  }
}

scan_params = query("actin network")
return_type = "entry"
url = "https://search.rcsb.org/rcsbsearch/v2/query?json="
num_attempts = 1
sleep_time = 0.5
search(scan_params = scan_params, url = url, return_type = return_type)
