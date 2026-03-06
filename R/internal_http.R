# Internal HTTP helpers used by exported request/search wrappers.

rpdbapi_http_request <- function(url, method = "GET", body = NULL, encode = "json", content_type_value = "application/json") {
  method <- toupper(method)

  if (method == "POST") {
    return(POST(url, body = body, encode = encode, content_type(content_type_value)))
  }

  if (method == "GET") {
    return(GET(url))
  }

  rpdbapi_abort(
    paste0("Unsupported HTTP method: ", method),
    class = "rPDBapi_error_invalid_input",
    function_name = "rpdbapi_http_request",
    method = method,
    url = url
  )
}

rpdbapi_http_success <- function(response) {
  http_status(response)$category == "Success"
}

rpdbapi_http_status_message <- function(response) {
  http_status(response)$message
}

rpdbapi_http_status_code <- function(response) {
  http_status(response)$status
}

rpdbapi_response_text <- function(response) {
  content(response, "text", encoding = "UTF-8")
}

rpdbapi_response_parsed <- function(response) {
  content(response, "parsed")
}
