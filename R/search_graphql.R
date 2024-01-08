library(httr)

RSCB_GRAPHQL_URL <- "https://data.rcsb.org/graphql?query="

search_graphql <- function(graphql_json_query) {
  response <- POST(url = RSCB_GRAPHQL_URL, body = graphql_json_query, encode = "json")

  if (http_status(response)$category != "Success") {
    warning(paste("It appears the request failed with:", content(response, "text", encoding = "UTF-8")))
    stop("Request failed.")
  }

  return(content(response, "parsed"))
}

# Example Usage
graphql_json_query <- list(query = "{entries(entry_ids: [\"4LZA\", \"5RU3\"]){cell {volume, angle_beta}, exptl {method}}}")
result <- search_graphql(graphql_json_query)
print(result)
