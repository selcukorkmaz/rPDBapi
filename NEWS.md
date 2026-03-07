## rPDBapi 3.0.0 (Major Release)

- Added CI workflows in `.github/workflows/ci.yml` and
  `.github/workflows/live-tests.yml`.
- Added explicit output contracts in `R/01_contracts.R`.
- Strengthened search, fetch, and data-frame return handling across:
  `R/query_search.R`, `R/perform_search.R`, `R/fetch_data.R`,
  `R/data_fetcher.R`, and `R/return_data_as_dataframe.R`.
- Improved result handling and compatibility logic in `R/find_results.R` and
  `R/find_papers.R`.
- Expanded deterministic test coverage substantially, including:
  API contracts, request encoding, scan parameters, group serialization,
  return-type handling, fetch behavior, citation compatibility, and
  `get_pdb_file()` struct-fact parsing.
- Updated `README.md` to document output contracts, compatibility aliases, and
  opt-in live tests.


## rPDBapi 2.1.1

### 2024-11-05 `166ecd7` version 2.1.1

- Extended `get_pdb_file()` and its documentation.
- Improved `query_search()` and its manual page.

## 2.1.x documentation and API cleanup

### 2024-09-06 `e414827` update documentation

- Refined `query_search()` and `send_api_request()` behavior and documentation.
- Updated related Rd files for consistency.

### 2024-09-05 `f33fa77` updated to version 2.1

- Added `R/get_pdb_api_url.R`, `R/handle_api_errors.R`,
  `R/parse_response.R`, and `R/send_api_request.R`.
- Improved request routing and response parsing across fetch/search helpers.
- Updated `data_fetcher()`, `describe_chemical()`, `get_info()`,
  `get_fasta_from_rcsb_entry()`, `get_pdb_file()`, and `search_graphql()`.
- Refreshed documentation for the new low-level request helpers.

## rPDBapi 2.0.0

### 2024-09-03 `ad1efa6` upgraded to version 2.0

- Large package-wide revision affecting search, fetch, parsing, and operator
  layers.
- Expanded and revised manual pages across most exported functions.
- Added a broad `testthat` suite for operators, search helpers, fetch helpers,
  and file/data retrieval.
- Tightened behavior in `perform_search()`, `query_search()`,
  `return_data_as_dataframe()`, and `get_pdb_file()`.

## rPDBapi 1.0.0 

### 2024-06-06 `b1aca34` fixed major errors

- Fixed major issues across `query_search()`, `perform_search()`,
  `get_pdb_file()`, `return_data_as_dataframe()`, `find_results()`,
  `find_papers()`, and related documentation.
- Renamed the project file from `pdbAPI.Rproj` to `rPDBapi.Rproj`.

### 2024-06-06 README and namespace follow-ups

- `7474cba`, `04ed3c7`, `ee0d286`, `561ca06`
  - Expanded and corrected README examples and package guidance.
- `0100977`
  - Small update to `R/query_search.R`.
- `548cfd8`
  - Small namespace export adjustment.

## rPDBapi 0.9.0 (2024-01-17)

### 2024-01-17 `217eae0`, `316c2c7`, `adfd67d`, `8f09f11`

- Added examples and fixes around `perform_search()`.
- Simplified exports and removed some earlier helper artifacts.
- Continued cleanup of `get_pdb_file()`, parsing, and search utilities.

### 2024-01-16 `fa36be0` RD files updated

- Large documentation pass across package functions and classes.
- Added `R/rPDBapi-package.R`.
- Expanded namespace exports and manual pages for operators, query helpers,
  fetch helpers, and package-level documentation.

### 2024-01-16 `7cec68d` minor fixes

- Adjusted `get_pdb_file()` implementation and documentation.

### 2024-01-15 `599f3e8` major update

- Introduced or expanded key helpers such as `add_property()`,
  `data_fetcher()`, `fetch_data()`, `find_papers()`, `find_results()`,
  `generate_json_query()`, `perform_search()`, and
  `return_data_as_dataframe()`.
- Removed older transitional files like `R/data_types.R`, `R/fasta_client.R`,
  and `R/search.R` in favor of a clearer package structure.
- Improved `search_client()` and GraphQL-related behavior.

### 2024-01-08 `67be19a`, `43c02d5`, `15b38b4`

- Added `get_fasta_from_rcsb_entry()` and expanded data type handling.
- Added early GraphQL support in `R/search_graphql.R`.
- Iterated on data-type definitions and related internals.

### 2024-01-05 `df8c648`, `7da031a`, `1379ab7`, `d11e442`

- Built out search client behavior and operator logic.
- Added then later refactored FASTA-related client functionality.
- Refined sequence, structure, and text operator behavior.

### 2024-01-03 `bfdda1c`

- Replaced `R/query.R` with `R/query_search.R`.
- Refactored `search_client.R` and related search helpers.

### 2024-01-02 `2291362`

- Added early query and nested-dictionary traversal helpers.

## rPDBapi 0.1.0 (2023-12-28)

### 2023-12-28 `b9aafb0` first commit

- Established package structure with `DESCRIPTION`, `NAMESPACE`, project file,
  `renv`, and initial search/get helpers.

### 2023-12-19 `bf37f8f` Initial commit

- Created the repository with the initial README.
