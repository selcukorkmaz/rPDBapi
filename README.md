# rPDBapi: A Comprehensive R Package Interface for Accessing the Protein Data Bank


## Introduction
`rPDBapi` is an R package designed to provide seamless access to the RCSB Protein Data Bank (PDB). It simplifies the retrieval and analysis of 3D structural data of large biological molecules, essential for bioinformatics and structural biology research. This package leverages the PDB's XML-based API to facilitate custom queries, data retrieval, and advanced search capabilities within the R programming environment.

## Features
* **User-Friendly Interface:** Simplifies access to PDB data for the R community.
* **Custom Queries:** Streamlines the process of crafting custom queries for efficient data retrieval.
* **Advanced Search Capabilities:** Includes specialized search functions for PubMed IDs, organisms, experimental methods, protein structure similarities, and more.
* **Data Retrieval:** Facilitates downloading of PDB files in various formats and extraction of FASTA sequences.
* **Integration with R:** Provides functions for data manipulation and analysis directly within R, enhancing research workflows.


## Installation
You can install the stable version of `rPDBapi` from CRAN:

```r
install.packages("rPDBapi", repos = "http://cran.us.r-project.org")
```

To install the development version from GitHub:

```r
devtools::install_github("selcukorkmaz/rPDBapi")
```

## Usage

**Loading the Package**
```r
library(rPDBapi)
```

**Retrieving PDB IDs**
Retrieve PDB IDs related to a specific term, such as "hemoglobin":

```r
pdbs <- query_search(search_term = "hemoglobin")
head(pdbs)
```

**Advanced Searches**
Search by PubMed ID:

```r
pdbs <- query_search(search_term = 32453425, query_type = "PubmedIdQuery")
pdbs
```

**Search by source organism:**

```r
pdbs <- query_search(search_term = '7227', query_type = 'TreeEntityQuery')
head(pdbs)
```

**Search by experimental method:**

```r
pdbs <- query_search(search_term = 'SOLID-STATE NMR', query_type='ExpTypeQuery')
head(pdbs)
```

**Data Retrieval**
Fetch data based on user-defined IDs and properties:

```r
properties <- list(rcsb_entry_info = c("molecular_weight"), exptl = "method", rcsb_accession_info = "deposit_date")
ids <- query_search("CRISPR")
df <- data_fetcher(id = ids, data_type = "ENTRY", properties = properties, return_as_dataframe = TRUE)
df
```

**Describing Chemical Compounds**
Retrieve comprehensive descriptions of chemical compounds:

```r
chem_desc <- describe_chemical('ATP')
chem_desc$rcsb_chem_comp_descriptor$smiles
```

**Retrieving PDB Files**
Download PDB files in various formats:

```r
pdb_file <- get_pdb_file(pdb_id = "4HHB", filetype = "cif")
head(pdb_file$atom)
```

**Additional Functions**
`get_info`: Retrieve detailed information about a specific PDB entry.
`get_fasta_from_rcsb_entry`: Fetch FASTA sequences for specified PDB entry IDs.

## Documentation
For more detailed examples and usage, please refer to the package documentation.

## Output Contracts
Core functions return typed objects to make downstream behavior explicit:

* `query_search()`
  * `return_type = "entry"`: character vector with class `rPDBapi_query_ids`
  * otherwise: parsed payload with class `rPDBapi_query_response`
* `perform_search()`
  * default ID output: class `rPDBapi_search_ids`
  * `return_with_scores = TRUE`: class `rPDBapi_search_scores`
  * `return_raw_json_dict = TRUE`: class `rPDBapi_search_raw_response`
* `fetch_data()`
  * validated payload with class `rPDBapi_fetch_response`
* `data_fetcher()`
  * `return_as_dataframe = TRUE`: data frame with class `rPDBapi_dataframe`
  * `return_as_dataframe = FALSE`: class `rPDBapi_fetch_response`

Error signaling uses typed conditions (e.g., `rPDBapi_error_malformed_response`,
`rPDBapi_error_unsupported_mapping`) for reliable programmatic handling.

## Backward-Compatible Aliases
* Search return types:
  * `NONPOLYMER_ENTITY` and `NON_POLYMER_ENTITY` map to the same API return type.
  * `CHEMICAL_COMPONENT` maps to `MOL_DEFINITION`.
* Citation fields:
  * `citation` and `rcsb_primary_citation` are resolved compatibly in
    `find_results()` and `find_papers()`.

## Testing
By default, the test suite runs only deterministic unit tests (no network calls):

```r
Sys.setenv(RPDBAPI_RUN_LIVE = "false")
testthat::test_dir("tests/testthat")
```

Live API integration tests are opt-in:

```r
Sys.setenv(RPDBAPI_RUN_LIVE = "true", NOT_CRAN = "true")
testthat::test_dir("tests/testthat")
```

## Authors
* Selcuk Korkmaz - Trakya University, Department of Biostatistics
* Bilge Eren Yamasan - Trakya University, Department of Biophysics

## License
This package is licensed under the MIT License.
