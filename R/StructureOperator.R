# Structure Search Modes (similar to Enum in Python)
StructureSearchMode <- c(
  "STRICT_SHAPE_MATCH" = "strict_shape_match",
  "RELAXED_SHAPE_MATCH" = "relaxed_shape_match"
)

# Structure Operator Class
StructureOperator <- function(pdb_entry_id, assembly_id = 1, search_mode = "STRICT_SHAPE_MATCH") {
  list(
    pdb_entry_id = pdb_entry_id,
    assembly_id = assembly_id,
    search_mode = search_mode,
    to_dict = function() {
      list(
        value = list(entry_id = pdb_entry_id, assembly_id = as.character(assembly_id)),
        operator = StructureSearchMode[[search_mode]]
      )
    }
  )
}

