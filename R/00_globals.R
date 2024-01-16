FASTA_BASE_URL <- "https://www.rcsb.org/fasta/entry/"
PDB_DOWNLOAD_BASE_URL <- "https://files.rcsb.org/download/"
RSCB_GRAPHQL_URL <- "https://data.rcsb.org/graphql?query="


# Text Search Operator Classes - for type checking and compatibility
TextSearchOperator <- list(
  "default", "exact_match", "in", "contains_words",
  "contains_phrase", "ComparisonOperator", "range", "exists",
  "greater", "greater_or_equal",  "equals", "not_equal", "less_or_equal", "less"
)

ComparisonType <- c(
  "GREATER" = "greater",
  "GREATER_OR_EQUAL" = "greater_or_equal",
  "EQUAL" = "equals",
  "NOT_EQUAL" = "not_equal",
  "LESS_OR_EQUAL" = "less_or_equal",
  "LESS" = "less"
)

DataType <- c(
  ENTRY = "entries",
  POLYMER_ENTITY = "polymer_entities",
  BRANCHED_ENTITY = "branched_entities",
  NONPOLYMER_ENTITY = "nonpolymer_entities",
  POLYMER_ENTITY_INSTANCE = "polymer_entity_instances",
  BRANCHED_ENTITY_INSTANCE = "branched_entity_instances",
  NONPOLYMER_ENTITY_INSTANCE = "nonpolymer_entity_instances",
  ASSEMBLY = "assemblies",
  CHEMICAL_COMPONENT = "chem_comps"
)

SEARCH_URL_ENDPOINT <- "https://search.rcsb.org/rcsbsearch/v2/query"

LogicalOperator <- c("AND" = "and", "OR" = "or")

ReturnType <- c(
  ENTRY = "entry",
  ASSEMBLY = "assembly",
  POLYMER_ENTITY = "polymer_entity",
  NON_POLYMER_ENTITY = "non_polymer_entity",
  POLYMER_INSTANCE = "polymer_instance"
)

SearchService <- c(
  BASIC_SEARCH = "full_text",
  TEXT = "text",
  SEQUENCE = "sequence",
  SEQMOTIF = "seqmotif",
  STRUCTURE = "structure",
  CHEMICAL = "chemical"
)

# Combine TEXTSEARCH_OPERATORS with other operators
SEARCH_OPERATORS <- c(TextSearchOperator, "sequence", "StructureOperator", "SeqMotifOperator")

FASTA_BASE_URL <- "https://www.rcsb.org/fasta/entry/"

DescriptorMatchingCriterion <- c(
  GRAPH_STRICT = "graph-strict",
  GRAPH_RELAXED = "graph-relaxed",
  GRAPH_RELAXED_STEREO = "graph-relaxed-stereo",
  FINGERPRINT_SIMILARITY = "fingerprint-similarity"
)



# Defines types of sequences for the SeqMotifOperator
SequenceType <- c("DNA" = "pdb_dna_sequence", "RNA" = "pdb_rna_sequence", "PROTEIN" = "pdb_protein_sequence")

# Defines types of patterns for the SeqMotifOperator
PatternType <- c("SIMPLE" = "simple", "PROSITE" = "prosite", "REGEX" = "regex")

# Sequence Types
SequenceType <- c("DNA" = "pdb_dna_sequence", "RNA" = "pdb_rna_sequence", "PROTEIN" = "pdb_protein_sequence")

# Structure Search Modes (similar to Enum in Python)
StructureSearchMode <- c(
  "STRICT_SHAPE_MATCH" = "strict_shape_match",
  "RELAXED_SHAPE_MATCH" = "relaxed_shape_match"
)
