
# Example Usage

### Fetch entries using PDB IDs


data_fetcher <- data_fetcher(id = c("4HHB", "12CA", "3PQR"), data_type = "ENTRY",
                            properties = list(exptl =  c("method", "details"), cell = c("length_a", "length_b", "length_c")),
                            return_as_dataframe = T)



data_fetcher

### Fetch Assemblies

data_fetcher <- data_fetcher(id = c("4HHB-1", "12CA-1", "3PQR-1"), data_type = "ASSEMBLY",
                            properties = list(rcsb_assembly_info =  c("entry_id", "assembly_id", "polymer_entity_instance_count")),
                            return_as_dataframe = T)



data_fetcher

### Fetch Polymer Entities

data_fetcher <- data_fetcher(id = c("2CPK_1","3WHM_1","2D5Z_1"), data_type = "POLYMER_ENTITY",
                            properties = list(rcsb_entity_source_organism = c("ncbi_taxonomy_id", "ncbi_scientific_name"),
                                              rcsb_cluster_membership = c("cluster_id", "identity")),
                            return_as_dataframe = T)



data_fetcher

### Fetch Polymer Entity Instance

data_fetcher <- data_fetcher(id = c("4HHB.A", "12CA.A", "3PQR.A"), data_type = "POLYMER_ENTITY_INSTANCE",
                            properties = list(rcsb_polymer_instance_annotation = c("annotation_id", "name", "type")),
                            return_as_dataframe = T)



data_fetcher


### Fetch Branched Entity

data_fetcher <- data_fetcher(id = c("5FMB_2", "6L63_3"), data_type = "BRANCHED_ENTITY",
                            properties = list(pdbx_entity_branch = "type", pdbx_entity_branch_descriptor = c("type", "descriptor")),
                            return_as_dataframe = T)



data_fetcher


### Fetch Chemical Components

data_fetcher <- data_fetcher(id = c("NAG","EBW"), data_type = "CHEMICAL_COMPONENT",
                            properties = list(chem_comp = c("type", "formula_weight","name","formula"),
                                              rcsb_chem_comp_info = c("initial_release_date")),
                            return_as_dataframe = T)



data_fetcher
