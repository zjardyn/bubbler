devtools::load_all()

#
# counts <- physeq1@otu_table %>% t()
# sample_id <- rownames(counts)
# counts_tb <- counts %>%
#     as.data.frame() %>%
#     as_tibble()
# counts_tb %>% add_column(sample_id) %>%
#     relocate(sample_id)
#
#
# physeq1@tax_table %>%
#     as.data.frame()
#
# asv_data_phy(physeq1)
#
# physeq1@tax_table %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column(var = "asv") %>%
#     tibble::as_tibble()
#
# meta <- physeq1@sam_data
# sample_id <- rownames(meta)
# meta_tb <- meta %>%
#     tibble::as_tibble()
# meta_tb %>% tibble::add_column(sample_id) %>%
#     dplyr::relocate(sample_id)

asv_data_phy(physeq1)
taxa_data_phy(physeq1)
meta_data_phy(physeq1)
