meta_data_phy <- function(phy) {
    sample_id <- rownames(phyloseq::sample_data(phy))
    phyloseq::sample_data(phy) %>%
        tibble::as_tibble() %>%
        tibble::add_column(sample_id)
}
