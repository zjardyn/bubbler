utils::globalVariables(c("sample_id", "count", ".", "asv", "level"))

#' Generate a relative abundance table from a phyloseq object.
#'
#' @param phy A phyloseq object containing an otu_table and tax_table.
#' @param taxa_level A character value specifying the taxa level from Domain to species.
#' @param meta_data A logical value specifying if metadata should be included from the phyloseq object.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' rel_abund(phy = physeq1)
rel_abund <- function(phy, taxa_level = "Phylum", meta_data = FALSE) {

    taxonomy <- taxa_data_phy(phy)
    metadata <- meta_data_phy(phy)

    rel_abund <- asv_data_phy(phy) %>%
        tidyr::pivot_longer(-sample_id,
                            names_to = "asv",
                            values_to = "count") %>%
        dplyr::mutate(rel_abund = count/sum(count)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-count) %>%
        dplyr::inner_join(., taxonomy, by =  "asv")

    taxa_lvls <- taxonomy %>%
        dplyr::select(-asv) %>%
        colnames()

    rel_abund_taxa <- rel_abund %>%
        tidyr::pivot_longer(taxa_lvls,
                            names_to = "level",
                            values_to = "taxon") %>%
        dplyr::filter(level == taxa_level)

    if(meta_data == TRUE){
       dplyr::inner_join(rel_abund_taxa, metadata, by = "sample_id")
    } else {
       rel_abund_taxa
    }
}
