#' Generate a relative abundance table from a phyloseq object, summed across samples
#'
#' @param phy A phyloseq object containing an otu_table and tax_table.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' rel_abund(phy = physeq1)
rel_abund <- function(phy, taxa_level = "Phylum") {

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

    rel_abund %>%
        tidyr::pivot_longer(taxa_lvls,
                            names_to = "level",
                            values_to = "taxon") %>%
        dplyr::filter(level == taxa_level) %>%
        dplyr::inner_join(., metadata, by = "sample_id")
}

#' Generate a relative abundance table from a phyloseq object, summed across a variable
#'
#' @param phy A phyloseq object containing an otu_table, tax_table, and sample_data.
#' @param var A character vector containing the variable present in sample_data.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' rel_abund_var(phy = physeq1, var = "Location")
rel_abund_var <- function(phy, var) {

    taxonomy <- taxa_data_phy(phy)
    metadata <- meta_data_phy(phy)

    rel_abund <- asv_data_phy(phy) %>%
        tidyr::pivot_longer(-sample_id,
                            names_to = "asv",
                            values_to = "count") %>%
        dplyr::inner_join(., metadata, by =  "sample_id") %>%
        dplyr::inner_join(., taxonomy, by =  "asv") %>%
        dplyr::group_by(!!rlang::sym(var)) %>%
        dplyr::mutate(rel_abund = count/sum(count)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-count)


    taxa_lvls <- taxonomy %>%
        dplyr::select(-asv) %>%
        colnames()

    rel_abund %>%
        tidyr::pivot_longer(taxa_lvls,
                            names_to = "level",
                            values_to = "taxon")
}
