#' Generate a relative abundance table from a phyloseq object, summed across samples
#'
#' @param phy A phyloseq object containing an otu_table and tax_table.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' rel_abund(phy = physeq1)
rel_abund <- function(phy) {

    taxonomy <- taxa_data_phy(phy)

    rel_abund <- asv_data_phy(phy) %>%
        tidyr::pivot_longer(-sample_id,
                            names_to = "asv",
                            values_to = "count") %>%
        # dplyr::inner_join(., taxonomy, by =  "asv") %>%
        # what should we group by? nothing! if you want the stack
        # geom to show you rel_abund between samples
        # dplyr::group_by(sample_id) %>%
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
                            values_to = "taxon")
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

#' Filter a rel_abund table at a specific taxonomic level
#'
#' @param rel_abund A rel_abund table in tibble format.
#' @param taxon_level A character vector of the taxonomic level.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' choose_taxa_level(rel_abund, "Phylum")
choose_taxa_level <- function(rel_abund, taxon_level = "Phylum") {
   rel_abund %>%
        dplyr::filter(level == taxon_level)
}

#' Filter a rel_abund table for a selection of samples
#'
#' @param rel_abund A rel_abund table in tibble format.
#' @param smp_selection A character vector of the selected samples.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' choose_samples(rel_abund, c("Smp1", "Smp2", "Smp3"))
choose_samples <- function(rel_abund, smp_selection) {
    rel_abund %>%
        dplyr::filter(sample_id == paste(smp_selection, sep = " || "))
}
