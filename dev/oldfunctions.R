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
#' Select samples prior to calculating rel_abund
#'
#' @param phy A phyloseq object .
#' @param smp_selection A character vector of sample selections.
#'
#' @return A tibble.
#' @export
#'
#' @examples
choose_samples_asv_phy <- function(phy, smp_selection ){

    asv_data_phy(phy) %>%
        filter(sample_id == smp_selection)
}

#' Helper function 1 to pool taxa below threshold
#'
#' Summarise the pooled group as a logical vector, also summarise
#' the mean rel_abund values to be used for ordering.
#'
#' @param rel_abund_tab A rel_abund table in tibble format.
#' @param threshold A numeric vector for the threshold.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' pool_taxon_thresh(rel_abund_tab, threshold)
pool_taxon_thresh <- function(rel_abund_tab, threshold = 0.2) {

    rel_abund_tab %>%
        dplyr::group_by(taxon) %>%
        dplyr::summarise(pool = max(rel_abund) < threshold,
                         mean = mean(rel_abund),
                         .groups = "drop")
}

#' Helper function 2 to pool taxa below threshold
#'
#' Pool the taxon that are below the threshold
#'
#' @param rel_abund_pool A pooled rel_abund table.
#' @param taxon A taxon column from rel_abund_pool.
#' @param threshold A numeric vector for the specified threshold.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' set_taxon_threshold(rel_abund_pool, taxon)
set_taxon_threshold <- function(rel_abund_pool, taxon, threshold = 0.2) {

    rel_abund_pool %>%
        dplyr::mutate(taxon = dplyr::if_else(pool, glue::glue("< {round(threshold, 2)}%"), taxon),
                      taxon = tidyr::replace_na(taxon, "Unclassified"))
}

