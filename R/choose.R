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

