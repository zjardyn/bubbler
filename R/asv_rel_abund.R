rel_abund <- function(phy) {

    taxonomy <- phyloseq::tax_table(phy) %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "asv") %>%
        tibble::as_tibble()

    rel_abund <- phyloseq::otu_table(phy) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "sample_id") %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(-sample_id, names_to = "asv", values_to = "count") %>%
        dplyr::inner_join(., taxonomy, by =  "asv") %>%
        # what should we group by?
        dplyr::group_by(sample_id) %>%
        dplyr::mutate(rel_abund = count/sum(count)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-count)

    taxon_lvls <-rel_abund %>%
        dplyr::select(-asv, -sample_id, -rel_abund) %>%
        colnames()

    rel_abund %>%
        tidyr::pivot_longer(taxon_lvls,
                            names_to = "level",
                            values_to = "taxon")
}

choose_taxa_level <- function(rel_abund, taxon_level = "Phylum") {
   rel_abund %>%
        dplyr::filter(level == taxon_level)
}

choose_samples <- function(rel_abund, smp_selection) {
    rel_abund %>%
        dplyr::filter(sample_id == paste(smp_selection, sep = " || "))
}
