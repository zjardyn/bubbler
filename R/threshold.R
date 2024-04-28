choose_n_taxa <- function(rel_abund, n_taxa = 3) {
    nrow <- rel_abund %>%
    dplyr::group_by(taxon) %>%
    dplyr::summarise(sum = sum(rel_abund)) %>%
    nrow()

    if(n_taxa > nrow)  {
        n_taxa = nrow
    }

    rel_abund %>%
    dplyr::group_by(taxon) %>%
    dplyr::summarise(sum = sum(rel_abund)) %>%
    dplyr::arrange(desc(sum)) %>%
        dplyr::slice(n_taxa) %>%
        dplyr::pull(sum) %>%
        round(2)
}

.pool_taxon_thresh <- function(rel_abund, threshold = 0.2) {
   rel_abund %>%
        dplyr::group_by(taxon) %>%
        dplyr::summarise(pool = sum(rel_abund) < threshold,
                         mean = mean(rel_abund),
                         .groups = "drop")
}

.set_taxon_threshold <- function(rel_abund_pool, taxon) {
    rel_abund_pool %>%
        dplyr::mutate(taxon = dplyr::if_else(pool, glue::glue("< {threshold}%"), taxon),
               taxon = tidyr::replace_na(taxon, "Unclassified"))
}


pool_taxon <- function(rel_abund, threshold = 0.2){
    taxon_pool <- .pool_taxon_thresh(rel_abund, threshold)
    dplyr::inner_join(rel_abund, taxon_pool, by = "taxon") %>%
        .set_taxon_threshold(taxon) %>%
        dplyr::group_by(sample_id, taxon) %>%
        dplyr::reframe(rel_abund = sum(rel_abund),
                mean = sum(mean)) %>%
        dplyr::distinct()
}
