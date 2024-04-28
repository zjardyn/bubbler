show_top_taxa <- function(rel_abund) {

    rel_abund %>%
    dplyr::group_by(taxon) %>%
    dplyr::summarise(max = max(rel_abund)) %>%
    dplyr::arrange(desc(max))
}

choose_n_taxa <- function(rel_abund, n_taxa = 3) {

    unique_taxa <- rel_abund %>%
        dplyr::pull(taxon) %>%
        dplyr::n_distinct()

    if(n_taxa > unique_taxa)  {
        n_taxa <- unique_taxa
    }

    rel_abund %>%
    dplyr::group_by(taxon) %>%
    dplyr::summarise(max = max(rel_abund)) %>%
    dplyr::arrange(desc(max)) %>%
        dplyr::slice(n_taxa) %>%
        dplyr::pull(max)
}

.pool_taxon_thresh <- function(rel_abund, threshold = 0.2) {

   rel_abund %>%
        dplyr::group_by(taxon) %>%
        dplyr::summarise(pool = max(rel_abund) < threshold,
                         mean = mean(rel_abund),
                         .groups = "drop")
}

.set_taxon_threshold <- function(rel_abund_pool, taxon) {

    rel_abund_pool %>%
        dplyr::mutate(taxon = dplyr::if_else(pool, glue::glue("< {round(threshold, 2)}%"), taxon),
               taxon = tidyr::replace_na(taxon, "Unclassified"))
}

pool_taxon <- function(rel_abund, threshold = 0.2, var = NULL) {

    taxon_pool <- .pool_taxon_thresh(rel_abund, threshold)
    pooled <- dplyr::inner_join(rel_abund, taxon_pool, by = "taxon") %>%
        .set_taxon_threshold(taxon)
    if(is.null(var)){
        pooled %>%
        dplyr::group_by(sample_id, taxon) %>%
        dplyr::reframe(rel_abund = sum(rel_abund),
                mean = sum(mean)) %>%
        dplyr::distinct()
    } else {
        pooled %>%
        dplyr::group_by(!!rlang::sym(var), sample_id, taxon) %>%
        dplyr::reframe(rel_abund = sum(rel_abund),
                mean = sum(mean),
                !!var := !!rlang::sym(var)) %>%
        dplyr::distinct()
    }
}
