utils::globalVariables(c("taxon", "pool", ":="))

#' Show the top taxa of a rel_abund table
#'
#' Finds the top taxa and arranges in descending order. Uses max() to
#' find the maximum taxon.
#'
#' @param rel_abund_tab A rel_abund table in tibble format.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' show_top_taxa(rel_abund_tab)
show_top_taxa <- function(rel_abund_tab) {

    rel_abund_tab %>%
        dplyr::group_by(taxon) %>%
        dplyr::summarise(max = max(rel_abund)) %>%
        dplyr::arrange(dplyr::desc(max))
}

#' Choose how many taxa to display when plotting
#'
#' Generates a numeric vector as the threshold to display n taxa.
#'
#' @param rel_abund_tab A rel_abund table in tibble format.
#' @param n_taxa An integer vector for the number of taxa to display.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' choose_n_taxa(rel_abund_tab, n_taxa = 8)
choose_n_taxa <- function(rel_abund_tab, n_taxa = 8) {

    unique_taxa <- show_top_taxa(rel_abund_tab) %>% nrow()

    if(n_taxa > unique_taxa)  {
        n_taxa = unique_taxa
    }

    rel_abund_tab %>%
    dplyr::group_by(taxon) %>%
    dplyr::summarise(max = max(rel_abund)) %>%
    dplyr::arrange(dplyr::desc(max)) %>%
        dplyr::slice(n_taxa) %>%
        dplyr::pull(max)
}

#' Pool taxa according to threshold
#'
#' Applies a threshold and pools any taxa below this threshold, across samples
#' and optionally, across a variable.
#'
#' @param rel_abund_tab A rel_abund table in tibble format.
#' @param threshold A numeric vector for the threshold.
#' @param var A variable to pool across.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' pool_taxa(rel_abund_tab, threshold = 0.2, var = "Location")
pool_taxa <- function(rel_abund_tab, threshold = 0.2, var = NULL) {

    # taxon_pool <- pool_taxon_thresh(rel_abund_tab, threshold)
    taxon_pool <- rel_abund_tab %>%
        dplyr::group_by(taxon) %>%
        dplyr::summarise(pool = max(rel_abund) < threshold,
                         mean = mean(rel_abund),
                         .groups = "drop")

    pooled <- dplyr::inner_join(rel_abund_tab, taxon_pool, by = "taxon") %>%
        # set_taxon_threshold(taxon, threshold)
         dplyr::mutate(taxon = dplyr::if_else(pool, glue::glue("< {round(threshold, 4)}%"), taxon),
               taxon = tidyr::replace_na(taxon, "Unclassified"))

    if(is.null(var)){
        pooled %>%
        dplyr::group_by(sample_id, taxon) %>%
        dplyr::reframe(rel_abund = sum(rel_abund))%>%
                # mean = sum(mean)) %>%
        dplyr::distinct()
    } else {
        pooled %>%
        dplyr::group_by(!!rlang::sym(var), sample_id, taxon) %>%
        dplyr::reframe(rel_abund = sum(rel_abund),
                # mean = sum(mean),
                !!var := !!rlang::sym(var)) %>%
        dplyr::distinct()
    }
}
