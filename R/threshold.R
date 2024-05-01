#' Show the top taxa of a rel_abund table
#'
#' Finds the top taxa and arranges in descending order. Uses max() to
#' find the maximum taxon.
#'
#' @param rel_abund A rel_abund table in tibble format.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' show_top_taxa(rel_abund)
show_top_taxa <- function(rel_abund_tab) {

    rel_abund_tab %>%
    dplyr::group_by(taxon) %>%
    dplyr::summarise(max = max(rel_abund)) %>%
    dplyr::arrange(desc(max))
}

#' Choose how many taxa to display when plotting
#'
#' Generates a numeric vector as the threshold to display n taxa.
#'
#' @param rel_abund A rel_abund table in tibble format.
#' @param n_taxa An integer vector for the number of taxa to display.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' choose_n_taxa(rel_abund, n_taxa = 8)
choose_n_taxa <- function(rel_abund_tab, n_taxa = 8) {

    unique_taxa <- show_top_taxa(rel_abund_tab) %>% nrow()

    # unique_taxa <- rel_abund_tab %>%
    #     # dplyr::summarise(n = dplyr::n_distinct(taxon))
    #     dplyr::pull(taxon) %>%
    #     dplyr::n_distinct()

    if(n_taxa > unique_taxa)  {
        n_taxa = unique_taxa
    }

    rel_abund_tab %>%
    dplyr::group_by(taxon) %>%
    dplyr::summarise(max = max(rel_abund)) %>%
    dplyr::arrange(desc(max)) %>%
        dplyr::slice(n_taxa) %>%
        dplyr::pull(max)
}

#' Helper function 1 to pool taxa below threshold
#'
#' Summarise the pooled group as a logical vector, also summarise
#' the mean rel_abund values to be used for ordering.
#'
#' @param rel_abund A rel_abund table in tibble format.
#' @param threshold A numeric vector for the threshold.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' .pool_taxon_tresh(rel_abund, threshold)
.pool_taxon_thresh <- function(rel_abund_tab, threshold = 0.2) {

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
#' @param taxon The taxon column.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' .set_taxon_threshold(rel_abund_pool, taxon)
.set_taxon_threshold <- function(rel_abund_pool, taxon, threshold = 0.2) {

    rel_abund_pool %>%
        dplyr::mutate(taxon = dplyr::if_else(pool, glue::glue("< {round(threshold, 2)}%"), taxon),
               taxon = tidyr::replace_na(taxon, "Unclassified"))
}

#' Pool taxa according to threshold
#'
#' Applies a threshold and pools any taxa below this threshold, across samples
#' and optionally, across a variable.
#'
#' @param rel_abund A rel_abund table in tibble format.
#' @param threshold A numeric vector for the threshold.
#' @param var A variable to pool across.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' pool_taxa(rel_abund, threshold = 0.2, var = "Location")
pool_taxa <- function(rel_abund, threshold = 0.2, var = NULL) {

    taxon_pool <- .pool_taxon_thresh(rel_abund, threshold)
    pooled <- dplyr::inner_join(rel_abund, taxon_pool, by = "taxon") %>%
        .set_taxon_threshold(taxon, threshold)
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
