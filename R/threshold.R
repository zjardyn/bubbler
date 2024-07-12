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
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' rel_abund_phy(physeq1) %>% pool_taxa()
pool_taxa <- function(rel_abund_tab, threshold, n_taxa = 12, keep_metadata = FALSE, label = TRUE) {
    if(missing(rel_abund_tab)){stop("Provide a relative abundance table.")}
    if(!("taxon" %in% colnames(rel_abund_tab))){stop('variable taxon not found in colnames' )}
    if(missing(threshold)){threshold = choose_n_taxa(rel_abund_tab, n_taxa)}

    taxon_pool <- rel_abund_tab %>%
        dplyr::group_by(taxon) %>%
        dplyr::summarise(pool = max(rel_abund) <= threshold,
                         .groups = "drop")

    if(label == TRUE){
        pooled <- dplyr::inner_join(rel_abund_tab, taxon_pool, by = "taxon") %>%
            dplyr::mutate(taxon = dplyr::if_else(pool, glue::glue("< {round(threshold, 4)}%"), taxon),
                          taxon = tidyr::replace_na(taxon, "Unclassified"))
    } else {

        pooled <- dplyr::inner_join(rel_abund_tab, taxon_pool, by = "taxon") %>%
            dplyr::mutate(taxon = dplyr::if_else(pool, "Other", taxon),
                          taxon = tidyr::replace_na(taxon, "Unclassified"))
    }

    rel_abund_pooled <- pooled %>%
        dplyr::group_by(sample_id, taxon) %>%
        dplyr::summarise(rel_abund = sum(rel_abund),
                         .groups = "drop")

    if(keep_metadata == TRUE) {
        metadata <- rel_abund_tab %>%
            dplyr::select(!(asv:rel_abund)) %>%
            dplyr::distinct()

    if(dim(metadata)[2] > 1) {
        dplyr::inner_join(rel_abund_pooled, metadata, by = "sample_id")
        } else {
            rel_abund_pooled
            }
    } else {
        rel_abund_pooled
    }
}

#' Detect the label used for threshold from a pooled relative_abundance table
#'
#' @param rel_abund_tb A relative abundance table in tibble format.
#'
#' @return A character vector of length one.
#' @export
#'
#' @examples
#' rel_abund_phy(physeq1) %>% detect_threshold()
detect_threshold <- function(rel_abund_tb){
    threshold <- grep("<", rel_abund_tb[["taxon"]], value = TRUE, fixed = TRUE, useBytes = TRUE)[1]

    if(is.na(threshold)){
        threshold <- "Other"
    }

   threshold
}


#' Detect the label used in a character vector.
#'
#' @param vector The character vector of taxon.
#'
#' @return A character vector of length one.
#' @export
#'
#' @examples
#' rel_abund_phy(physeq1) %>% all_taxa() %>% dplyr::pull(taxon) %>% detect_threshold_vec()
detect_threshold_vec <- function(vector){
    threshold <- grep("<", vector, value = TRUE, fixed = TRUE, useBytes = TRUE)[1]

    if(is.na(threshold)){
        threshold <- "Other"
    }

   threshold
}
