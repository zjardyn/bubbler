utils::globalVariables(c("rel_abund", "top_taxon"))

#' @export
arrange_sample_by_taxa <- function(rel_abund_tab){
   grouping <- rel_abund_tab %>%
       dplyr::group_by(sample_id) %>%
       dplyr::slice_max(order_by = rel_abund, with_ties = FALSE) %>%
       dplyr::select(taxon, sample_id) %>%
       dplyr::rename(top_taxon = taxon)

    dplyr::inner_join(rel_abund_tab, grouping, by = "sample_id") %>%
        dplyr::group_by(top_taxon) %>%
        dplyr::mutate(rank = rank(rel_abund)) %>%
        dplyr::mutate(sample_id = stats::reorder(sample_id, -rank)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-rank, -top_taxon)

}

#' @export
arrange_taxa <- function(rel_abund_tab, pooled_top = FALSE) {
   grouping <- rel_abund_tab %>%
        dplyr::group_by(taxon) %>%
        dplyr::summarise(mean = mean(rel_abund))

    if(pooled_top == FALSE){

        dplyr::inner_join(rel_abund_tab, grouping, by = "taxon") %>%
            dplyr::mutate(taxon = as.factor(taxon)) %>%
            dplyr::mutate(taxon = forcats::fct_reorder(taxon, mean)) %>%
            dplyr::select(-mean)
    } else {
        threshold <- grep("<", rel_abund_tab$taxon, value = TRUE, fixed = TRUE, useBytes = TRUE)[1]

        dplyr::inner_join(rel_abund_tab, grouping, by = "taxon") %>%
            dplyr::mutate(taxon = as.factor(taxon)) %>%
            dplyr::mutate(taxon = forcats::fct_reorder(taxon, mean)) %>%
            dplyr::select(-mean) %>%
            dplyr::mutate(taxon = forcats::fct_relevel(taxon, threshold, after = 0),
                          taxon = forcats::fct_relevel(taxon, "Unclassified", after = 1))

    }
}
