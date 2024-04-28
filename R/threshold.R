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
