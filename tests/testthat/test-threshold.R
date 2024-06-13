devtools::load_all()

counts <- system.file("extdata/tsv/seqtab.tsv", package = "bubbler")
taxa <- system.file("extdata/tsv/taxa.tsv", package = "bubbler")
meta <- system.file("extdata/tsv/metadata.tsv", package = "bubbler")

rel_abund <- rel_abund_tsv(counts, taxa, meta_data = meta, taxa_level = "Genus")
choose_n_taxa(rel_abund, 20)
show_top_taxa(rel_abund)

rel_abund %>%
    pool_taxa(threshold = 0.005) %>%
    bar_plot()
# rel_abund_phy(physeq1, meta_data = T) %>%
#     pool_taxa(threshold = 0.01, var = "Location" )
    # bar_plot()


# rel_abund %>%
#     pool_taxa_t()

taxon_pool <- rel_abund %>%
    dplyr::group_by(taxon) %>%
    dplyr::summarise(pool = max(rel_abund) < threshold,
                     mean = mean(rel_abund),
                     .groups = "drop")

pooled <- dplyr::inner_join(rel_abund, taxon_pool, by = "taxon") %>%
    # set_taxon_threshold(taxon, threshold)
    dplyr::mutate(taxon = dplyr::if_else(pool, glue::glue("< {round(threshold, 4)}%"), taxon),
                  taxon = tidyr::replace_na(taxon, "Unclassified"))


    # TODO: Add this to other parts of function.
rel_abund_pooled <- pooled %>%
    dplyr::group_by(sample_id, asv) %>%
    dplyr::reframe(rel_abund = sum(rel_abund)) %>%
    # mean = sum(mean)) %>%
    dplyr::relocate(sample_id, asv, rel_abund ) %>%
    dplyr::distinct()


rel_abund_pooled %>%
    dplyr::inner_join(taxa_data_tsv(taxa))



add_taxa <- function(rel_abund_tab, taxa_data_type){
    if(missing(rel_abund_tab)){stop("Provide a rel_abund table.")}
    if(missing(taxa_data_type)){stop('Provide either "tsv", "phylo", "qiime"')}

    if(taxa_data_type == "tsv"){

    }

}





pool_taxa_t <- function(rel_abund_tab, threshold = 0.2, var = NULL) {

    # metadata <- rel_abund_tab %>%
    #     dplyr::select(!(asv:count))

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

        # TODO: Add this to other parts of function.
        rel_abund_pooled <- pooled %>%
            dplyr::group_by(sample_id, asv) %>%
            dplyr::reframe(rel_abund = sum(rel_abund)) %>%
            # mean = sum(mean)) %>%
            dplyr::relocate(sample_id, asv, rel_abund ) %>%
            dplyr::distinct()


        rel_abund_pooled
        # if(dim(metadata)[2] != 0) {
        #    inner_join(rel_abund_pooled, metadata, by = "sample_id") %>%
        #         distinct()
        # } else {
        #     rel_abund_pooled
        # }

    } else {
        pooled %>%
            dplyr::group_by(!!rlang::sym(var), sample_id, taxon) %>%
            dplyr::reframe(rel_abund = sum(rel_abund),
                           level = level,
                           asv = asv,
                           !!var := !!rlang::sym(var)) %>%
            dplyr::relocate(sample_id, asv, level, taxon, rel_abund, !!rlang::sym(var)) %>%
            dplyr::distinct()
    }
}
