utils::globalVariables(c("sample_id", "count", ".", "asv", "level"))

#' Generate a relative abundance table in tibble format from a phyloseq object.
#'
#' @param phy A phyloseq object containing an otu_table and tax_table.
#' @param taxa_level A character value specifying the taxa level from Domain to species.
#' @param var A character value of a variable to sum by.
#' @param meta_data A logical value specifying if metadata should be included from phy.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' rel_abund_phy(phy = physeq1, taxa_level = "Phylum", var = NULL , meta_data = FALSE)
rel_abund_phy <- function(phy, taxa_data = TRUE, taxa_level = "Phylum", meta_data = FALSE, var = NULL ) {

   if(!is.null(var) & meta_data == TRUE) {

   metadata <- meta_data_phy(phy)

   rel_abund <- asv_data_phy(phy) %>%
       tidyr::pivot_longer(-sample_id,
                           names_to = "asv",
                           values_to = "count") %>%
       dplyr::inner_join(., metadata, by =  "sample_id") %>%
       dplyr::group_by(!!rlang::sym(var)) %>%
       dplyr::mutate(rel_abund = count/sum(count)) %>%
       dplyr::ungroup() %>%
       dplyr::select(-count)
       # dplyr::inner_join(., taxonomy, by =  "asv")

   } else if (!is.null(var) & meta_data == FALSE) {

   rel_abund <- asv_data_phy(phy) %>%
       tidyr::pivot_longer(-sample_id,
                           names_to = "asv",
                           values_to = "count") %>%
       dplyr::group_by(!!rlang::sym(var)) %>%
       dplyr::mutate(rel_abund = count/sum(count)) %>%
       dplyr::ungroup() %>%
       dplyr::select(-count)
       # dplyr::inner_join(., taxonomy, by =  "asv")
   }

   if(is.null(var) & meta_data == TRUE) {

    metadata <- meta_data_phy(phy)

    rel_abund <- asv_data_phy(phy) %>%
        tidyr::pivot_longer(-sample_id,
                            names_to = "asv",
                            values_to = "count") %>%
        dplyr::inner_join(., metadata, by =  "sample_id") %>%
        dplyr::mutate(rel_abund = count/sum(count)) %>%
        dplyr::select(-count)
        # dplyr::inner_join(., taxonomy, by =  "asv")

   } else if (is.null(var) & meta_data == FALSE){

    rel_abund <- asv_data_phy(phy) %>%
        tidyr::pivot_longer(-sample_id,
                            names_to = "asv",
                            values_to = "count") %>%
        dplyr::mutate(rel_abund = count/sum(count)) %>%
        dplyr::select(-count)
        # dplyr::inner_join(., taxonomy, by =  "asv")
   }

    if(taxa_data == TRUE) {

    taxonomy <- taxa_data_phy(phy)

    taxa_lvls <- taxonomy %>%
        dplyr::select(-asv) %>%
        colnames()

    rel_abund %>%
        dplyr::inner_join(., taxonomy, by =  "asv") %>%
        tidyr::pivot_longer(taxa_lvls,
                            names_to = "level",
                            values_to = "taxon") %>%
        dplyr::filter(level == taxa_level)

    } else {

       rel_abund

    }

}

#' Generate a relative abundance table in tibble format from raw tsv files.
#'
#' @param asv A tsv file path containing an asv table.
#' @param taxa  A tsv file path containing a taxa table.
#' @param meta_data Either NULL, or, optionally, a tsv file path containing a meta_data table.
#' @param var A character value of a variable to sum by.
#' @param taxa_level A character value specifying the taxa level from Domain to species.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' asv <- system.file("extdata", "seqtab.tsv", package = "bubbler")
#' taxa <- system.file("extdata", "taxa.tsv", package = "bubbler")
#' meta_data <- system.file("extdata", "metadata.tsv", package = "bubbler")
#' rel_abund_raw(asv, taxa, meta_data)
rel_abund_raw <- function(asv, taxa = NULL, meta_data = NULL, var = NULL, taxa_level = "Phylum" ) {
#
#     if(!is.null(meta_data)) {
#         metadata <- meta_data_tsv(meta_data)
#     }

    if(!is.null(var) & !is.null(meta_data)){

        metadata <- meta_data_tsv(meta_data)

        rel_abund <- asv_data_tsv(asv) %>%
            tidyr::pivot_longer(-sample_id,
                                names_to = "asv",
                                values_to = "count") %>%
            dplyr::inner_join(., metadata, by =  "sample_id") %>%
            dplyr::group_by(!!rlang::sym(var)) %>%
            dplyr::mutate(rel_abund = count/sum(count)) %>%
            dplyr::ungroup() %>%
            dplyr::select(-count)
            # dplyr::inner_join(., taxonomy, by =  "asv")

    } else if (!is.null(var) & is.null(meta_data)) {

        rel_abund <- asv_data_tsv(asv) %>%
            tidyr::pivot_longer(-sample_id,
                                names_to = "asv",
                                values_to = "count") %>%
            dplyr::group_by(!!rlang::sym(var)) %>%
            dplyr::mutate(rel_abund = count/sum(count)) %>%
            dplyr::ungroup() %>%
            dplyr::select(-count)
            # dplyr::inner_join(., taxonomy, by =  "asv")
    }

    if(is.null(var) & !is.null(meta_data)) {

        metadata <- meta_data_tsv(meta_data)

        rel_abund <- asv_data_tsv(asv) %>%
            tidyr::pivot_longer(-sample_id,
                                names_to = "asv",
                                values_to = "count") %>%
            dplyr::inner_join(., metadata, by =  "sample_id") %>%
            dplyr::mutate(rel_abund = count/sum(count)) %>%
            dplyr::select(-count)
            # dplyr::inner_join(., taxonomy, by =  "asv")

    } else if (is.null(var) & is.null(meta_data)) {

        rel_abund <- asv_data_tsv(asv) %>%
            tidyr::pivot_longer(-sample_id,
                                names_to = "asv",
                                values_to = "count") %>%
            dplyr::mutate(rel_abund = count/sum(count)) %>%
            dplyr::select(-count)
            # dplyr::inner_join(., taxonomy, by =  "asv")
    }

    if(!is.null(taxa)){

    taxonomy <- taxa_data_tsv(taxa)

    taxa_lvls <- taxonomy %>%
        dplyr::select(-asv) %>%
        colnames()

    rel_abund %>%
        dplyr::inner_join(., taxonomy, by =  "asv") %>%
        tidyr::pivot_longer(taxa_lvls,
                            names_to = "level",
                            values_to = "taxon") %>%
        dplyr::filter(level == taxa_level)

    } else {

        rel_abund

    }
}
