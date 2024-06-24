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
rel_abund_phy <- function(phy, taxa_data, meta_data, taxa_level, var) {
   if(missing(phy)){stop("rel_abund_phy needs a physeq object with a asv/otu table.")}
   if(missing(taxa_data)){taxa_data = TRUE}
   if(missing(meta_data)){meta_data = FALSE}
   if(missing(taxa_level)){taxa_level = "Phylum"}
   if(missing(var)){var = NULL}

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

   } else if (!is.null(var) & meta_data == FALSE) {

   rel_abund <- asv_data_phy(phy) %>%
       tidyr::pivot_longer(-sample_id,
                           names_to = "asv",
                           values_to = "count") %>%
       dplyr::group_by(!!rlang::sym(var)) %>%
       dplyr::mutate(rel_abund = count/sum(count)) %>%
       dplyr::ungroup() %>%
       dplyr::select(-count)
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

   } else if (is.null(var) & meta_data == FALSE){

    rel_abund <- asv_data_phy(phy) %>%
        tidyr::pivot_longer(-sample_id,
                            names_to = "asv",
                            values_to = "count") %>%
        dplyr::mutate(rel_abund = count/sum(count)) %>%
        dplyr::select(-count)

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
        dplyr::filter(level == taxa_level) %>%
        dplyr::relocate(sample_id, asv, level, taxon, rel_abund)

    } else {

       rel_abund %>%
            dplyr::relocate(sample_id, asv, rel_abund)
    }

}

#' Generate a relative abundance table in tibble format from tsv files.
#'
#' @param asv A tsv file path containing an asv table.
#' @param taxa_data  A tsv file path containing a taxa table.
#' @param taxa_level A character value specifying the taxa level from Domain to species.
#' @param meta_data A tsv file path containing a meta_data table.
#' @param var A character value of a variable to sum by.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' asv <- system.file("extdata/tsv", "seqtab.tsv", package = "bubbler")
#' taxa <- system.file("extdata/tsv", "taxa.tsv", package = "bubbler")
#' meta_data <- system.file("extdata/tsv", "metadata.tsv", package = "bubbler")
#' rel_abund_tsv(asv, taxa, meta_data)
rel_abund_tsv <- function(asv, taxa_data, meta_data, taxa_level, var) {
   if(missing(asv)){stop("rel_abund_tsv needs a .tsv asv/otu table filepath.")}
   if(missing(taxa_data)){taxa_data = NULL}
   if(missing(taxa_level)){taxa_level = "Phylum"}
   if(missing(meta_data)){meta_data = NULL}
   if(missing(var)){var = NULL}



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

    } else if (!is.null(var) & is.null(meta_data)) {

        rel_abund <- asv_data_tsv(asv) %>%
            tidyr::pivot_longer(-sample_id,
                                names_to = "asv",
                                values_to = "count") %>%
            dplyr::group_by(!!rlang::sym(var)) %>%
            dplyr::mutate(rel_abund = count/sum(count)) %>%
            dplyr::ungroup() %>%
            dplyr::select(-count)
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

    } else if (is.null(var) & is.null(meta_data)) {

        rel_abund <- asv_data_tsv(asv) %>%
            tidyr::pivot_longer(-sample_id,
                                names_to = "asv",
                                values_to = "count") %>%
            dplyr::mutate(rel_abund = count/sum(count)) %>%
            dplyr::select(-count)
    }

    if(!is.null(taxa_data)){

    taxonomy <- taxa_data_tsv(taxa_data)

    taxa_lvls <- taxonomy %>%
        dplyr::select(-asv) %>%
        colnames()

    rel_abund %>%
        dplyr::inner_join(., taxonomy, by =  "asv") %>%
        tidyr::pivot_longer(taxa_lvls,
                            names_to = "level",
                            values_to = "taxon") %>%
        dplyr::filter(level == taxa_level) %>%
          dplyr::relocate(sample_id, asv, level, taxon, rel_abund)

    } else {

        rel_abund %>%
              dplyr::relocate(sample_id, asv, rel_abund)

    }
}

#' @export
rel_abund_qiime <- function(asv_qiime, taxa_qiime, metadata_qiime, taxa_level, var) {
   if(missing(asv_qiime)){stop("rel_abund_qiime needs an .asv filepath.")}
   if(missing(taxa_qiime)){taxa_qiime = NULL}
   if(missing(metadata_qiime)){metadata_qiime = NULL}
   if(missing(taxa_level)){taxa_level = "Phylum"}
   if(missing(var)){var = NULL}

    if(!is.null(var) & !is.null(metadata_qiime)){

        metadata <- meta_data_qiime(metadata_qiime)

        rel_abund <- asv_data_qiime(asv_qiime) %>%
            tidyr::pivot_longer(-sample_id,
                                names_to = "asv",
                                values_to = "count") %>%
            dplyr::inner_join(., metadata, by =  "sample_id") %>%
            dplyr::group_by(!!rlang::sym(var)) %>%
            dplyr::mutate(rel_abund = count/sum(count)) %>%
            dplyr::ungroup() %>%
            dplyr::select(-count)

    } else if (!is.null(var) & is.null(metadata_qiime)) {

        rel_abund <- asv_data_qiime(asv_qiime) %>%
            tidyr::pivot_longer(-sample_id,
                                names_to = "asv",
                                values_to = "count") %>%
            dplyr::group_by(!!rlang::sym(var)) %>%
            dplyr::mutate(rel_abund = count/sum(count)) %>%
            dplyr::ungroup() %>%
            dplyr::select(-count)
    }

    if(is.null(var) & !is.null(metadata_qiime)) {

        metadata <- meta_data_qiime(metadata_qiime)

        rel_abund <- asv_data_qiime(asv_qiime) %>%
            tidyr::pivot_longer(-sample_id,
                                names_to = "asv",
                                values_to = "count") %>%
            dplyr::inner_join(., metadata, by =  "sample_id") %>%
            dplyr::mutate(rel_abund = count/sum(count)) %>%
            dplyr::select(-count)

    } else if (is.null(var) & is.null(metadata_qiime)) {

        rel_abund <- asv_data_qiime(asv_qiime) %>%
            tidyr::pivot_longer(-sample_id,
                                names_to = "asv",
                                values_to = "count") %>%
            dplyr::mutate(rel_abund = count/sum(count)) %>%
            dplyr::select(-count)
    }

    if(!is.null(taxa_qiime)){

    taxonomy <- taxa_data_qiime(taxa_qiime)

    taxa_lvls <- taxonomy %>%
        dplyr::select(-asv) %>%
        colnames()

    rel_abund %>%
        dplyr::inner_join(., taxonomy, by =  "asv") %>%
        tidyr::pivot_longer(taxa_lvls,
                            names_to = "level",
                            values_to = "taxon") %>%
        dplyr::filter(level == taxa_level) %>%
        dplyr::relocate(sample_id, asv, level, taxon, rel_abund)

    } else {

        rel_abund %>%
            dplyr::relocate(sample_id, asv, rel_abund)

    }
}

#' @export
rel_abund_bracken <- function(path, remove_human){
    if(missing(path)){stop("rel_abund_bracken needs a folder of bracken output files.")}
    if(missing(remove_human)){remove_human = TRUE}

    file_list <- list.files(path = path, pattern = "*.txt", full.names = TRUE)
    bracken_data <- purrr::map(file_list, read_bracken_file)
    combined_data <- dplyr::bind_rows(bracken_data)

    data <- combined_data %>%
        mutate(sample_id = str_split(sample_id, "_kraken2") %>% map_chr(1)) %>%
        select(sample_id, name, taxonomy_lvl, kraken_assigned_reads)

    if(remove_human == TRUE) {
        data <- data %>%
            filter(name != "Homo sapiens")
    }
    # TODO: Check to make sure taxonomy_lvl is all S
    data %>%
        mutate(rel_abund = kraken_assigned_reads/sum(kraken_assigned_reads)) %>%
        rename('taxon' = name) %>%
        select(sample_id, taxon, rel_abund)
}
