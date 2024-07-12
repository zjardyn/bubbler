utils::globalVariables(c("sample_id", "count", ".", "asv", "level", "name", "taxonomy_lvl", "kraken_assigned_reads"))

#' This is data to be included in my package
#'
#' @name physeq1
#' @docType data
#' @author Zjardyn Liera-Hood \email{zlieraho@uwaterloo.ca}
#' @keywords data
NULL


#' Generate a relative abundance table in tibble format from a phyloseq object.
#'
#' @param phy A phyloseq object containing an otu_table and tax_table.
#' @param taxa_level A character value specifying the taxa level from Domain to species.
#' @param var A character value of a variable to sum by.
#' @param meta_data A logical value specifying if metadata should be included from phy.
#' @param taxa_data Logical. Whether to include taxonomic information.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' rel_abund_phy(phy = physeq1, taxa_level = "Phylum", var = NULL , meta_data = FALSE)
rel_abund_phy <- function(phy, taxa_data = TRUE, meta_data = FALSE, taxa_level = "Phylum", var = NULL) {
   if(missing(phy)){stop("rel_abund_phy needs a physeq object with a asv/otu table.")}

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
rel_abund_tsv <- function(asv, taxa_data = NULL, meta_data = NULL, taxa_level = "Phylum", var = NULL) {
   if(missing(asv)){stop("rel_abund_tsv needs a .tsv asv/otu table filepath.")}

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

#' Generate a relative abundance table in tibble form, from QIIME2 artifacts.
#'
#' @param asv_qiime A count table in .qza format.
#' @param taxa_qiime A taxonomy table in .qza format.
#' @param metadata_qiime A metadata table in .tsv format. With second row as commented variable descriptions.
#' @param taxa_level The level to set the taxa, from Domain:Species.
#' @param var A variable to group by when computing relative abundance.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' asv_q <- system.file("extdata/qiime", "table-dada2.qza", package = "bubbler")
#' taxa_q <- system.file("extdata/qiime", "taxonomy.qza", package = "bubbler")
#' meta_q <- system.file("extdata/qiime", "sample-metadata.tsv", package = "bubbler")
#' rel_abund_qiime(asv_q, taxa_q, meta_q)

rel_abund_qiime <- function(asv_qiime, taxa_qiime = NULL, metadata_qiime = NULL, taxa_level = "Phylum", var = NULL) {
   if(missing(asv_qiime)){stop("rel_abund_qiime needs an .asv filepath.")}

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

#' Generate a relative abundance table using kracken2/bracken data.
#'
#' @param path The filepath to bracken data in .txt format.
#' @param remove_human Logical. Choose to remove human samples.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' bracken <- system.file("extdata/bracken", package = "bubbler")
#' rel_abund_bracken(bracken)
rel_abund_bracken <- function(path, remove_human){
    if(missing(path)){stop("rel_abund_bracken needs a folder of bracken output files.")}
    if(missing(remove_human)){remove_human = TRUE}

    file_list <- list.files(path = path, pattern = "*.txt", full.names = TRUE)
    bracken_data <- purrr::map(file_list, read_bracken_file)
    combined_data <- dplyr::bind_rows(bracken_data)

    data <- combined_data %>%
        dplyr::mutate(sample_id = stringr::str_split(sample_id, "_kraken2") %>% purrr::map_chr(1)) %>%
        dplyr::select(sample_id, name, taxonomy_lvl, kraken_assigned_reads)

    if(remove_human == TRUE) {
        data <- data %>%
            dplyr::filter(name != "Homo sapiens")
    }
    # TODO: Check to make sure taxonomy_lvl is all S
    data %>%
        dplyr::mutate(rel_abund = kraken_assigned_reads/sum(kraken_assigned_reads)) %>%
        dplyr::rename('taxon' = name) %>%
        dplyr::select(sample_id, taxon, rel_abund)
}
