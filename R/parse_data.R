## .TSV

#' Grab asv table from a tsv file and format to tibble
#'
#' @param tsv A character vector specifying the path to seqtab.tsv
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' fpath <- system.file("extdata/tsv", "seqtab.tsv", package = "bubbler")
#' asv_data_tsv(fpath)
asv_data_tsv <- function(tsv){
    suppressMessages(readr::read_tsv(tsv, show_col_types = FALSE)) %>%
        dplyr::rename_with(~ "asv", 1) %>%
        tibble::column_to_rownames(var = "asv") %>%
        as.matrix() %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "sample_id") %>%
        tibble::as_tibble()
}


#' Grab taxa table from a tsv file and format to tibble
#'
#' @param tsv  A character vector specifying the path to seqtab.tsv
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' fpath <- system.file("extdata/tsv", "taxa.tsv", package = "bubbler")
#' taxa_data_tsv(fpath)
taxa_data_tsv <- function(tsv){
    suppressMessages(readr::read_tsv(tsv, show_col_types = FALSE)) %>%
        dplyr::rename_with(~ "asv", 1)
}

#' Grab metadata table from a tsv file and format to tibble
#'
#' @param tsv  A character vector specifying the path to metadata.tsv
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' fpath <- system.file("extdata/tsv", "metadata.tsv", package = "bubbler")
#' meta_data_tsv(fpath)
meta_data_tsv <- function(tsv){
    suppressMessages(readr::read_tsv(tsv, show_col_types = FALSE)) %>%
        dplyr::rename_with(~ "sample_id", 1)
}

# PHY

#' Grab metadata from a physeq object and format to tibble
#'
#' @param phy A phyloseq object containig sample_data
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' meta_data_phy(physeq1)
meta_data_phy <- function(phy) {
    meta <- phy@sam_data
    sample_id <- rownames(meta)
    meta_tb <- meta %>%
        tibble::as_tibble()
    meta_tb %>% tibble::add_column(sample_id) %>%
        dplyr::relocate(sample_id)
}

#' Grab taxa table from a physeq object and format to tibble
#'
#' @param phy A phyloseq object containing  tax_table
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' taxa_data_phy(physeq1)
taxa_data_phy <- function(phy){
    phy@tax_table %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "asv") %>%
        tibble::as_tibble()
}

#' Grab asv table from a physeq object and format to tibble
#'
#' @param phy A phyloseq object containing tax_table
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' asv_data_phy(physeq1)
asv_data_phy <- function(phy){

    counts <- phy@otu_table %>% t()
    sample_id <- rownames(counts)
    counts_tb <- counts %>%
        as.data.frame() %>%
        tibble::as_tibble()
    counts_tb %>% tibble::add_column(sample_id) %>%
        dplyr::relocate(sample_id)
}

# QIIME2

#' @export
asv_data_qiime <- function(qza){
    read_qza(qza)[["data"]] %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "sample_id") %>%
        tibble::as_tibble()
}

#' @export
taxa_data_qiime <- function(qza){
    read_qza(qza)[["data"]] %>%
        {purrr::quietly(tidyr::separate)}(Taxon, into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = "; ") %>% magrittr::extract2("result") %>%
        dplyr::mutate(dplyr::across(.cols = Domain:Species,
                                    .fns = ~stringr::str_remove(.,"[a-z]__"))) %>%
        dplyr::mutate(dplyr::across(.cols = Domain:Species,
                                    .fns = ~dplyr::na_if(., "") )) %>%
        dplyr::rename_with(~"asv", 1) %>%
        dplyr::select(-Confidence) %>%
        tibble::as_tibble()
}


#' @export
meta_data_qiime <- function(qza){
    metadata <- read_q2metadata(qza) %>%
        dplyr::rename_with(~"sample_id", 1) %>%
        tibble::as_tibble()

    metadata_names <- metadata %>%
        names() %>%
        #TODO: Add these to all parsing functions
        stringr::str_replace_all("-", "_") %>%
        stringr::str_replace_all(" ", "_")

    colnames(metadata) <- metadata_names
    metadata
}


