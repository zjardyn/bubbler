utils::globalVariables(c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Taxon", "Confidence"))

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

# PHYLOSEQ

#' Grab metadata from a physeq object and format to tibble
#'
#' @param phy A phyloseq object containig sample_data
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' meta_data_phy(physeq)
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
#' taxa_data_phy(physeq)
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
#' asv_data_phy(physeq)
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

#' Grab asv table from a .qza file and format to tibble.
#'
#' @param qza  Path to .qza file containing otu counts.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' fpath <- system.file("extdata/qiime", "table-dada2.qza", package = "bubbler")
#' asv_data_qiime(fpath)
asv_data_qiime <- function(qza){
    read_qza(qza)[["data"]] %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "sample_id") %>%
        tibble::as_tibble()
}

#' Grab taxonomy table from a .qza file and format to tibble.
#'
#' @param qza  Path to .qza file containing a taxonomy table.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' fpath <- system.file("extdata/qiime", "taxonomy.qza", package = "bubbler")
#' taxa_data_qiime(fpath)
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


#' Grab metadata table from a .tsv QIIME2-formatted file and format to tibble.
#'
#' @param tsv Path to .tsv file in QIIME2 format.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' fpath <- system.file("extdata/qiime", "sample-metadata.tsv", package = "bubbler")
#' meta_data_qiime(fpath)
meta_data_qiime <- function(tsv){
    metadata <- read_q2metadata(tsv) %>%
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

## BRACKEN

#' Grab bracken data and format to tibble.
#'
#' @param filepath Path to bracken .txt files.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' fpath <- system.file("extdata/bracken",
#' "7_S79_kraken2_report_bracken_output.txt", package = "bubbler")
#' read_bracken_file(fpath)
read_bracken_file <- function(filepath) {
    sample_id <- basename(filepath)
    readr::read_tsv(filepath, show_col_types = FALSE) %>%
        dplyr::mutate(sample_id = sample_id) %>%
        dplyr::relocate(sample_id)
}


