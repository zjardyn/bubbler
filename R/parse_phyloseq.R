#' meta_data_phy <- function(phy) {
#'     meta <- phy@sam_data
#'     sample_id <- rownames(meta)
#'     meta_tb <- meta %>%
#'         tibble::as_tibble()
#'     meta_tb %>% tibble::add_column(sample_id) %>%
#'         dplyr::relocate(sample_id)
#' }
#' #' taxa_data_phy(physeq1)
#' taxa_data_phy <- function(phy){
#'     phy@tax_table %>%
#'         as.data.frame() %>%
#'         tibble::rownames_to_column(var = "asv") %>%
#'         tibble::as_tibble()
#' }
#'
#' #' asv_data_phy(physeq1)
#' asv_data_phy <- function(phy){
#'
#'     counts <- phy@otu_table %>% t()
#'     sample_id <- rownames(counts)
#'     counts_tb <- counts %>%
#'         as.data.frame() %>%
#'         tibble::as_tibble()
#'     counts_tb %>% tibble::add_column(sample_id) %>%
#'         dplyr::relocate(sample_id)
#' }
#'
#'
#' asv_data_qiime <- function(qza){
#'    read_qza(qza)[["data"]] %>%
#'         t() %>%
#'         as.data.frame() %>%
#'         tibble::rownames_to_column(var = "sample_id") %>%
#'         tibble::as_tibble()
#' }
#'
#' taxa_data_qiime <- function(qza){
#'     read_qza(qza)[["data"]] %>%
#'         {purrr::quietly(tidyr::separate)}(Taxon, into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = "; ") %>% magrittr::extract2("result") %>%
#'         dplyr::mutate(dplyr::across(.cols = Domain:Species,
#'                       .fns = ~stringr::str_remove(.,"[a-z]__"))) %>%
#'         dplyr::mutate(dplyr::across(.cols = Domain:Species,
#'                       .fns = ~dplyr::na_if(., "") )) %>%
#'         dplyr::rename_with(~"asv", 1) %>%
#'         dplyr::select(-Confidence) %>%
#'         tibble::as_tibble()
#' }
#'
#'
#' meta_data_qiime <- function(qza){
#'     metadata <- read_q2metadata(qza) %>%
#'         dplyr::rename_with(~"sample_id", 1) %>%
#'         tibble::as_tibble()
#'
#'     metadata_names <- metadata %>%
#'         names() %>%
#'         #TODO: Add these to all parsing functions
#'         str_replace_all("-", "_") %>%
#'         str_replace_all(" ", "_")
#'
#'     colnames(metadata) <- metadata_names
#'     metadata
#' }
