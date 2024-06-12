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
