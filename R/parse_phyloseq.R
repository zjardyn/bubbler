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
    sample_id <- rownames(phyloseq::sample_data(phy))
    phyloseq::sample_data(phy) %>%
        tibble::as_tibble() %>%
        tibble::add_column(sample_id)
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
    phyloseq::tax_table(phy) %>%
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
    phyloseq::otu_table(phy) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "sample_id") %>%
        tibble::as_tibble()
}
