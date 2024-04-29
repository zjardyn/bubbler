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
