#' Grab asv table from a tsv file and format to tibble
#'
#' @param tsv A character vector specifying the path to seqtab.tsv
#'
#' @return A tibble
#' @export
#'
#' @examples
#' fpath <- system.file("extdata", "seqtab.tsv", package = "bubbler")
#' asv_data_tsv(fpath)
asv_data_tsv <- function(tsv){
    readr::read_tsv(tsv) %>%
        tibble::column_to_rownames(var = "asv") %>%
        as.matrix() %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "sample_id") %>%
        tibble::as_tibble()
}
