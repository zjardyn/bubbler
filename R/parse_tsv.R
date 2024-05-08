#' Grab asv table from a tsv file and format to tibble
#'
#' @param tsv A character vector specifying the path to seqtab.tsv
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' fpath <- system.file("extdata", "seqtab.tsv", package = "bubbler")
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
#' fpath <- system.file("extdata", "taxa.tsv", package = "bubbler")
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
#' fpath <- system.file("extdata", "metadata.tsv", package = "bubbler")
#' meta_data_tsv(fpath)
meta_data_tsv <- function(tsv){
    suppressMessages(readr::read_tsv(tsv, show_col_types = FALSE)) %>%
        dplyr::rename_with(~ "sample_id", 1)
}
