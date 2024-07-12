#' Return a character vector of unique taxa from a relative abundance table.
#'
#' @param rel_abund_tb A relative abundance table in tibble format.
#'
#' @return A vector arranged by abundance.
#' @export
#'
#' @examples
#' rel_abund_phy(phy = physeq) %>%
#'     all_taxa()
all_taxa <- function(rel_abund_tb){

    rel_abund_tb %>%
        arrange_taxa() %>%
        dplyr::select(taxon) %>%
        dplyr::reframe(taxon = unique(taxon))
}

#' Combine data.frames that are in a list.
#'
#' @param dataframes List of dataframes.
#' @param by_var Variable to join by.
#'
#' @return A joined dataframe.
#' @export
#'
#' @examples
#' df1 <- data.frame(id = 1:3, value = c("A", "B", "C"))
#' df2 <- data.frame(id = 2:4, value = c("D", "E", "F"))
#' df3 <- data.frame(id = 1:4, value = c("G", "H", "I", "J"))
#' dataframes <- list(df1, df2, df3)
#' combine_dataframes(dataframes, by_var = "value")
combine_dataframes <- function(dataframes, by_var) {
    if (length(dataframes) < 2) {
        stop("At least two dataframes are required")
    }

    combined_df <- purrr::reduce(dataframes, function(df1, df2) {
        dplyr::full_join(df1, df2, by = by_var)
    })

    return(combined_df)
}

#' Get the unique taxa from the combined data.frame.
#'
#' @param ... The dataframes to join.
#'
#' @return A character vector of the unique taxon from the combined data.frames.
#' @export
#'
#' @examples
#' asv <- system.file("extdata/tsv", "seqtab.tsv", package = "bubbler")
#' taxa <- system.file("extdata/tsv", "taxa.tsv", package = "bubbler")
#' rel_abund <- rel_abund_tsv(asv, taxa)
#' rel_abund_l <- rel_abund %>%
#'     subset_high_low(n = 10)
#' rel_abund_h <- rel_abund %>%
#'     subset_high_low(n = 50, flip = TRUE)
#' extract_unique_taxa(rel_abund_l, rel_abund_h)
extract_unique_taxa <- function(...) {

    dataframes <- list(...)
    combined_df <- combine_dataframes(dataframes, by_var = "taxon")

    combined_df$taxon %>%
        unique()

}


#' Set a global colour scheme for a series of plots.
#'
#' @param all_taxa All taxa from the initial relative abundance table, as a character vector.
#' @param unique_taxa The unique taxa among your subsetted/pooled relative abundance table.
#'
#' @return A named vector.
#' @export
#'
#' @examples

#' asv <- system.file("extdata/tsv", "seqtab.tsv", package = "bubbler")
#' taxa <- system.file("extdata/tsv", "taxa.tsv", package = "bubbler")
#' rel_abund <- rel_abund_tsv(asv, taxa) %>% add_other()
#' rel_abund_l <- rel_abund %>%
#'     subset_high_low(n = 10)
#' rel_abund_h <- rel_abund %>%
#'     subset_high_low(n = 50, flip = TRUE)
#' unique_taxa <- extract_unique_taxa(rel_abund_l, rel_abund_h)
#' all_taxa <- rel_abund %>% all_taxa()
#' global_colour_scheme(all_taxa, unique_taxa)
global_colour_scheme <- function(all_taxa, unique_taxa) {
    threshold <- detect_threshold_vec(unique_taxa)

    all_taxa <- all_taxa %>%
        dplyr::mutate(taxon = forcats::fct_expand(taxon, threshold) %>%
                   forcats::fct_relevel(threshold, after = 0))

    new_row <- tibble::tibble(
        taxon = factor(threshold, levels = levels(all_taxa[["taxon"]])),
    )

    all_taxa <- dplyr::bind_rows(new_row, all_taxa)

    global_factor_lvs <- all_taxa %>%
        dplyr::filter(taxon %in% unique_taxa ) %>%
        dplyr::mutate(taxon = droplevels(taxon))

    lvs <- levels(global_factor_lvs[["taxon"]])
    colors <- viridis::turbo(n = nrow(global_factor_lvs))
    taxa_colors <- stats::setNames(colors, lvs)
    taxa_colors
}

#' Add an "Other" observation to a relative abundance table.
#'
#' @param rel_abund_tb A relative abundance table in tibble format.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' rel_abund_phy(physeq) %>% add_other()
add_other <- function(rel_abund_tb){
    rel_abund_tb %>%
        dplyr::mutate(taxon = as.factor(taxon),
                      taxon = forcats::fct_expand(taxon, "Other")) %>%
        tibble::add_row(taxon = "Other")
}
