#' @export
all_taxa <- function(rel_abund_tb){

    rel_abund_tb %>%
        arrange_taxa() %>%
        dplyr::select(taxon) %>%
        dplyr::reframe(taxon = unique(taxon))
}

#' @export
combine_dataframes <- function(dataframes, by_var) {
    if (length(dataframes) < 2) {
        stop("At least two dataframes are required")
    }

    combined_df <- purrr::reduce(dataframes, function(df1, df2) {
        dplyr::full_join(df1, df2, by = by_var)
    })

    return(combined_df)
}

#' @export
extract_unique_taxa <- function(...) {

    dataframes <- list(...)
    combined_df <- combine_dataframes(dataframes, by_var = "taxon")

    combined_df$taxon %>%
        unique()

}

#' @export
global_colour_scheme <- function(all_taxa, unique_taxa) {
    threshold <- detect_threshold_vec(unique_taxa)

    all_taxa <- all_taxa %>%
        mutate(taxon = fct_expand(taxon, threshold) %>%
                   fct_relevel(threshold, after = 0))

    new_row <- tibble(
        taxon = factor(threshold, levels = levels(all_taxa[["taxon"]])),
    )

    all_taxa <- bind_rows(new_row, all_taxa)

    global_factor_lvs <- all_taxa %>%
        dplyr::filter(taxon %in% unique_taxa ) %>%
        mutate(taxon = droplevels(taxon))

    lvs <- levels(global_factor_lvs[["taxon"]])
    colors <- viridisLite::turbo(n = nrow(global_factor_lvs))
    taxa_colors <- setNames(colors, lvs)
    taxa_colors
}

#' @export
global_color_scheme <- function(all_taxa, unique_taxa) {
    threshold <- detect_threshold_vec(unique_taxa)

    all_taxa <- all_taxa %>%
        mutate(taxon = fct_expand(taxon, threshold) %>%
                   fct_relevel(threshold, after = 0))

    new_row <- tibble(
        taxon = factor(threshold, levels = levels(all_taxa[["taxon"]])),
    )

    all_taxa <- bind_rows(new_row, all_taxa)

    global_factor_lvs <- all_taxa %>%
        dplyr::filter(taxon %in% unique_taxa ) %>%
        mutate(taxon = droplevels(taxon))

    lvs <- levels(global_factor_lvs[["taxon"]])
    colors <- viridisLite::turbo(n = nrow(global_factor_lvs))
    taxa_colors <- setNames(colors, lvs)
    taxa_colors
}
