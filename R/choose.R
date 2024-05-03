#' Subset samples from a rel_abund tibble and recompute rel_abund
#'
#' @param rel_abund_tab A tibble.
#' @param smp_selection A character vector of sample selections.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' smp_selection <- c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5")
#' choose_samples_rel_abund(rel_abund_tab, smp_selection)
subset_rel_abund <- function(rel_abund_tab, var, selection) {
    rel_abund_tab %>%
            dplyr::filter(!!rlang::sym(var) %in% selection) %>%
            dplyr::mutate(rel_abund = rel_abund/sum(rel_abund))
}
