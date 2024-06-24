#' Subset samples from a rel_abund tibble and recompute rel_abund
#'
#' @param rel_abund_tab A tibble.
#' @param var A character vector of the variable to subset from.
#' @param selection A character vector of elements to select.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' smp_selection <- c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5")
#' subset_rel_abund(rel_abund_tab, var = "sample_id", selection = smp_selection)
subset_rel_abund <- function(rel_abund_tb, var, selection) {
    rel_abund_tb %>%
            dplyr::filter(!!rlang::sym(var) %in% selection) %>%
            dplyr::mutate(rel_abund = rel_abund/sum(rel_abund))
}

#' @export
subset_high_low <- function(rel_abund_tb, subset = "low", n = 10, flip = FALSE){
    if(missing(rel_abund_tb)){stop("Needs a rel_abund table")}

    subset <- match.arg(subset, c("low", "high"))

    subset_samples <- if (subset == "low") {

    rel_abund_tb %>%
        dplyr::group_by(sample_id) %>%
        dplyr::summarise(sum = sum(rel_abund)) %>%
        dplyr::arrange(sum) %>%
        head(n = n) %>% dplyr::select(sample_id) %>% dplyr::pull()

    } else {

    rel_abund_tb %>%
        dplyr::group_by(sample_id) %>%
        dplyr::summarise(sum = sum(rel_abund)) %>%
        dplyr::arrange(dplyr::desc(sum)) %>%
        head(n = n) %>% dplyr::select(sample_id) %>% dplyr::pull()

    }
        if(flip){

        rel_abund_tb %>%
            filter(!sample_id %in% subset_samples)

        } else {

        rel_abund_tb %>%
            filter(sample_id %in% subset_samples)
        }
}
