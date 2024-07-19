#' Subset samples from a relative abundance table.
#'
#' @param rel_abund_tb A tibble.
#' @param var The variable to subset from.
#' @param selection The elements to select.
#' @param recompute Logical, whether to recompute rel_abund.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' smp_selection <- c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5")
#' subset_rel_abund(rel_abund_phy(physeq), var = "sample_id", selection = smp_selection)
subset_rel_abund <- function(rel_abund_tb, var, selection, recompute = FALSE) {

    if(recompute == TRUE){

        rel_abund_tb %>%
                dplyr::filter(!!rlang::sym(var) %in% selection) %>%
                dplyr::mutate(rel_abund = rel_abund/sum(rel_abund))

    } else {

        rel_abund_tb %>%
                dplyr::filter(!!rlang::sym(var) %in% selection)
    }

}

#' Subset the highest or lowest abundance samples from a relative abundance table.
#'
#' @param rel_abund_tb A relative abundance table in tibble form.
#' @param subset The subset choice, of "high" or "low"
#' @param n The number of samples.
#' @param flip Subset the opposite set of samples, logical.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' rel_abund_phy(phy = physeq) %>%
#'   subset_high_low(n = 5)
subset_high_low <- function(rel_abund_tb, subset = "low", n = 10, flip = FALSE){
    if(missing(rel_abund_tb)){stop("Needs a rel_abund table")}

    subset <- match.arg(subset, c("low", "high"))

    if (subset == "low") {
    subset_samples <- rel_abund_tb %>%
        dplyr::group_by(sample_id) %>%
        dplyr::summarise(sum = sum(rel_abund)) %>%
        dplyr::arrange(sum) %>%
        utils::head(n = n) %>% dplyr::select(sample_id) %>% dplyr::pull()

    } else if(subset == "high") {

    subset_samples <- rel_abund_tb %>%
        dplyr::group_by(sample_id) %>%
        dplyr::summarise(sum = sum(rel_abund)) %>%
        dplyr::arrange(sum) %>%
        utils::tail(n = n) %>% dplyr::select(sample_id) %>% dplyr::pull()

    }
        if(flip){

        rel_abund_tb %>%
            dplyr::filter(!sample_id %in% subset_samples)

        } else {

        rel_abund_tb %>%
            dplyr::filter(sample_id %in% subset_samples)
        }
}
