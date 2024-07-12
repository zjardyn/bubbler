utils::globalVariables(c("rel_abund", "top_taxon"))

#' Arrange samples by the most abundant taxa.
#'
#' @param rel_abund_tb A relative abundance table in tibble form.
#'
#' @return A tibble with ordered samples.
#' @export
#'
#' @examples
#' rel_abund_phy(phy = physeq) %>%
#'      arrange_sample_by_taxa()
arrange_sample_by_taxa <- function(rel_abund_tb){
    if(missing(rel_abund_tb)){stop("Provide a relative abundance table.")}
   grouping <- rel_abund_tb %>%
       dplyr::group_by(sample_id) %>%
       dplyr::slice_max(order_by = rel_abund, with_ties = FALSE) %>%
       dplyr::select(taxon, sample_id) %>%
       dplyr::rename(top_taxon = taxon)

    dplyr::inner_join(rel_abund_tb, grouping, by = "sample_id") %>%
        dplyr::group_by(top_taxon) %>%
        dplyr::mutate(rank = rank(rel_abund)) %>%
        dplyr::mutate(sample_id = stats::reorder(sample_id, -rank)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-rank, -top_taxon)

}

#' Arrange taxa by abundance.
#'
#' @param rel_abund_tb A relative abundance table in tibble form.
#' @param pooled The order of pooled threshold, either "top" or "bottom".
#' @param order The order of taxa, either "top" or "bottom".
#'
#' @return A tibble with ordered taxon.
#' @export
#'
#' @examples
#' rel_abund_phy(phy = physeq) %>%
#'      arrange_taxa(pooled = "bottom")
arrange_taxa <- function(rel_abund_tb, pooled = "top", order = "bottom") {
   if(missing(rel_abund_tb)){stop("Provide a relative abundance table.")}
   pooled <- match.arg(pooled,  c("top", "bottom"))
   order <- match.arg(order,  c("top", "bottom"))

   grouping <- rel_abund_tb %>%
        dplyr::group_by(taxon) %>%
        dplyr::summarise(mean = mean(rel_abund))

    threshold <- detect_threshold(rel_abund_tb)

    if(order == "top"){

        if(pooled == "top"){

        rel_abund_arranged <- dplyr::inner_join(rel_abund_tb, grouping, by = "taxon") %>%
            dplyr::mutate(taxon = as.factor(taxon)) %>%
            dplyr::mutate(taxon = forcats::fct_reorder(taxon, mean)) %>%
            dplyr::mutate(taxon = forcats::fct_rev(taxon)) %>% # new
            dplyr::select(-mean) %>%
            dplyr::mutate(taxon = forcats::fct_relevel(taxon, threshold, after = 0),
                          taxon = forcats::fct_relevel(taxon, "Unclassified", after = 1))
        }
        if (pooled == "bottom"){

            rel_abund_arranged <- dplyr::inner_join(rel_abund_tb, grouping, by = "taxon") %>%
                dplyr::mutate(taxon = as.factor(taxon)) %>%
                dplyr::mutate(taxon = forcats::fct_reorder(taxon, mean)) %>%
                dplyr::mutate(taxon = forcats::fct_rev(taxon)) %>% # new
                dplyr::select(-mean) %>%
                dplyr::mutate(taxon = forcats::fct_relevel(taxon, "Unclassified", after = Inf),
                              taxon = forcats::fct_relevel(taxon, threshold, after =  Inf))
        }

    } else if (order  == "bottom"){

        if(pooled == "top"){

            rel_abund_arranged <- dplyr::inner_join(rel_abund_tb, grouping, by = "taxon") %>%
                dplyr::mutate(taxon = as.factor(taxon)) %>%
                dplyr::mutate(taxon = forcats::fct_reorder(taxon, mean)) %>%
                dplyr::select(-mean) %>%
                dplyr::mutate(taxon = forcats::fct_relevel(taxon, threshold, after = 0),
                              taxon = forcats::fct_relevel(taxon, "Unclassified", after = 1))
        }
        if (pooled == "bottom"){

            rel_abund_arranged <- dplyr::inner_join(rel_abund_tb, grouping, by = "taxon") %>%
                dplyr::mutate(taxon = as.factor(taxon)) %>%
                dplyr::mutate(taxon = forcats::fct_reorder(taxon, mean)) %>%
                dplyr::select(-mean) %>%
                dplyr::mutate(taxon = forcats::fct_relevel(taxon, "Unclassified", after = Inf),
                              taxon = forcats::fct_relevel(taxon, threshold, after =  Inf))
        }

    }

   rel_abund_arranged

}

#' Arrange a variable by the ordering of another variable
#'
#' @param rel_abund_tb A relative abundance table in tibble form.
#' @param variable The variable to be sorted.
#' @param levels The variable to sort with.
#'
#' @return A tibble with a sorted variable.
#' @export
#'
#' @examples
#' rel_abund_phy(phy = physeq, meta_data = TRUE) %>%
#'     arrange_variable(levels = "Location")
arrange_variable <- function(rel_abund_tb, variable = "sample_id", levels){
   if(missing(rel_abund_tb)){stop("Provide a relative abundance table.")}
   if(missing(levels)){stop("levels not provided.")}

   rel_abund_tb %>%
   dplyr::mutate(!!rlang::sym(variable) := as.factor(!!rlang::sym(variable)),
                 !!rlang::sym(variable) := forcats::fct_relevel(!!rlang::sym(variable), levels))
}
