# devtools::load_all()
#

arrange_var0 <- function(rel_abund_tb, var = "sample_id", lvls){
    if(missing(rel_abund_tb)){stop("Provide a relative abundance table.")}
    if(missing(lvls)){stop("lvls not provided.")}

    if(is.numeric(rel_abund_tb[[lvls]])){

        rel_abund_tb %>%
            dplyr::mutate(!!rlang::sym(var) := as.factor(!!rlang::sym(var)),
                          !!rlang::sym(var) := forcats::fct_reorder(!!rlang::sym(var), !!rlang::sym(lvls)))

    } else {

       ord_lvls <- rel_abund_tb %>%
            dplyr::arrange(!!rlang::sym(lvls)) %>% dplyr::distinct(!!rlang::sym(var)) %>% dplyr::pull(!!rlang::sym(var))

        rel_abund_tb %>%
                dplyr::mutate(!!rlang::sym(var) := factor(!!rlang::sym(var)),  # Convert to factor first
                              !!rlang::sym(var) := forcats::fct_relevel(!!rlang::sym(var), ord_lvls))
    }
}


test_that("arrange_sample_by_taxa works", {
    tb <- rel_abund_phy(phy = physeq)
    expect_no_error(arrange_sample_by_taxa(tb))
})

test_that("arrange_var_abund works", {
    tb <- rel_abund_phy(phy = physeq)
    expect_no_error(arrange_var_abund(tb))
})

test_that("arrange_var works", {
    tb <- rel_abund_phy(phy = physeq, meta_data = TRUE)
    expect_no_error(arrange_var(tb, levels = "location"))
})

test_that("arrange_taxa works", {
    tb <- rel_abund_phy(phy = physeq)
    expect_no_error(arrange_taxa(tb))
})
