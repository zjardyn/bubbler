# devtools::load_all()
#

tb <- rel_abund_phy(phy = physeq)
arrange_taxa(tb)

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
    lvls <- tb[["sample_id"]] %>%
        unique() %>%
        sample(size = length(unique(tb[["sample_id"]])), replace = FALSE)

    expect_no_error(arrange_var(tb, levels = "location"))
    expect_no_error(arrange_var(tb, levels = "depth"))
    expect_no_error(arrange_var(tb, levels = lvls))
})

test_that("arrange_taxa works", {
    tb <- rel_abund_phy(phy = physeq)
    expect_no_error(arrange_taxa(tb, pooled = "top", order = "bottom"))
    expect_no_error(arrange_taxa(tb, pooled = "bottom", order = "bottom"))
    expect_no_error(arrange_taxa(tb, pooled = "bottom", order = "top"))
    expect_no_error(arrange_taxa(tb, pooled = "top", order = "top"))
})
