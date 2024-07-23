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
    expect_no_error(arrange_var(tb, levels = "location"))
})

test_that("arrange_taxa works", {
    tb <- rel_abund_phy(phy = physeq)
    expect_no_error(arrange_taxa(tb))
})
