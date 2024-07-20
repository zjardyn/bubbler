test_that("subset_rel_abund works", {
    tb <- rel_abund_phy(phy = physeq)
    smp_selection <- c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5")
    expect_no_error(subset_rel_abund(tb, selection = smp_selection))
})

test_that("subset_high_low works", {
    tb <- rel_abund_phy(phy = physeq)
    expect_no_error(subset_high_low(tb, n = 5))
})
