test_that("bar_plot works", {

    tb <- rel_abund_phy(physeq) %>%
        pool_taxa()

    expect_no_error(bar_plot(tb))

})
test_that("bubble_plot works", {

    tb <- rel_abund_phy(physeq) %>%
        pool_taxa()

    expect_no_error(bubble_plot(tb))

})
