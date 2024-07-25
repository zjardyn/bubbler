
test_that("all_taxa works", {

    tb <- rel_abund_phy(phy = physeq,taxa_level = "Genus")
    tbp <- tb %>% pool_taxa(n = 19, label = FALSE)


    expect_no_error(all_taxa(tb))
    expect_true("Other" %in% all_taxa(tbp)[["taxon"]])

})

test_that("extract_unique_taxa works", {

    tb1 <- rel_abund_phy(phy = physeq, taxa_level = "Genus") %>% pool_taxa(n = 19, label = FALSE)
    tb2 <- rel_abund_phy(phy = physeq, taxa_level = "Genus") %>% add_other()

    l <- subset_high_low(tb1, subset = "low", flip = FALSE) %>% pool_taxa(n = 11, label = FALSE)
    p <- subset_high_low(tb1, subset = "high", flip = FALSE) %>% pool_taxa(n = 11, label = FALSE)

    expect_no_error(extract_unique_taxa(tb1, l, p))
    expect_no_error(extract_unique_taxa(tb2, l, p))

})


test_that("global_color_scheme works", {

    tb <- rel_abund_phy(phy = physeq,taxa_level = "Genus") %>% pool_taxa(n = 19, label = FALSE)
    l <- subset_high_low(tb, subset = "low", flip = FALSE) %>% pool_taxa(n = 11, label = FALSE)
    p <- subset_high_low(tb, subset = "high", flip = FALSE) %>% pool_taxa(n = 11, label = FALSE)

    all <- tb %>% all_taxa()
    taxa <- extract_unique_taxa(all, l, p)

    expect_no_error(global_colour_scheme(all, taxa))

})


test_that("adding_<other><unclassified> work", {
    tb1 <- rel_abund_phy(phy = physeq,taxa_level = "Genus") %>% add_other()
    tb2 <- rel_abund_phy(phy = physeq,taxa_level = "Genus") %>% add_unclassified()

    expect_true("Other" %in% tb1[["taxon"]])
    expect_true("Unclassified" %in% tb2[["taxon"]])
})
