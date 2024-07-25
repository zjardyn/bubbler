test_that("asv_tree works", {

    counts_q <- system.file("extdata/qiime/table-dada2.qza", package = "bubbler")
    expect_no_error(asv_tree(asv_data_qiime(counts_q)))
})

test_that("tip_order works", {

    counts_q <- system.file("extdata/qiime/table-dada2.qza", package = "bubbler")
    tree <- asv_tree(asv_data_qiime(counts_q))
    expect_no_error(tip_order(tree))
})
