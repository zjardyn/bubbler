# devtools::load_all()

test_that("show_top_taxa works", {

    counts_q <- system.file("extdata/qiime/table-dada2.qza", package = "bubbler")
    taxa_q <- system.file("extdata/qiime/taxonomy.qza", package = "bubbler")
    meta_q <- system.file("extdata/qiime/sample-metadata.tsv", package = "bubbler")

    expect_no_error(rel_abund_qiime(counts_q, taxa_q, meta_q) %>% show_top_taxa())

})

test_that("choose_n_taxa works", {

    counts_q <- system.file("extdata/qiime/table-dada2.qza", package = "bubbler")
    taxa_q <- system.file("extdata/qiime/taxonomy.qza", package = "bubbler")
    meta_q <- system.file("extdata/qiime/sample-metadata.tsv", package = "bubbler")

    expect_no_error(rel_abund_qiime(counts_q, taxa_q, meta_q) %>% choose_n_taxa())

})

test_that("pool_taxa works", {

    counts_q <- system.file("extdata/qiime/table-dada2.qza", package = "bubbler")
    taxa_q <- system.file("extdata/qiime/taxonomy.qza", package = "bubbler")
    meta_q <- system.file("extdata/qiime/sample-metadata.tsv", package = "bubbler")

    expect_no_error(rel_abund_qiime(counts_q, taxa_q, meta_q) %>% pool_taxa(keep_metadata = TRUE))
    expect_no_error(rel_abund_qiime(counts_q, taxa_q, meta_q) %>% pool_taxa())

})

test_that("detect_threshold works", {

    counts_q <- system.file("extdata/qiime/table-dada2.qza", package = "bubbler")
    taxa_q <- system.file("extdata/qiime/taxonomy.qza", package = "bubbler")
    meta_q <- system.file("extdata/qiime/sample-metadata.tsv", package = "bubbler")
    tb <- rel_abund_qiime(counts_q, taxa_q, meta_q) %>% pool_taxa()
    tb_nolabel <- rel_abund_qiime(counts_q, taxa_q, meta_q) %>% pool_taxa(label = FALSE)
    thresh_tb <- detect_threshold(tb)
    thresh_nolabel <- detect_threshold(tb_nolabel)

    expect_match(thresh_tb, "<")
    expect_match(thresh_nolabel, "Other")

})
