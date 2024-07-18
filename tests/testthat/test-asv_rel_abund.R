# devtools::load_all()

test_that("rel_abund_phy works", {
    expect_no_error(rel_abund_phy(physeq,
                  taxa_data = FALSE,
                  meta_data = FALSE))
    expect_no_error(rel_abund_phy(physeq,
                  taxa_data = FALSE,
                  meta_data = TRUE))
    expect_no_error(rel_abund_phy(physeq,
                  taxa_data = TRUE,
                  meta_data = FALSE))
    expect_no_error(rel_abund_phy(physeq,
                  taxa_data = TRUE,
                  meta_data = TRUE))
})

test_that("rel_abund_tsv works", {
    # tsv
    counts <- system.file("extdata/tsv/seqtab.tsv", package = "bubbler")
    taxa <- system.file("extdata/tsv/taxa.tsv", package = "bubbler")
    meta <- system.file("extdata/tsv/metadata.tsv", package = "bubbler")

    expect_no_error(rel_abund_tsv(counts,
                  taxa_data = NULL,
                  meta_data = NULL))
    expect_no_error(rel_abund_tsv(counts,
                  taxa_data = NULL,
                  meta_data = meta))
    expect_no_error(rel_abund_tsv(counts,
                  taxa_data = taxa,
                  meta_data = NULL))
    expect_no_error(rel_abund_tsv(counts,
                  taxa_data = taxa,
                  meta_data = meta))
})

test_that("rel_abund_qiime works", {
    # Qiime2
    counts <- system.file("extdata/qiime/table-dada2.qza", package = "bubbler")
    taxa <- system.file("extdata/qiime/taxonomy.qza", package = "bubbler")
    meta <- system.file("extdata/qiime/sample-metadata.tsv", package = "bubbler")

    expect_no_error(rel_abund_qiime(counts,
                  taxa_qiime = NULL,
                  metadata_qiime = NULL))
    expect_no_error(rel_abund_qiime(counts,
                  taxa_qiime = NULL,
                  metadata_qiime = meta))
    expect_no_error(rel_abund_qiime(counts,
                  taxa_qiime = taxa,
                  metadata_qiime = NULL))
    expect_no_error(rel_abund_qiime(counts,
                  taxa_qiime= taxa,
                  metadata_qiime = meta))
})

test_that("rel_abund_bracken works", {
    # bracken
    path <- system.file("extdata/bracken", package = "bubbler")

    expect_no_error(rel_abund_bracken(path, remove_human = TRUE))
    expect_no_error(rel_abund_bracken(path, remove_human = FALSE))
})

test_that("relative abundance sums are correct", {
    withr::local_package("dplyr")
    # qiime
    counts_q <- system.file("extdata/qiime/table-dada2.qza", package = "bubbler")
    taxa_q <- system.file("extdata/qiime/taxonomy.qza", package = "bubbler")
    meta_q <- system.file("extdata/qiime/sample-metadata.tsv", package = "bubbler")
    # tsv
    counts <- system.file("extdata/tsv/seqtab.tsv", package = "bubbler")
    taxa <- system.file("extdata/tsv/taxa.tsv", package = "bubbler")
    meta <- system.file("extdata/tsv/metadata.tsv", package = "bubbler")
    # bracken
    path <- system.file("extdata/bracken", package = "bubbler")

    phy <- rel_abund_phy(physeq) %>% summarise(sum = sum(rel_abund)) %>% pull(sum)
    phy_s <- rel_abund_phy(physeq, var = "sample_id") %>% summarise(sum = sum(rel_abund)) %>% pull(sum)
    phy_s_v <- rel_abund_phy(physeq, meta_data = TRUE, var = "location") %>% summarise(sum = sum(rel_abund)) %>% pull(sum)

    s <- n_distinct(rel_abund_phy(physeq)[["sample_id"]])
    s_v <- n_distinct(rel_abund_phy(physeq, meta_data = T)[["location"]])

    # sum of ungrouped rel_abund = 1
    # sum of grouped rel_abund = k levels of grouping variable so sum(rel_abund)/k = 1
    expect_equal(phy, 1)
    expect_equal(phy_s/s, 1)
    expect_equal(phy_s_v/s_v, 1)

    tsv <- rel_abund_tsv(counts) %>% summarise(sum = sum(rel_abund)) %>% pull(sum)
    tsv_s <- rel_abund_tsv(counts, var = "sample_id") %>% summarise(sum = sum(rel_abund)) %>% pull(sum)
    tsv_s_v<- rel_abund_tsv(counts, taxa, meta, var = "Carbon_source") %>% summarise(sum = sum(rel_abund)) %>% pull(sum)

    s <- n_distinct(rel_abund_tsv(counts)[["sample_id"]])
    s_v <- n_distinct(rel_abund_tsv(counts, taxa, meta)[["Carbon_source"]])

    expect_equal(tsv, 1)
    expect_equal(tsv_s/s, 1)
    expect_equal(tsv_s_v/s_v, 1)


    qiime <- rel_abund_qiime(counts_q) %>% summarise(sum = sum(rel_abund)) %>% pull(sum)
    qiime_s <- rel_abund_qiime(counts_q,
                               taxa_qiime = taxa_q,
                               var = "sample_id") %>% summarise(sum = sum(rel_abund)) %>% pull(sum)
    qiime_s_v <- rel_abund_qiime(counts_q,
                                taxa_qiime = taxa_q,
                                metadata_qiime = meta_q,
                                var = "body_site"
                                ) %>% summarise(sum = sum(rel_abund)) %>% pull(sum)

    s <- n_distinct(rel_abund_qiime(counts_q,
                                           taxa_qiime = taxa_q,
                                           var = "sample_id")[["sample_id"]])
    s_v <- n_distinct(rel_abund_qiime(counts_q, taxa_q, meta_q)[["body_site"]])

    expect_equal(qiime, 1)
    expect_equal(qiime_s/s, 1)
    expect_equal(qiime_s_v/s_v, 1)

})

