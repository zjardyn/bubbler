test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# phyloseq
devtools::load_all()

rel_abund_phy(physeq1,
              taxa_data = TRUE,
              meta_data = TRUE)

# tsv
counts <- system.file("extdata/tsv/seqtab.tsv", package = "bubbler")
taxa <- system.file("extdata/tsv/taxa.tsv", package = "bubbler")
meta <- system.file("extdata/tsv/metadata.tsv", package = "bubbler")

rel_abund_tsv(asv = counts,
              # taxa_data = taxa,
              meta_data = meta)

# qiime
counts_q <- system.file("extdata/qiime/table-dada2.qza", package = "bubbler")
taxa_q <- system.file("extdata/qiime/taxonomy.qza", package = "bubbler")
meta_q <- system.file("extdata/qiime/sample-metadata.tsv", package = "bubbler")

rel_abund_qiime(asv_qiime = counts_q,
                taxa_qiime = taxa_q,
                metadata_qiime = meta_q)
