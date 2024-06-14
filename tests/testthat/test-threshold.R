# devtools::load_all()
#
# counts_q <- system.file("extdata/qiime/table-dada2.qza", package = "bubbler")
# taxa_q <- system.file("extdata/qiime/taxonomy.qza", package = "bubbler")
# meta_q <- system.file("extdata/qiime/sample-metadata.tsv", package = "bubbler")
#
# rel_abund_q <- rel_abund_qiime(asv_qiime = counts_q,
#                 taxa_qiime = taxa_q,
#                 metadata_qiime = meta_q,
#                 var = "body_site")
#
# rel_abund_q %>%
#     pool_taxa() %>%
#     bar_plot(x_var = "body_site")
#

