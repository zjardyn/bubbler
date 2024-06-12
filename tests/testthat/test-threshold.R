devtools::load_all()

counts <- system.file("extdata/tsv/seqtab.tsv", package = "bubbler")
taxa <- system.file("extdata/tsv/taxa.tsv", package = "bubbler")
meta <- system.file("extdata/tsv/metadata.tsv", package = "bubbler")

rel_abund <- rel_abund_tsv(counts, taxa, meta_data = meta, taxa_level = "Genus")
choose_n_taxa(rel_abund, 20)
show_top_taxa(rel_abund)

rel_abund %>%
    pool_taxa(threshold = 0.005) %>%
    bar_plot()
# rel_abund_phy(physeq1, meta_data = T) %>%
#     pool_taxa(threshold = 0.01, var = "Location" )
    # bar_plot()

