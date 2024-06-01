library(tidyverse)
library(phyloseq)

# qiime importing
library(qiime2R)

asv_data_qiime("inst/extdata/qiime/table-dada2.qza")
# taxa_data_qiime("inst/extdata/qiime/taxonomy.qza")
# meta_data_qiime("inst/extdata/qiime/sample-metadata.tsv")

asv_qiime <- "inst/extdata/qiime/table-dada2.qza"
taxa_qiime <- "inst/extdata/qiime/taxonomy.qza"
metadata_qiime <- "inst/extdata/qiime/sample-metadata.tsv"


rel_abund_qiime(asv_qiime = asv_qiime, metadata_qiime = metadata_qiime)


rel_abund_qiime(asv_qiime, taxa_qiime)

# new dataset
library(tidyverse)
asv <- system.file("extdata", "seqtab.tsv", package = "bubbler")
taxa <- system.file("extdata", "taxa.tsv", package = "bubbler")
meta_data <- system.file("extdata", "metadata.tsv", package = "bubbler")

rel_abund <- rel_abund_raw(asv, taxa , meta_data = meta_data,  taxa_level = "Genus")
# rel_abund_o <- arrange_taxa(rel_abund_o)
# rel_abund_o2 <- arrange_sample_by_taxa(rel_abund)

ggplot(rel_abund, aes(x = Depth, y = rel_abund)) +
    geom_bar(stat = "identity", aes(fill = taxon))

a <- rel_abund_phy(physeq1, taxa_level = "Genus")
t <- choose_n_taxa(a, 5)
b <- pool_taxa(a,t)
b_1 <- b %>%
    mutate(taxon = if_else(taxon == "Pseudomonas", "Unclassified", taxon))

c <- arrange_taxa(b_1, pooled_top = TRUE)

c %>%
    # mutate(taxon = fct_rev(taxon)) %>%
ggplot( aes(x = sample_id, y = rel_abund)) +
    geom_bar(stat = "identity", aes(fill = taxon))


