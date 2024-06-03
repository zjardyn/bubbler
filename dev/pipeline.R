library(tidyverse)
library(phyloseq)

# qiime importing
library(qiime2R)

# order my sample read abundance, plot as line

asv_qiime <- "inst/extdata/qiime/table-dada2.qza"
taxa_qiime <- "inst/extdata/qiime/taxonomy.qza"
metadata_qiime <- "inst/extdata/qiime/sample-metadata.tsv"


# sample_id, asv, rel_abund, level, taxon
q <- rel_abund_qiime(
    asv_qiime = asv_qiime,
    taxa_qiime = taxa_qiime,
    metadata_qiime = metadata_qiime,
    taxa_level = "Genus", )

q2 <- q %>%
    pool_taxa(threshold = 0.005)

q_no <- q2 %>%
    filter(reported_antibiotic_usage == "No")

q_yes <- q2 %>%
    filter(reported_antibiotic_usage == "Yes")

new_layer <- q %>%
    group_by(body_site, reported_antibiotic_usage) %>%
    summarise(sum = sum(rel_abund)) %>%
    ungroup()

no <- new_layer %>%
    filter(reported_antibiotic_usage == "No")

yes <- new_layer %>%
    filter(reported_antibiotic_usage == "Yes")

q2 %>%
    ggplot(aes(x = body_site, y = rel_abund, fill = taxon)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~reported_antibiotic_usage)

colors <- c(" " = "black")

p1 <- q_no %>%
    ggplot(aes(x = body_site, y = rel_abund, fill = taxon)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_point(data = no, aes(x = body_site, y = sum, color = " "),
               inherit.aes = FALSE) +
    geom_line(data = no, aes(x = body_site, y = sum, group = 1, color = " "),
               inherit.aes = FALSE) + scale_color_manual(values = colors) +
    labs(x = "Body Site",
         y = "Relative abundance within samples",
         color = "Relative abundance between samples")

p2 <- q_yes %>%
    ggplot(aes(x = body_site, y = rel_abund, fill = taxon)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_point(data = yes, aes(x = body_site, y = sum),
               inherit.aes = FALSE) +
    geom_line(data = yes, aes(x = body_site, y = sum, group = 1),
               inherit.aes = FALSE) +
    labs(x = "Body Site",
         y = "Relative abundance within samples" )

library(patchwork)

p1 + p2 + plot_layout(guides = "collect", axes = "collect")



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


