library(tidyverse)
library(phyloseq)
load("data/physeq1.rda")

# can we generate the same plot by subsetting samples before
# and after generating rel_abund?

rel_abund_tab <- rel_abund(phy = physeq1, meta_data = TRUE)
use_data(rel_abund_tab, overwrite = TRUE)

threshold <- choose_n_taxa(rel_abund_tab, 8)
use_data(threshold)

rel_abund_pool <- pool_taxa(rel_abund_tab, threshold)
use_data(rel_abund_pool)

smp_selection <- c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5")
a <- phyloseq::prune_samples(smp_selection, physeq1) %>%
rel_abund(taxa_level = "Genus", meta_data = FALSE)

show_top_taxa(a)

threshold <- choose_n_taxa(a, 6)
b <- pool_taxa(a, threshold)

b %>%
    ggplot(aes(x = sample_id, y = rel_abund, fill = taxon)) +
    geom_bar(position = "fill", stat = "identity")

b %>%
    ggplot(aes(x = sample_id, y = rel_abund, fill = taxon)) +
    geom_bar(position = "stack", stat = "identity")

a <- rel_abund(physeq1, taxa_level = "Genus")

smp_selection <- c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5")

b <- a %>%
    dplyr::filter(sample_id %in% smp_selection) %>%
    dplyr::mutate(rel_abund = rel_abund/sum(rel_abund))

threshold <- choose_n_taxa(b, 6)
c <- pool_taxa(b, threshold)


c %>%
    ggplot(aes(x = sample_id, y = rel_abund, fill = taxon)) +
    geom_bar(position = "fill", stat = "identity")

c %>%
    ggplot(aes(x = sample_id, y = rel_abund, fill = taxon)) +
    geom_bar(position = "stack", stat = "identity")


# ## #physeq1
 # a %>%
# #     group_by(sample_id) %>%
#     summarise(sum = sum(rel_abund))
#
# a %>%
#     group_by(taxon) %>%
#     summarise(sum = sum(rel_abund)) %>%
#     arrange(desc(sum))
#
# b %>%
#     group_by(sample_id) %>%
#     summarise(sum = sum(rel_abund))
#
# b %>%
#     group_by(taxon) %>%
#     summarise(sum = sum(rel_abund)) %>%
#     arrange(desc(sum))
#
# c %>%
#     group_by(sample_id) %>%
#     summarise(sum = sum(rel_abund))
#
# c %>%
#     group_by(taxon) %>%
#     summarise(sum = sum(rel_abund)) %>%
#     arrange(desc(sum))



# c %>%
#     group_by(taxon) %>%
#     summarise(sum = sum(rel_abund)) %>%
#     nrow()
#
# c  %>%
#     group_by(taxon) %>%
#     summarise(max = max(rel_abund)) %>%
#     arrange(desc(max))
#
# c %>%
#     group_by(sample_id) %>%
#     summarise(sum = sum(rel_abund))
