library(tidyverse)
library(phyloseq)
load("data/physeq1.rda")

a <- rel_abund(phy = physeq2,
               taxa_level = "Phylum",
               meta_data = TRUE)
subset_rel_abund(a, var = "sample", selection = c("enrichment", "enrichment2"))
subset_rel_abund <- function(rel_abund_tab, var, selection) {
    rel_abund_tab %>%
        dplyr::filter(!!rlang::sym(var) %in% selection) %>%
        dplyr::mutate(rel_abund = rel_abund/sum(rel_abund))
}
subset_rel_abund(a, var = "sample", selection =  )

a %>%
    dplyr::filter(sample %in% c("enrichment", "enrichment2")) %>%
    dplyr::mutate(rel_abund = rel_abund/sum(rel_abund))

# can we generate the same plot by subsetting samples before
# and after generating rel_abund?
plot_bar(physeq1, fill = "Family")

rel_abund(
          phy = physeq1,
          taxa_level = "Genus",
          var = "Location",
          meta_data = TRUE
          ) %>%
    summarise(sum = sum(rel_abund))

rel_abund_tab <- rel_abund(phy = physeq1, meta_data = TRUE)
use_data(rel_abund_tab, overwrite = TRUE)

threshold <- choose_n_taxa(rel_abund_tab, 8)
use_data(threshold)

rel_abund_pool <- pool_taxa(rel_abund_tab, threshold)
use_data(rel_abund_pool)

# smp_selection <- c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5")
# a <- phyloseq::prune_samples(smp_selection, physeq1) %>%
a <- rel_abund(physeq1, taxa_level = "Genus", meta_data = FALSE)

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
