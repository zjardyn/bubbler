library(tidyverse)
load("C:/Users/zjard/OneDrive/Desktop/bubbler/data/physeq1.rda")

asv <- choose_samples_asv_phy(physeq1, c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5"))
taxa <- taxa_data_phy(physeq1)
meta <- meta_data_phy(physeq1)

asv <- list(asv = asv,
     taxa = taxa,
     meta = meta)

# keep counts and recompute rel_abund var

choose_samples_asv_phy(physeq1, c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5"))


taxa_data_phy(physeq1)
meta_data_phy(physeq1)


a <- rel_abund(physeq1)
b <- choose_taxa_level(a, taxon_level = "Genus")
# c <- choose_samples(b, smp_selection = c("Smp1", "Smp2", "Smp3", "Smp4", "Smp5"))

threshold <- choose_n_taxa(c, 6)
d <- pool_taxa(c, threshold)

d %>%
    ggplot(aes(x = sample_id, y = rel_abund, fill = taxon)) +
    geom_bar(position = "fill", stat = "identity")

d %>%
    ggplot(aes(x = sample_id, y = rel_abund, fill = taxon)) +
    geom_bar(position = "stack", stat = "identity")



load("C:/Users/zjard/OneDrive/Desktop/bubbler/data/physeq1.rda")
a <- rel_abund_var(physeq1, "Location")
b <- choose_taxa_level(a, taxon_level = "Genus")
threshold <- choose_n_taxa(b, 6)

d <- pool_taxa(b, threshold, "Location")

d %>%
ggplot2::ggplot(ggplot2::aes(x = Location , y = rel_abund, fill = taxon)) +
    ggplot2::geom_bar(position = "stack", stat = "identity")



# ## #physeq1
h # a %>%
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
