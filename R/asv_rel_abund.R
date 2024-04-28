asv_rel_abund <- function(phy) {

    taxonomy <- phyloseq::tax_table(phy) %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "asv") %>%
        tibble::as_tibble()

    rel_abund <- phyloseq::otu_table(phy) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "sample_id") %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(-sample_id, names_to = "asv", values_to = "count") %>%
        dplyr::inner_join(., taxonomy, by =  "asv") %>%
        # what should we group by?
        dplyr::group_by(sample_id) %>%
        dplyr::mutate(rel_abund = count/sum(count)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-count)

    taxon_lvls <-rel_abund %>%
        dplyr::select(-asv, -sample_id, -rel_abund) %>%
        colnames()

    rel_abund %>%
        tidyr::pivot_longer(taxon_lvls,
                            names_to = "level",
                            values_to = "taxon")

}

choose_taxa_level <- function(rel_abund, taxon_level = "Phylum") {
   rel_abund %>%
        dplyr::filter(level == taxon_level)
}
choose_samples <- function(rel_abund, smp_selection) {
    rel_abund %>%
        dplyr::filter(sample_id == paste(smp_selection, sep = " || "))
}
# load("C:/Users/zjard/OneDrive/Desktop/bubbler/data/physeq1.rda")
# a <- asv_rel_abund(physeq1)
# b <- choose_taxa_level(a, taxon_level = "Genus")
# c <- choose_samples(b, c("Smp1", "Smp2", "Smp3", "Smp4"))
#
#
# a %>%
#     group_by(sample_id) %>%
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
