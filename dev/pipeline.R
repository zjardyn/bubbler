library(tidyverse)
library(phyloseq)
load("data/physeq1.rda")

# qiime importing
library(qiime2R)
asv_data_q <- read_qza("inst/extdata/qiime/table-dada2.qza")
asv_data_q$data %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column(var = "sample_id") %>%
    as_tibble()

taxa_data_q <- read_qza("inst/extdata/qiime/taxonomy.qza")
taxa_data_q$data %>%
    {quietly(separate)}(Taxon, into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = "; ") %>% magrittr::extract2("result") %>%
    mutate(across(.cols = Domain:Species,
                  .fns = ~str_remove(.,"[a-z]__"))) %>%
    mutate(across(.cols = Domain:Species,
                  .fns = ~na_if(., "") )) %>%
    rename_with(~"asv", 1) %>%
    select(-Confidence) %>% as_tibble()

meta_data_q <- read_q2metadata("inst/extdata/qiime/sample-metadata.tsv")
meta_data_q %>%
    rename_with(~"sample_id", 1)


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


