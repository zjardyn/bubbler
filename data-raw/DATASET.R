## code to prepare `DATASET` dataset goes here

library(tidyverse)
library(phyloseq)
options(scipen = 999)

generate_taxa <- function(rdp_path = "inst/extdata/rdp/rdp_taxa.tsv", n_smp = 10, n_taxa = 20, seed = 123){
    set.seed(seed)
    taxa <- read_tsv(rdp_path, col_names = FALSE, show_col_types = FALSE) %>%
        select(2) %>% pull()
    taxa_subset <- sample(taxa, n_taxa)
    taxa_df <- as.data.frame(do.call(rbind, strsplit(taxa_subset, ";", fixed = TRUE)))
    colnames(taxa_df) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus")
    taxa_df$asv <- paste0("ASV", 1:n_taxa)
    taxa_df <- taxa_df[, c("asv", "Domain", "Phylum", "Class", "Order", "Family", "Genus")]
    taxa_df
}

generate_counts_lnorm <- function(n_smp = 10, n_taxa = 20 , meanlog = 2, sdlog = 3, seed = 123){
    set.seed(123)
    dat <- rlnorm(n_smp * n_taxa, meanlog, sdlog) %>%
        matrix(ncol = n_smp)
    hist(dat)
    colnames(dat) <- paste("Smp", 1:n_smp, sep = "")
    rownames(dat) <- paste("ASV", 1:n_taxa, sep = "")
    dat %>%
        as.data.frame() %>%
        rownames_to_column(var = "asv") %>%
        as_tibble() %>%
        mutate(across(where(is.numeric), floor))
}

generate_metadata <- function(n_smp = 10, seed = 123){
    set.seed(seed)
    data.frame(
        sample_id = paste0("Smp", 1:n_smp),
        depth = sample(1:30, n_smp, replace = TRUE),
        location = sample(c("place_a", "place_b", "place_c"), n_smp, replace = TRUE),
        date = as.Date(sample(18000:19000, n_smp, replace = TRUE), origin = "1970-01-01"))
}

generate_phyloseq <- function(N_smp = 10, N_taxa = 20, Seed = 123456) {

    counts_df <-  generate_counts_lnorm(n_smp = N_smp, n_taxa = N_taxa, seed = Seed)
    taxa_df <- generate_taxa(n_smp = N_smp, n_taxa = N_taxa, seed = Seed)
    metadata_df <- generate_metadata(n_smp = N_smp, seed = Seed)

    otumat <- counts_df %>%
        column_to_rownames(var =  "asv") %>%
        as.matrix()
    taxmat <- taxa_df %>%
        column_to_rownames(var = "asv") %>%
        as.matrix()
    physeq <- phyloseq(otu_table(otumat, taxa_are_rows = TRUE),
                       tax_table(taxmat))
    metadata_df2 <- metadata_df %>%
        as.data.frame(,row.names = sample_names(physeq))
    merge_phyloseq(physeq, sample_data(metadata_df2))

}
physeq <- generate_phyloseq(N_smp = 50, N_taxa = 20, Seed = 666)

use_data(physeq, overwrite = TRUE)

# devtools::load_all()

# usethis::use_data(DATASET, overwrite = TRUE)

