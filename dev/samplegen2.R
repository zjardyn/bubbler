library(tidyverse)
options(scipen = 999)

n_smp = 10
n_taxa = 20

set.seed(123)
taxa <- read_tsv("inst/extdata/rdp/rdp_taxa.tsv", col_names = FALSE) %>%
    select(2) %>% pull()
taxa_subset <- sample(taxa, n_taxa)
taxa_tb <- as_tibble(do.call(rbind, strsplit(taxa_subset, ";", fixed = TRUE)))

colnames(taxa_tb) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus")
taxa_tb$asv <- paste0("ASV", 1:n_taxa)
taxa_tb <- taxa_tb[, c("asv", "Domain", "Phylum", "Class", "Order", "Family", "Genus")]

# generate_counts_nbinom <- function(n_smp = 100, n_asv = 5000 , size = 1, prob = 0.01 ){
#     dat <- rnbinom(n_smp * n_asv, size = size, prob = prob) %>%
#         matrix(ncol = n_smp)
#     hist(dat)
#     colnames(dat) <- paste("Smp", 1:n_smp, sep = "")
#     rownames(dat) <- paste("ASV", 1:n_asv, sep = "")
#     dat %>%
#         as.data.frame() %>%
#         rownames_to_column(var = "asv") %>%
#         as_tibble()
# }

generate_counts_lnorm <- function(n_smp = 100, n_asv = n_taxa , meanlog = 2, sdlog = 3 ){
    dat <- rlnorm(n_smp * n_asv, meanlog, sdlog) %>%
        matrix(ncol = n_smp)
    hist(dat)
    colnames(dat) <- paste("Smp", 1:n_smp, sep = "")
    rownames(dat) <- paste("ASV", 1:n_asv, sep = "")
    dat %>%
        as.data.frame() %>%
        rownames_to_column(var = "asv") %>%
        as_tibble() %>%
        mutate(across(where(is.numeric), floor))
}

set.seed(123)
counts_df <- generate_counts_lnorm()

set.seed(123)
metadata_df <- data.frame(
    sample_id = paste0("Smp", 1:n_smp),
    Depth = sample(1:30, n_smp, replace = TRUE),
    Carbon_source = sample(c("Glucose", "Hexadecane", "Styrene"), n_smp, replace = TRUE),
    Date = as.Date(sample(18000:19000, n_smp, replace = TRUE), origin = "1970-01-01")
)

write_tsv(taxa_tb, "inst/extdata/tsv/taxa.tsv")
write_tsv(counts_df, "inst/extdata/tsv/seqtab.tsv")
write_tsv(metadata_df, "inst/extdata/tsv/metadata.tsv")

# generate_counts <- function(N) {
#     sample(1:20000, N, replace = TRUE)
# }:w

#
# introduce_zeros_and_low_counts <- function(df, zero_prob = 0.1, low_count_prob = 0.2, max_low_count = 100, max_count = 500) {
#     modified_df <- df
#     for (i in 1:nrow(df)) {
#         for (j in 1:ncol(df)) {
#             if (runif(1) < zero_prob) {
#                 modified_df[i, j] <- 0
#             } else if (runif(1) < low_count_prob) {
#                 modified_df[i, j] <- min(max_count, sample(1:max_low_count, 1))
#             }
#         }
#     }
#     return(modified_df)
# }
# N_smp <- 100
# counts_list <- lapply(rep(20, N_smp), generate_counts)
# counts_df <- as.data.frame(do.call(cbind, counts_list))
# for (i in 1:3) {
#     counts_df <- introduce_zeros_and_low_counts(counts_df, max_count = 500)
# }
# counts_df <- cbind("asv" = paste0("ASV", 1:20), counts_df)
# colnames(counts_df) <- c("asv", paste0("Smp", 1:100))

# bacteria <- c(
#     "Bacteria, Proteobacteria, Alphaproteobacteria, Rhodospirillales, Acetobacteraceae, Acetobacter, pasteurianus",
#     "Bacteria, Proteobacteria, Alphaproteobacteria, Rhizobiales, Bradyrhizobiaceae, Bradyrhizobium, japonicum",
#     "Bacteria, Proteobacteria, Betaproteobacteria, Burkholderiales, Comamonadaceae, Acidovorax, facilis",
#     "Bacteria, Firmicutes, Bacilli, Bacillales, Bacillaceae, Bacillus, cereus",
#     "Bacteria, Bacteroidetes, Bacteroidia, Bacteroidales, Bacteroidaceae, Bacteroides, fragilis",
#     "Bacteria, Firmicutes, Bacilli, Lactobacillales, Streptococcaceae, Streptococcus, pneumoniae",
#     "Bacteria, Proteobacteria, Gammaproteobacteria, Pseudomonadales, Pseudomonadaceae, Pseudomonas, aeruginosa",
#     "Bacteria, Proteobacteria, Gammaproteobacteria, Pseudomonadales, Pseudomonadaceae, Pseudomonas, putida",
#     "Bacteria, Proteobacteria, Betaproteobacteria, Burkholderiales, Burkholderiaceae, Burkholderia, cepacia",
#     "Bacteria, Proteobacteria, Alphaproteobacteria, Rhizobiales, Bradyrhizobiaceae, Bradyrhizobium, elkanii",
#     "Bacteria, Proteobacteria, Gammaproteobacteria, Enterobacterales, Enterobacteriaceae, Escherichia, coli",
#     "Bacteria, Actinobacteria, Actinobacteria, Actinomycetales, Micrococcaceae, Micrococcus, luteus",
#     "Bacteria, Firmicutes, Clostridia, Clostridiales, Clostridiaceae, Clostridium, botulinum",
#     "Bacteria, Proteobacteria, Alphaproteobacteria, Rhizobiales, Bradyrhizobiaceae, Bradyrhizobium, betae",
#     "Bacteria, Actinobacteria, Actinobacteria, Actinomycetales, Corynebacteriaceae, Corynebacterium, diphtheriae",
#     "Bacteria, Proteobacteria, Alphaproteobacteria, Rhodospirillales, Acetobacteraceae, Acetobacter, pomorum",
#     "Bacteria, Firmicutes, Bacilli, Bacillales, Bacillaceae, Bacillus, subtilis",
#     "Bacteria, Bacteroidetes, Bacteroidia, Bacteroidales, Bacteroidaceae, Bacteroides, caccae",
#     "Bacteria, Proteobacteria, Alphaproteobacteria, Rhizobiales, Bradyrhizobiaceae, Bradyrhizobium, icense",
#     "Bacteria, Proteobacteria, Betaproteobacteria, Burkholderiales, Comamonadaceae, Acidovorax, aveneae"
# )
# bacteria_tb <- as_tibble(do.call(rbind, strsplit(bacteria, ", ", fixed = TRUE)))
# colnames(bacteria_tb) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
# bacteria_tb$asv <- paste0("ASV", 1:20)
# bacteria_tb <- bacteria_tb[, c("asv", "Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")]
#
# taxonomy_levels <- list(
#    Domain = "Bacteria",
#    Phylum = c("Acidobacteria",
#               "Actinobacteria",
#               "Bacteroidetes",
#               "Chloroflexi",
#               "Cyanobacteria",
#               "Firmicutes",
#               "Planctomycetes",
#               "Proteobacteria",
#               "Spirochaetes"),
#
#    Class = c("Alphaproteobacteria",
#              "Betaproteobacteria",
#              "Gammaproteobacteria",
#              "Bacilli",
#              "Bacteroidia",
#              "Actinobacteria",
#              "Clostridia",
#              "Actinomycetia",
#              )
#
# )
