library(tidyverse)
library(RColorBrewer)

bacteria <- c(
    "Bacteria, Proteobacteria, Alphaproteobacteria, Rhodospirillales, Acetobacteraceae, Acetobacter, pasteurianus",
    "Bacteria, Proteobacteria, Alphaproteobacteria, Rhizobiales, Bradyrhizobiaceae, Bradyrhizobium, japonicum",
    "Bacteria, Proteobacteria, Betaproteobacteria, Burkholderiales, Comamonadaceae, Acidovorax, facilis",
    "Bacteria, Firmicutes, Bacilli, Bacillales, Bacillaceae, Bacillus, cereus",
    "Bacteria, Bacteroidetes, Bacteroidia, Bacteroidales, Bacteroidaceae, Bacteroides, fragilis",
    "Bacteria, Firmicutes, Bacilli, Lactobacillales, Streptococcaceae, Streptococcus, pneumoniae",
    "Bacteria, Proteobacteria, Gammaproteobacteria, Pseudomonadales, Pseudomonadaceae, Pseudomonas, aeruginosa",
    "Bacteria, Proteobacteria, Gammaproteobacteria, Pseudomonadales, Pseudomonadaceae, Pseudomonas, putida",
    "Bacteria, Proteobacteria, Betaproteobacteria, Burkholderiales, Burkholderiaceae, Burkholderia, cepacia",
    "Bacteria, Proteobacteria, Alphaproteobacteria, Rhizobiales, Bradyrhizobiaceae, Bradyrhizobium, elkanii",
    "Bacteria, Proteobacteria, Gammaproteobacteria, Enterobacterales, Enterobacteriaceae, Escherichia, coli",
    "Bacteria, Actinobacteria, Actinobacteria, Actinomycetales, Micrococcaceae, Micrococcus, luteus",
    "Bacteria, Firmicutes, Clostridia, Clostridiales, Clostridiaceae, Clostridium, botulinum",
    "Bacteria, Proteobacteria, Alphaproteobacteria, Rhizobiales, Bradyrhizobiaceae, Bradyrhizobium, betae",
    "Bacteria, Actinobacteria, Actinobacteria, Actinomycetales, Corynebacteriaceae, Corynebacterium, diphtheriae",
    "Bacteria, Proteobacteria, Alphaproteobacteria, Rhodospirillales, Acetobacteraceae, Acetobacter, pomorum",
    "Bacteria, Firmicutes, Bacilli, Bacillales, Bacillaceae, Bacillus, subtilis",
    "Bacteria, Bacteroidetes, Bacteroidia, Bacteroidales, Bacteroidaceae, Bacteroides, caccae",
    "Bacteria, Proteobacteria, Alphaproteobacteria, Rhizobiales, Bradyrhizobiaceae, Bradyrhizobium, icense",
    "Bacteria, Proteobacteria, Betaproteobacteria, Burkholderiales, Comamonadaceae, Acidovorax, aveneae"
)
bacteria_tb <- as_tibble(do.call(rbind, strsplit(bacteria, ", ", fixed = TRUE)))
colnames(bacteria_tb) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
bacteria_tb$asv <- paste0("ASV", 1:20)
bacteria_tb <- bacteria_tb[, c("asv", "Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")]

generate_counts <- function(N) {
    sample(1:20000, N, replace = TRUE)
}

introduce_zeros_and_low_counts <- function(df, zero_prob = 0.1, low_count_prob = 0.2, max_low_count = 100, max_count = 500) {
    modified_df <- df
    for (i in 1:nrow(df)) {
        for (j in 1:ncol(df)) {
            if (runif(1) < zero_prob) {
                modified_df[i, j] <- 0
            } else if (runif(1) < low_count_prob) {
                modified_df[i, j] <- min(max_count, sample(1:max_low_count, 1))
            }
        }
    }
    return(modified_df)
}

counts_list <- lapply(rep(20, 10), generate_counts)
counts_df <- as.data.frame(do.call(cbind, counts_list))
for (i in 1:3) {
    counts_df <- introduce_zeros_and_low_counts(counts_df, max_count = 500)
}
counts_df <- cbind("asv" = paste0("ASV", 1:20), counts_df)
colnames(counts_df) <- c("asv", paste0("Smp", 1:10))

metadata_df <- data.frame(
    sample_id = paste0("Smp", 1:10),
    Location = sample(c("Location1", "Location2", "Location3"), 10, replace = TRUE),
    Carbon_source = sample(c("Glucose", "Hexadecane", "Styrene"), 10, replace = TRUE),
    Date = as.Date(sample(18000:19000, 10, replace = TRUE), origin = "1970-01-01")
)

write_tsv(bacteria_tb, "taxa.tsv")
write_tsv(counts_df, "seqtab.tsv")
write_tsv(metadata_df, "metadata.tsv")

my_data <- replicate(100, generate_percentages(8)) %>%
    as.data.frame() %>%
    cbind(class = letters[1:8]) %>%
    pivot_longer(cols = !class, names_to = "sample", values_to = "percentage")

my_data

no_reorder <- my_data %>%
    ggplot(aes(x = sample, y = percentage)) +
    geom_bar(stat = "identity", aes(fill = class)) +
    scale_fill_manual(values = brewer.pal(8, "Set2")) +
    theme_classic() +
    theme(axis.text.x = element_blank())

no_reorder

sample_grouping <- my_data %>%
    group_by(sample) %>%
    slice_max(order_by = percentage) %>%
    select(class, sample) %>%
    rename(peak_class = class)

sample_grouping

my_data_reordered <- my_data %>%
    inner_join(sample_grouping, by = "sample") %>%
    group_by(peak_class) %>%
    mutate(rank = rank(percentage)) %>%  # rank samples at the level of each peak subtype
    mutate(sample = reorder(sample, -rank)) %>%   # this reorders samples
    ungroup()

head(my_data_reordered)

bars_reordered <- my_data_reordered %>%
    ggplot(aes(x = sample, y = percentage)) +
    geom_bar(stat = "identity", aes(fill = class)) +
    scale_fill_manual(values = brewer.pal(8, "Set2")) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.spacing.x = unit(0.1, "line"))

wrap_plots(
    no_reorder +
        labs(title = "Without reordering bars"),
    bars_reordered +
        labs(title = "Bars reordered"),
    guides = "collect",
    nrow = 2
) &
    labs(y = "relative abundance (%)") &
    theme(title = element_text(size = 10))

