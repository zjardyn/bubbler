library(tidyverse)
library(phyloseq)

# qiime importing
library(qiime2R)

# fix pool_taxa, go through it to see if it is legit, add option to remove the threshold taxa
# count number of asvs per taxonomic group and add as variable

# add bray-curtis tree beside bargraph
# organize legend by a higher taxonomic level using ggnewscale
# stackoverflow: grouping legend by higher classification, filum and genus?
#
# library(RColorBrewer)
# RColorBrewer::display.brewer.all()
library(ggplot2)
library(viridis)

# Function to display a vector of colors with 10 colors per row using ggplot2
display_colors_ggplot <- function(color_vector, colors_per_row = 10) {
    n <- length(color_vector)
    rows <- ceiling(n / colors_per_row)
    data <- data.frame(
        x = rep(1:colors_per_row, length.out = n),
        y = rep(1:rows, each = colors_per_row)[1:n],
        color = color_vector
    )

    ggplot(data, aes(x = factor(x), y = factor(y), fill = color)) +
        geom_tile() +
        scale_fill_identity() +
        theme_void() +
        theme(
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()
        )
        # geom_text(aes(label = color), angle = 90, hjust = 1, vjust = 0.5, size = 3)
}

###~%~ trying to get a nested colour scheme for phylum ~%~###

taxa_qiime <- "inst/extdata/qiime/taxonomy.qza"
tx <- taxa_data_qiime(taxa_qiime)
tx %>%
    select(Phylum) %>%
    group_by(Phylum) %>%
    summarise(sum = n()) %>%
    arrange(desc(sum))

asv_qiime <- "inst/extdata/qiime/table-dada2.qza"
taxa_qiime <- "inst/extdata/qiime/taxonomy.qza"
metadata_qiime <- "inst/extdata/qiime/sample-metadata.tsv"

# sample_id, asv, rel_abund, level, taxon
q <- rel_abund_qiime(
    asv_qiime = asv_qiime,
    taxa_qiime = taxa_qiime,
    # metadata_qiime = metadata_qiime,
    taxa_level = "Genus", )

qp <- pool_taxa(q, 0.001)

qp$taxon %>%
    unique()

taxa_pooled <- qp %>%
    rename(Genus = "taxon") %>%
    select(Genus)

taxa_full <- inner_join(tx, taxa_pooled, by = "Genus",
                        relationship = "many-to-many") %>%
    distinct()
    # select(Genus) %>%
    # unique()``
taxa_unique <- taxa_full %>%
    select(Phylum) %>%
    unique() %>%
    pull()

# Generate viridis colors for each group
phylum_colors <- setNames(viridis::turbo(length(taxa_unique)), taxa_unique)


phylum_genus <- taxa_full %>%
    select(Phylum, Genus) %>%
    arrange(Phylum, Genus ) %>%
    distinct()

# new way of doing it

pg_nest <- phylum_genus %>%
    group_by(Phylum) %>%
    nest()

for(i in 1:nrow(pg_nest)){
    phylum <- pg_nest$Phylum[i]
    group
}




# old way of doing it

phylum_genus_col <- phylum_genus %>%
    mutate(color = phylum_colors[Phylum])


# Group by Phylum and generate shades of colors
phylum_genus_col2 <- phylum_genus_col %>%
    group_by(Phylum) %>%
    mutate(color = generate_shades(first(Phylum), n())) %>%
    ungroup()

display_colors_ggplot(phylum_genus_col2$color)

# Function to generate shades for each phylum with specified base color
generate_shades <- function(phylum, n) {
    base_color <- phylum_colors[[phylum]]
    scales::seq_gradient_pal(base_color, "white", space = "Lab")(seq(0.1, 0.7, length.out = n))
}

library(RColorBrewer)

RColorBrewer::display.brewer.all()
# viridis::turbo(n_rows
RColorBrewer::brewer.pal(11, "Spectral")

end_col <- "black"
n_rows <- 10 # the number of different colours
n_cols <- 15 # the number of different shades of a given colour
cutoff <- ceiling(n_rows * 0.7)

cols_full <- vector()
for(i in turbo(n_rows, begin = 0.2,end = 0.8)){
   col_pal <- colorRampPalette(c(i, "black"))
   cols <- col_pal(n_cols + cutoff)
   cols <- cols[1:(length(cols) - cutoff)]
   cols_full <- c(cols_full, cols)
}
display_colors_ggplot(cols_full, colors_per_row = n_cols)


# "#30123bff" "#a2fc3cff" "#7a0403ff"
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

# arrange samples by a chosen taxa

q <- rel_abund_qiime(
    asv_qiime = asv_qiime,
    taxa_qiime = taxa_qiime,
    metadata_qiime = metadata_qiime,
    taxa_level = "Genus" )

q %>%
    pool_taxa(threshold = choose_n_taxa(q,12))
    # unite("date",day, month, year) %>%
    # mutate(date = as.factor(dmy(date))) %>%
    # arrange_taxa(pooled = "top")
    # bar_plot(x_var = "date", position = "fill") + guides(fill = guide_legend(reverse = TRUE))

# add line for taxa of interest
# Arrange by taxa
tax <- "Pseudomonas"

(taxon_sums <- q %>%
    filter(taxon == tax) %>%
    group_by(sample_id) %>%
    summarise(taxon_sum = sum(count)) %>%
    arrange(desc(taxon_sum))
)
q2 <- inner_join(q, taxon_sums, by = "sample_id") %>%
    mutate(sample_id = as.factor(sample_id)) %>%
    mutate(sample_id = fct_reorder(sample_id, taxon_sum, .desc = TRUE))

pool_taxa(q2, threshold = choose_n_taxa(q2, 8)) %>%
    arrange_taxa(pooled_top = TRUE)%>%
    bar_plot(position = "fill") + guides(fill = guide_legend(reverse = TRUE))


bar_plot <- function(rel_abund_tab, x_var = "sample_id", position = c("stack", "fill")){

    p <- ggplot(rel_abund_tab, aes(x = !!rlang::sym(x_var), y = rel_abund, fill = taxon))
    position <- match.arg(position)
    if(position == "stack") {
        p <- p + geom_bar(stat = "identity", position = position)
    }
    if(position == "fill") {
        p <- p + geom_bar(stat = "identity", position = position)
    }
    p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

# new dataset
library(tidyverse)
asv <- system.file("extdata", "seqtab.tsv", package = "bubbler")
taxa <- system.file("extdata", "taxa.tsv", package = "bubbler")
meta_data <- system.file("extdata", "metadata.tsv", package = "bubbler")

rel_abund <- rel_abund_tsv(asv, taxa , meta_data = meta_data,  taxa_level = "Genus")
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


