library(tidyverse)
library(phyloseq)
library(viridis)
library(ggnewscale)


asv_qiime <- system.file("extdata", "qiime", "table-dada2.qza", package = "bubbler")
taxa_qiime <- system.file("extdata", "qiime", "taxonomy.qza", package = "bubbler")
metadata_qiime <- system.file("extdata", "qiime", "sample-metadata.tsv", package = "bubbler")

import_rel_abund.qiime(asv_qiime, taxa_qiime, metadata_qiime)
rel_abund(asv_qiime, taxa_qiime, metadata_qiime)


asv_data_qiime(asv_qiime)
taxa_data_qiime(taxa_qiime)
meta_data_qiime(metadata_q)
###~%~ trying to get a nested colour scheme for phylum ~%~###

taxa_qiime <- system.file("extdata", "qiime", "taxonomy.qza", package = "bubbler")

tx <- taxa_data_qiime(taxa_qiime)

asv_qiime <- system.file("extdata", "qiime", "table-dada2.qza", package = "bubbler")
taxa_qiime <- system.file("extdata", "qiime", "taxonomy.qza", package = "bubbler")
metadata_qiime <- system.file("extdata", "qiime", "sample-metadata.tsv", package = "bubbler")

# generate rel_abund
q <- rel_abund_qiime(
    asv_qiime = asv_qiime,
    taxa_qiime = taxa_qiime,
    metadata_qiime = metadata_qiime,
    taxa_level = "Genus", )

# pool taxa, rename taxon to Genus (its true level) and select it.
qp <- pool_taxa(q, n_taxa = 20, label = FALSE)

taxa_pooled <- qp %>%
    rename(Genus = "taxon") %>%
    select(Genus) %>%
    distinct()

# merge the filtered
taxa_full <- inner_join(tx, taxa_pooled, by = "Genus" )

phylum_genus <- taxa_full %>%
    select(Phylum, Genus) %>%
    arrange(Phylum, Genus ) %>%
    distinct()

pg_nest <- phylum_genus %>%
    group_by(Phylum) %>%
    nest()

base_colours <- turbo(n = nrow(pg_nest), begin = 0.2, end = 0.8)
# cutoff <- ceiling(n_rows * 0.7)
2 * 0.8

# taxon is Genus actually.
tb <- tibble(taxon = character(), color = character())
for(i in 1:nrow(pg_nest)){
    phylum <- pg_nest$Phylum[i]
    genera <- pg_nest$data[[i]]

    phylum_shades_pal <- colorRampPalette(c(base_colours[i], "black"))
    cutoff <- ceiling(nrow(genera) * 0.5)
    phylum_shades <- phylum_shades_pal(nrow(genera) + cutoff)
    phylum_shades <- phylum_shades[1:(length(phylum_shades) - cutoff)]

    pgc <- tibble(taxon = pull(genera),
                  color = phylum_shades)

    tb <- bind_rows(tb, pgc)
}

qpc <- left_join(qp, tb, by = "taxon")

idx <- grepl("^<", qpc$taxon)
qpc[idx,"color"] <- "#999999"

qpc[qpc[["taxon"]] == "Unclassified", "color"] <- "#555555"


names(qpc$color) <- qpc$taxon

# colours are now set, now time to group by phylum for plotting!

cols <- qpc$color

tx2 <- tx %>%
    select(Genus, Phylum) %>%
    rename(taxon = "Genus") %>%
    na.omit() %>%
    distinct()

qpc2 <- left_join(qpc, tx2, by = "taxon")

idx <- grepl("^<", qpc2$taxon)
qpc2[idx,"Phylum"] <- "Other"

qpc2[qpc2[["taxon"]] == "Unclassified", "Phylum"] <- "Other"

groups <- qpc2 %>%
    distinct(Phylum, taxon) %>%
    mutate(order = as.numeric(forcats::fct_inorder(Phylum))) %>%
    split(.$Phylum)

qpc %>%
    arrange_taxa(pooled = "top") %>%
    arrange_sample_by_taxa() %>%
    ggplot() +
    lapply(groups, function(x){
        list(
            geom_col(aes(x = sample_id, y = rel_abund, fill = taxon), position = "fill"),
            scale_fill_manual(name = unique(x$Phylum),
                              values = cols, limits = x$taxon, na.value = "transparent",
                              guide = guide_legend(order = unique(x$order))),
            new_scale_fill()

        )
    }) + guides(fill=guide_legend(ncol=3)) +
    theme(
        # legend.position = "bottom"
        plot.margin=margin(t=30),
        legend.key.size = unit(10, "pt"),
        # legend.position = "bottom"
        # legend.box = "vertical"
    )




# # old way of doing it
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

asv_qiime <- system.file("extdata", "qiime", "table-dada2.qza", package = "bubbler")
taxa_qiime <- system.file("extdata", "qiime", "taxonomy.qza", package = "bubbler")
metadata_qiime <- system.file("extdata", "qiime", "sample-metadata.tsv", package = "bubbler")

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


# new dataset
library(tidyverse)
asv <- system.file("extdata/tsv", "seqtab.tsv", package = "bubbler")
taxa <- system.file("extdata/tsv", "taxa.tsv", package = "bubbler")
meta_data <- system.file("extdata/tsv", "metadata.tsv", package = "bubbler")

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

asv_qiime <- system.file("extdata", "qiime", "table-dada2.qza", package = "bubbler")
taxa_qiime <- system.file("extdata", "qiime", "taxonomy.qza", package = "bubbler")
metadata_qiime <- system.file("extdata", "qiime", "sample-metadata.tsv", package = "bubbler")

# sample_id, asv, rel_abund, level, taxon
q <- rel_abund_qiime(
    asv_qiime = asv_qiime,
    taxa_qiime = taxa_qiime,
    metadata_qiime = metadata_qiime,
    taxa_level = "Genus", ) %>%
    pool_taxa(n_taxa = 20, keep_metadata = TRUE)

q %>%
    taxon_italics() %>%
    arrange_taxa() %>%
    arrange_sample_by_taxa() %>%
    bubble_plot(color =  "reported_antibiotic_usage", italics = TRUE)

# taxon_italics <- function(rel_abund_tb){
#     rel_abund_tb %>%
#         dplyr::mutate(taxon = dplyr::if_else(taxon == detect_threshold(q), taxon, glue::glue("*{taxon}*")))
# }

library(ggplot2)
counts <- system.file("extdata", "qiime", "table-dada2.qza", package = "bubbler")
taxa <- system.file("extdata", "qiime", "taxonomy.qza", package = "bubbler")
metadata <- system.file("extdata", "qiime", "sample-metadata.tsv", package = "bubbler")

tb <- rel_abund_qiime(counts, taxa, metadata, taxa_level = "Genus") %>%
    arrange_var_abund(flip = TRUE)

unique_taxa <- tb %>%
    add_other() %>%
    all_taxa()

# the highest ten samples
tb_h <- tb %>%
    subset_high_low(subset = "high", n = 10) %>%
    pool_taxa(n_taxa = 10, label = FALSE) %>%
    arrange_taxa()

# lowest ten samples
tb_l <- tb %>%
    subset_high_low(subset = "low", n = 10) %>%
    pool_taxa(n_taxa = 10, label = FALSE) %>%
    arrange_taxa()

# the middle set of samples
tb_m <- tb %>%
    subset_high_low(subset = "low", n = 10, flip = TRUE) %>%
    subset_high_low(subset = "high", n = 10, flip = TRUE) %>%
    pool_taxa(n_taxa = 10, label = FALSE) %>%
    arrange_taxa()

subset_unique_taxa <- extract_unique_taxa(tb_h, tb_l, tb_m)

colourscheme <- global_colour_scheme(unique_taxa, subset_unique_taxa)

tb_h %>%
    bar_plot(global_colours = colourscheme) + ggtitle("Highest Ten")

tb_l %>%
    bar_plot(global_colours = colourscheme) + ggtitle("Lowest Ten")

tb_m %>%
    bar_plot(global_colours = colourscheme) + ggtitle("Lowest Ten")

## new section
tb1 <- rel_abund_phy(physeq, meta_data = TRUE, taxa_level = "Order") %>%
    pool_taxa(n = 18, keep_metadata = TRUE) %>%
    arrange_taxa() %>%
    arrange_sample_by_taxa()

tb2 <- rel_abund_phy(physeq, meta_data = TRUE, taxa_level = "Order",
                     var = "sample_id") %>%
    pool_taxa(n = 18, keep_metadata = TRUE) %>%
    arrange_taxa()
    # arrange_sample_by_taxa()
library(cowplot)
library(ggplot2)
p1 <- bar_plot(tb1, position = "fill") +
theme(panel.grid.major = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
    )
p2 <- bubble_plot(tb2, color = "date") +
    scale_color_viridis_c(option = "turbo") +
    theme_half_open() +
    geom_point(pch = 21) +
    theme(panel.grid.major = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
    )
p3 <- align_plots(p1, p2, align = "hv", axis = "tblr")
sp <- ggdraw(p3[[1]]) + draw_plot(p3[[2]])

library(hexSticker)
imgurl <- system.file("figures/bubbler_hex.png", package="bubbler")
s <- sticker(imgurl,
             package="bubbler",
             p_size=20, p_color = "black", p_y = 0.69,
             s_x=1, s_y=1, s_width=0.86, s_height=0.5,
             h_color = "white", h_size = 0.8,
             filename="inst/figures/hex_final.png")

logo <- system.file("figures/hex_final.png", package="bubbler")

library(vegan)
library(ape)
library(ggtree)
library(phyloseq)

# add bray-curtis tree beside bargraph
counts_q <- system.file("extdata", "qiime", "table-dada2.qza", package = "bubbler")
taxa_q <- system.file("extdata", "qiime", "taxonomy.qza", package = "bubbler")
metadata_q <- system.file("extdata", "qiime", "sample-metadata.tsv", package = "bubbler")

# asv_data = asv_data_phy(physeq)
# asv_data = asv_data_qiime(counts_q)
# asv_data = asv_data_tsv(counts_q)

asv_tree <- function(asv_data , method = "bray") {

        asv = asv_data %>%
            as.data.frame() %>%
            tibble::column_to_rownames(var = "sample_id") %>%
            as.matrix()

        vegan::vegdist(asv, method = method) %>%
            stats::hclust(method = "average") %>%
            ape::as.phylo() %>%
            ggtree::ggtree()

}

tip_order <- function(tree){
        tree$data %>%
            dplyr::filter(isTip == TRUE) %>%
            dplyr::arrange(y) %>%
            dplyr::select(label) %>%
            rev() %>%
            dplyr::pull()
}

tree <- asv_tree(asv_data_qiime(counts_q))
tip_ord <- tip_order(tree)

# Make the tree
p1 <- tree +
    theme(plot.margin = margin(0, -5, 0, 0))

# Make the relative abundance table and the sideways barplot.
q <- rel_abund_qiime(counts_q, taxa_q, metadata_q) %>%
    pool_taxa(n_taxa = 12, keep_metadata = TRUE)

p2 <- q %>%
    arrange_var(levels = tip_ord) %>%
    bar_plot(position = "fill", color = "body_site") +
    labs(x = NULL) +
    scale_x_discrete(position = "top") +
    coord_flip() +
    theme(plot.margin = margin(0, 0, 0, 5))

meta <- meta_data_qiime(metadata_q)
tree_m <- dplyr::inner_join(tree$data, meta, by = dplyr::join_by(label == sample_id))
tree$data <- tree_m

tree$data

p1 | p2





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

