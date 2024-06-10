library(tidyverse)
library(phyloseq)
library(viridis)
library(ggnewscale)
library(vegan)
library(ape)
library(ggtree)
# qiime importing
# library(qiime2R)

# fix pool_taxa, go through it to see if it is legit, add option to remove the threshold taxa
# count number of asvs per taxonomic group and add as variable

# add bray-curtis tree beside bargraph

otufile = system.file("extdata", "GP_otu_table_rand_short.txt.gz", package="phyloseq")
mapfile = system.file("extdata", "master_map.txt", package="phyloseq")
trefile = system.file("extdata", "GP_tree_rand_short.newick.gz", package="phyloseq")
rs_file = system.file("extdata", "qiime500-refseq.fasta", package="phyloseq")
qiimedata = import_qiime(otufile, mapfile, trefile, rs_file)

asv <- data.frame(phyloseq::otu_table(qiimedata))
asv <- t(asv)
bc_dist <- vegan::vegdist(asv, method = "bray")
hc <- hclust(bc_dist, method = "average")


phylo_tree <- as.phylo(hc)

# Use ggtree to get the tip order
ggtree_plot <- ggtree(phylo_tree)


tip_order <- ggtree_plot$data %>%
    filter(isTip == TRUE) %>%
    arrange(y) %>%
    select(label) %>%
    rev() %>%
    pull()

# Visualize the phylogenetic tree using ggtree
p1 <- ggtree(phylo_tree) +
    # geom_tiplab() +
    # theme_tree2() +
    theme(plot.margin = margin(0, -5, 0, 0))
#
q <- rel_abund_phy(qiimedata, taxa_level = "Phylum")

p1 <- q %>%
    arrange_variable(levels = tip_order) %>%
    bar_plot(position = "fill") +
    labs(x = NULL) +
    scale_x_discrete(position = "top") +
    scale_y_continuous(expand = c(0,0)) +
    coord_flip() +
    theme(plot.margin = margin(0, 0, 0, 5))

library(patchwork)
p1 | p2



library(viridis)
library(ggnewscale)

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

# generate rel_abund
q <- rel_abund_qiime(
    asv_qiime = asv_qiime,
    taxa_qiime = taxa_qiime,
    # metadata_qiime = metadata_qiime,
    taxa_level = "Genus", )

# pool taxa, rename taxon to Genus (its true level) and select it.
qp <- pool_taxa(q, 0.0035)

taxa_pooled <- qp %>%
    rename(Genus = "taxon") %>%
    select(Genus) %>%
    distinct()

# merge the filtered
taxa_full <- inner_join(tx, taxa_pooled, by = "Genus" )
    # select(Genus) %>%
    # unique()``

# taxa_unique <- taxa_full %>%
#     select(Phylum) %>%
#     unique() %>%
#     pull()

# Generate viridis colors for each group
# phylum_colors <- setNames(viridis::turbo(length(taxa_unique)), taxa_unique)


phylum_genus <- taxa_full %>%
    select(Phylum, Genus) %>%
    arrange(Phylum, Genus ) %>%
    distinct()

# new way of doing it

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

guides(fill=guide_legend(nrow=2,byrow=TRUE))
legend.key.height = unit(0.3, "npc"),
legend.key.width = unit(30, "pt"),
legend.key = element_blank(),
legend.title = element_text(margin=margin(t=-30)),
plot.margin=margin(t=50)

guides(color = guide_legend(nrow = 2))


# old way of doing it``




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


