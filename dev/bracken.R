library(tidyverse)
# library(bubbler)

# path <- "C:/Users/zjardyn/Desktop/alyssa_bracken"

path <- system.file("extdata", "bracken", package = "bubbler")

rel_abund <- rel_abund_bracken(path)

## Rel_abund_tabs
# rel_abund <- rel_abund0(data_s)
pooled_all<- pool_taxa(rel_abund, choose_n_taxa(rel_abund, 17), label = F)

lowest_ten <- rel_abund %>%
    group_by(sample_id) %>%
    summarise(sum = sum(rel_abund)) %>%
    arrange(sum) %>%
    head(n = 10) %>% select(sample_id) %>% pull()

rel_abund_l <- data_s %>%
    filter(sample_id %in% lowest_ten) %>%
    rel_abund0()

pooled_l <- rel_abund_l %>%
    pool_taxa(threshold = choose_n_taxa(rel_abund_l, 12), label = F)


rel_abund_h <-  data_s %>%
    filter(!(sample_id %in% lowest_ten)) %>%
    rel_abund0()

pooled_h <- rel_abund_h %>%
    pool_taxa(threshold = choose_n_taxa(rel_abund_h, 20), label = F)

## setting global levels for all plots ##
rel_abund <- rel_abund0(data_s)

unique_taxa_all <- rel_abund %>%
    mutate(taxon = as.factor(taxon),
           taxon = fct_expand(taxon, "Other")) %>%
    add_row(sample_id = "1_S73", taxon = "Other", taxonomy_lvl = "S",
            kraken_assigned_reads = 0, rel_abund = 0) %>%
    arrange_taxa(pooled = "top")%>%
    select(taxon) %>%
    distinct()

## SET COLOURS ##
unique_taxa <- c(
    pooled_all$taxon,
    pooled_h$taxon,
    pooled_l$taxon
) %>%
    unique()

global_factor_lvs  <- unique_taxa_all %>%
    filter(taxon %in% unique_taxa) %>%
    mutate(taxon = droplevels(taxon))

lvs <- levels(global_factor_lvs$taxon)
## Setting global colourscheme ##

library(viridis)

colors <- turbo(n = nrow(global_factor_lvs))
taxa_colors <- setNames(colors, lvs)

arranged_all <- pooled_all %>%
    mutate(taxon = factor(taxon, levels = lvs)) %>%
    # arrange_taxa(pooled = "top") %>%
    arrange_sample_by_taxa()

arranged_l <-pooled_l %>%
    mutate(taxon = factor(taxon, levels = lvs)) %>%
    # mutate(taxon = levels(taxon))
    arrange_sample_by_taxa()

arranged_h <- pooled_h %>%
    mutate(taxon = factor(taxon, levels = lvs)) %>%
    # arrange_taxa(pooled = "top") %>%
    arrange_sample_by_taxa()


saveRDS(object = list(arranged_all, arranged_l, arranged_h, taxa_colors), file = paste(system.file("dev",package = "bubbler"), "/tabs.RDS", sep = ""))

p1 <- bar_plot(arranged_all) + scale_fill_manual(values = taxa_colors)

p2 <- bar_plot(arranged_l, position = "stack")+ scale_fill_manual(values = taxa_colors) +
    ggtitle("10 least abundant samples")
p3 <- bar_plot(arranged_l, position = "fill")+ scale_fill_manual(values = taxa_colors)

p4 <- bar_plot(arranged_h, position = "stack")+ scale_fill_manual(values = taxa_colors)
p5 <- bar_plot(arranged_h, position = "fill")+ scale_fill_manual(values = taxa_colors)

# path <- system.file("dev", package = "bubbler")
# saveRDS(file = paste(path, "/", "plot.RDS",sep = "" ), object = list(p1, p2, p3, p4, p5))

# library(patchwork)
#
# p1
#
# p2 + p3 + plot_layout(guides = "collect")
#
#
# p4 + p5 + plot_layout(guides = "collect")


# layout <- "
# AAAAAA
# AAAAAA
# BBBCCC
# BBBCCC
# DDDEEE
# DDDEEE
# "
#
# p1 + p2 + p3 + p4 + p5 +
#     plot_layout(design = layout,
#                 axis_titles = "collect",
#                 guides = "collect") &
#     guides(shape = guide_legend(override.aes = list(size = 0.5))) &
#     guides(color = guide_legend(override.aes = list(size = 0.5))) &
#     guides(fill = guide_legend(override.aes = list(size = 0.5))) &
#     theme(legend.title = element_text(size = 3),
#                legend.text = element_text(size = 11))
#
#


