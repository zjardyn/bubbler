library(tidyverse)
# library(bubbler)

# path <- "C:/Users/zjardyn/Desktop/alyssa_bracken"

path <- system.file("extdata", "bracken", package = "bubbler")

rel_abund <- rel_abund_bracken(path)

## Rel_abund_tabs
# rel_abund <- rel_abund0(data_s)
pooled_all<- pool_taxa(rel_abund, choose_n_taxa(rel_abund, 17), label = F)

rel_abund_l <- pooled_all %>%
    subset_high_low()

pooled_l <- rel_abund_l %>%
    pool_taxa(threshold = choose_n_taxa(rel_abund_l, 12), label = F)

rel_abund_h <- pooled_all %>%
    subset_high_low(flip = TRUE)

pooled_h <- rel_abund_h %>%
    pool_taxa(threshold = choose_n_taxa(rel_abund_h, 20), label = F)

unique_taxa_all <- rel_abund %>%
    add_other() %>%
    all_taxa()

unique_taxa <- extract_unique_taxa(pooled_all, pooled_h, pooled_l)

taxa_colors <- global_colour_scheme(unique_taxa_all, unique_taxa)

arranged_all <- pooled_all %>%
    arrange_taxa(pooled = "top") %>%
    arrange_sample_by_taxa()

arranged_l <-pooled_l %>%
    arrange_taxa(pooled = "top") %>%
    arrange_sample_by_taxa()

arranged_h <- pooled_h %>%
    arrange_taxa(pooled = "top") %>%
    arrange_sample_by_taxa()

# saveRDS(object = list(arranged_all, arranged_l, arranged_h, taxa_colors), file = paste(system.file("dev",package = "bubbler"), "/tabs.RDS", sep = ""))

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


layout <- "
AAAAAA
AAAAAA
BBBCCC
BBBCCC
DDDEEE
DDDEEE
"

p1 + p2 + p3 + p4 + p5 +
    plot_layout(design = layout,
                axis_titles = "collect",
                guides = "collect") &
    guides(shape = guide_legend(override.aes = list(size = 0.5))) &
    guides(color = guide_legend(override.aes = list(size = 0.5))) &
    guides(fill = guide_legend(override.aes = list(size = 0.5))) &
    theme(legend.title = element_text(size = 3),
               legend.text = element_text(size = 11))




