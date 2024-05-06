library(tidyverse)
library(RColorBrewer)

generate_percentages <- function(N){
    random_N <- rlnorm(n = N, sdlog = 3)
    (random_N / sum(random_N)) * 100
}

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

