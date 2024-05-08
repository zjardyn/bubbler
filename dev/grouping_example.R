library(tidyverse)
library(RColorBrewer)
set.seed(666)

generate_percentages <- function(N){
    random_N <- rlnorm(n = N, sdlog = 3)
    (random_N / sum(random_N)) * 100
}

generate_counts <- function(n_smp = 100, n_asv = 20 , size = 1, prob = 0.01 ){
    dat <- rnbinom(n_smp * n_asv, size = size, prob = prob) %>%
        matrix(ncol = n_smp)
    hist(dat)
    colnames(dat) <- paste("Smp", 1:n_smp, sep = "")
    rownames(dat) <- paste("ASV", 1:n_asv, sep = "")
    dat %>%
        as.data.frame() %>%
        rownames_to_column(var = "asv") %>%
        as_tibble()
}
generate_counts()


# Parameters


my_data <- replicate(100, generate_percentages(8)) %>%
    as.data.frame() %>%
    cbind(class = letters[1:8]) %>%
    pivot_longer(cols = !class, names_to = "sample", values_to = "percentage")

my_data

# Don't worry about what happened here. I just simulated some data.
# Here we have an example data with 100 samples and 8 classes of member.

# Without optimization
# Let's see what will happen if we just make a plot as such without optimization.
no_reorder <- my_data %>%
    ggplot(aes(x = sample, y = percentage)) +
    geom_bar(stat = "identity", aes(fill = class)) +
    scale_fill_manual(values = brewer.pal(8, "Set2")) +
    theme_classic()
    # theme(axis.text.x = element_blank())

no_reorder

# Due to the number of samples and classes, it is very hard to discern anything from this graph without optimizing the order of bars.

# Group samples by peak class
# A prerequisite for reordering samples is grouping them.
# Usually there is a natural way to group them.
# For example grouping by control vs. treatment according to the design of the experiment.
# In this example, we don't have any experimental condition, I will just group them by peak class.
# This just means I am grouping samples by which class is the most abundant member in said sample.

sample_grouping <- my_data %>%
  group_by(sample) %>%
  slice_max(order_by = percentage) %>%
  select(class, sample) %>%
  rename(peak_class = class)

sample_grouping

# Reordering bars
# This is a 3 step process.
#
# 1. Grouping by sample grouping using `group_by()`.
# 2. Rank samples at _each level of sample grouping_ with `rank()`. R knows it should rank at each level of sample grouping because we called `group_by()`.
# 3. Reorder samples by rank using `reorder()`.

my_data_reordered <- my_data %>%
  inner_join(sample_grouping, by = "sample") %>%
  group_by(peak_class) %>%
  mutate(rank = rank(percentage)) %>%  # rank samples at the level of each peak subtype
  mutate(sample = reorder(sample, -rank)) %>%   # this reorders samples
  ungroup()

head(my_data_reordered)

# Plot reordered
# Now let's plot the optimized graph
bars_reordered <- my_data_reordered %>%
    ggplot(aes(x = sample, y = percentage)) +
    geom_bar(stat = "identity", aes(fill = class)) +
    scale_fill_manual(values = brewer.pal(8, "Set2")) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.spacing.x = unit(0.1, "line"))
bars_reordered

# ...and make a comparison.

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
