
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bubbler

<!-- badges: start -->
<!-- badges: end -->

Bubbler is a tidy approach to amplicon sequencing visualization. Bubbler
makes relative abundance tables from amplicon-sequencing (16S/18S rRNA)
datasets. Bubbler combines asv counts, taxonomic information, and
optionally, meta data to create stacked barcharts, or bubble plots.
Bubbler can import data from [dada2](https://github.com/benjjneb/dada2),
[qiime2](https://github.com/qiime2/qiime2),
[phyloseq](https://joey711.github.io/phyloseq/) and
[Bracken](https://github.com/jenniferlu717/Bracken) output. Relative
abundance tables can be modified, with various methods implemented in
bubbler.

## Installation

You can install bubbler from [GitHub](https://github.com/) with:

``` r
if (!requireNamespace("devtools", quietly = TRUE)){
    install.packages("devtools")
}
devtools::install_github("zjardyn/bubbler")
```

bubbler

A bubbler workflow has three steps:

    1. Import data into a relative abundance table.
    2. Modify the `rel_abund` table. 
    3. Make a `ggplot2` object, with stacked-bar or bubble plot aesthetics.

Here, I am using `tidyverse` notation:

``` r
library(bubbler)
# 1. import data into rel_abund.
rel_abund_phy(physeq, taxa_level = "Genus") %>%
    
    # 2. modify  rel_abund
    pool_taxa(n_taxa = 16) %>%
    
    # arrange plotting variables by most abundant taxa.
    arrange_taxa() %>%
    arrange_sample_by_taxa() %>%
    
    # 3. plot
    bar_plot(position = "fill")
#> Loading required package: phyloseq
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" /> The
[bubbler documentation](https://zjardyn.github.io/bubbler/) has examples
for importing data, constructing rel_abund tables, and other aspects of
the package.

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
