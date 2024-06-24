
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bubbler

<!-- badges: start -->
<!-- badges: end -->

Bubbler makes relative abundance tables for visualization of
amplicon-sequencing (16S/18S rRNA) datasets. Bubbler combines asv
counts, taxonomic information, and optionally, meta data to create
stacked barcharts, or bubble plots. Bubbler works with
[dada2](https://github.com/benjjneb/dada2),
[qiime2](https://github.com/qiime2/qiime2), and
[phyloseq](https://joey711.github.io/phyloseq/) output.

## Installation

You can install bubbler from [GitHub](https://github.com/) with:

``` r
if (!requireNamespace("devtools", quietly = TRUE)){
    install.packages("devtools")
}
devtools::install_github("zjardyn/bubbler")
```

bubbler

A bubbler workflow typically has three steps: 1. Importing data into a
relative abundance table. 2. Modifying the `rel_abund` table. 3.
Plotting, in the form of a stacked-bar or bubble plot

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
