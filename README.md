
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bubbler

<!-- badges: start -->
<!-- badges: end -->

Bubbler is a tidy approach to community composition visualization of
amplicon-sequencing datasets (breath), which is inspired by Pat
Schloss’s implementation of 16S rRNA stacked barcharts in his Coding
Club Youtube [series](https://www.youtube.com/@Riffomonas). Bubbler
generates ASV (Actual Sequence Variant) relative abundance tables from
ASV sequence tables and taxonomic classifications generated from
[dada2](https://github.com/benjjneb/dada2) or
[qiime2](https://github.com/qiime2/qiime2).

## Installation

You can install bubbler from [GitHub](https://github.com/) with:

``` r
if (!requireNamespace("devtools", quietly = TRUE)){
    install.packages("devtools")
}
devtools::install_github("zjardyn/bubbler")
```

If you want to import data from phyloseq or qiime2, install these
respective packages:

``` r
# install phyloseq
if(!requireNamespace("BiocManager")){
  install.packages("BiocManager")
}
BiocManager::install("phyloseq")

# install qiime2R
if (!requireNamespace("devtools", quietly = TRUE)){
    install.packages("devtools")
}
devtools::install_github("jbisanz/qiime2R")
```

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
