
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bubbler

<!-- badges: start -->
<!-- badges: end -->
<!-- Bubbler is a tidy approach to community composition visualization of amplicon-sequencing datasets (16S/18S rRNA), which is inspired by Pat Schloss's implementation of stacked barcharts in his Coding Club Youtube [series](https://www.youtube.com/@Riffomonas).
Bubbler makes relative abundance tables for visualization of amplicon-sequencing (16S/18S rRNA) datasets. Bubbler combines asv counts, taxonomic information, and optionally, meta data to create stacked barcharts, or bubble plots. Bubbler works with [dada2](https://github.com/benjjneb/dada2),  [qiime2](https://github.com/qiime2/qiime2), and [phyloseq](https://joey711.github.io/phyloseq/ output. 
&#10;## Installation
&#10;You can install bubbler from [GitHub](https://github.com/) with:
&#10;``` r
if (!requireNamespace("devtools", quietly = TRUE)){
    install.packages("devtools")
}
devtools::install_github("zjardyn/bubbler")
```
&#10;If you want to import data from a phyloseq object or qiime2 artifacts, install these packages:
&#10;```r
# install phyloseq
if(!requireNamespace("BiocManager")){
  install.packages("BiocManager")
}
BiocManager::install("phyloseq")
&#10;# install qiime2R
if (!requireNamespace("devtools", quietly = TRUE)){
    install.packages("devtools")
}
devtools::install_github("jbisanz/qiime2R")
&#10;```
&#10;
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
