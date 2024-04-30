# otumat = matrix(sample(1:100, 100, replace = TRUE), nrow = 10, ncol = 10)
# rownames(otumat) <- paste0("ASV", 1:nrow(otumat))
# colnames(otumat) <- paste0("Smp", 1:ncol(otumat))
#
# taxmat = matrix(sample(letters, 71, replace = TRUE), nrow = nrow(otumat), ncol = 7)
# rownames(taxmat) <- rownames(otumat)
# colnames(taxmat) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
#
# library("phyloseq")
# OTU = otu_table(otumat, taxa_are_rows = TRUE)
# TAX = tax_table(taxmat)
# physeq = phyloseq(OTU, TAX)
#
# sampledata = sample_data(data.frame(
#     Location = sample(LETTERS[1:4], size=nsamples(physeq), replace=TRUE),
#     Depth = sample(50:1000, size=nsamples(physeq), replace=TRUE),
#     row.names=sample_names(physeq),
#     stringsAsFactors=FALSE
# ))
# sampledata
#
# # library("ape")
# # random_tree = rtree(ntaxa(physeq), rooted=TRUE, tip.label=taxa_names(physeq))
# physeq1 = merge_phyloseq(physeq, sampledata)
# physeq2 = phyloseq(OTU, TAX, sampledata)
#
# identical(physeq1, physeq2)
#
# physeq1
# use_data(physeq1, overwrite = TRUE)
