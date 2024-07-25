new_rel_abund <- function(counts, taxonomy, metadata, taxa_level = NULL, rel_abund = NULL){
    structure(
        list(
            counts = counts,
            taxonomy = taxonomy,
            metadata = metadata,
            taxa_level = taxa_level,
            rel_abund = rel_abund
        ),
        class = "rel_abund"
    )
}

validate_rel_abund <- function(x) {
    if (!tibble::is_tibble(x$counts)) stop("Counts must be a tibble")
    if (!tibble::is_tibble(x$taxonomy)) stop("Taxonomy must be a tibble")
    if (!tibble::is_tibble(x$metadata)) stop("Metadata must be a tibble")

    if (ncol(x$counts) - 1 != nrow(x$taxonomy)) { # -1 for the feature ID column
        stop("Number of asvs must match between counts and taxonomy")
    }
    if (nrow(x$counts) != nrow(x$metadata)) {
        stop("Number of samples must match between counts and taxonomy")
    }
    # if (!is.null(x$taxon) && !(x$taxon %in% colnames(x$taxonomy))) {
    #     stop("Specified taxon must be a column in the taxonomy tibble")
    # }
    x
}

#' @export
rel_abund <- function(counts, taxonomy, metadata, taxon = NULL){
    x <- new_rel_abund(counts, taxonomy, metadata, taxon)
    validate_rel_abund(x)

}

#' @export
import_rel_abund <- function(counts, taxonomy, metadata, ...){
    UseMethod("import_rel_abund")
}

#' @export
import_rel_abund.default <- function(counts, taxonomy, metadata, ...){
    rel_abund(counts, taxonomy, metadata, ...)
}

#' @export
import_rel_abund.qiime <- function(counts_path, taxonomy_path, metadata_path, ...){
    counts <- asv_data_qiime(counts_path)
    taxonomy <- taxa_data_qiime(taxonomy_path)
    metadata <- meta_data_qiime(metadata_path)
    # rel_abund <-  rel_abund_qiime(counts_path, taxonomy_path, metadata_path)

    import_rel_abund(counts, taxonomy, metadata)
}

#' @export
import_rel_abund.tsv <- function(counts_path, taxonomy_path, metadata_path, ...){
    counts <- asv_data_tsv(counts_path)
    taxonomy <- taxa_data_tsv(taxonomy_path)
    metadata <- meta_data_tsv(metadata_path)
    # rel_abund <-  rel_abund_tsv(counts_path, taxonomy_path, metadata_path)

    import_rel_abund(counts, taxonomy, metadata)
}

#' @export
import_rel_abund.phy <- function(phy, ...){
    counts <- asv_data_phy(phy)
    taxonomy <- taxa_data_phy(phy)
    metadata <- meta_data_phy(phy)
    # rel_abund <-  rel_abund_phy(phy)

    import_rel_abund(counts, taxonomy, metadata)
}

#' @export
calculate_rel_abund <- function(counts, taxonomy, metadata, ...){
    UseMethod("calculate_rel_abund")
}

#' @export
calculate_rel_abund.default <- function(counts, taxonomy, metadata, ...){
    rel_abund(counts, taxonomy, metadata, ...)
}

