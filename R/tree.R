utils::globalVariables(c("isTip", "y", "label"))
#' Generate a ggtree object based on the distance metric, from asv data
#'
#' @param asv_data An asv_data_<qiime, tsv, phy, bracken> tibble.
#' @param method Distance metric to be used. Default is "bray".
#'
#' @return A ggtree object.
#' @export
#'
#' @examples
#' counts_q <- system.file("extdata", "qiime", "table-dada2.qza", package = "bubbler")
#' asv_data_qiime(counts_q) %>%
#'     asv_tree()
asv_tree <- function(asv_data , method = "bray") {

        asv = asv_data %>%
            as.data.frame() %>%
            tibble::column_to_rownames(var = "sample_id") %>%
            as.matrix()

        vegan::vegdist(asv, method = method) %>%
            stats::hclust(method = "average") %>%
            ape::as.phylo() %>%
            ggtree::ggtree()

}

#' Grab the order of tips (sample_id) to use as levels for plotting.
#'
#' @param tree A ggtree object.
#'
#' @return A vector of level order.
#' @export
#'
#' @examples
#' counts_q <- system.file("extdata", "qiime", "table-dada2.qza", package = "bubbler")
#' asv_data_qiime(counts_q) %>%
#'     asv_tree() %>%
#'     tip_order()
tip_order <- function(tree){
        tree$data %>%
            dplyr::filter(isTip == TRUE) %>%
            dplyr::arrange(y) %>%
            dplyr::select(label) %>%
            rev() %>%
            dplyr::pull()
}
