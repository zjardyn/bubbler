
#' @export
asv_data_qiime <- function(qza){
   read_qza(qza)[["data"]] %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "sample_id") %>%
        tibble::as_tibble()
}

#' @export
taxa_data_qiime <- function(qza){
    read_qza(qza)[["data"]] %>%
        {purrr::quietly(tidyr::separate)}(Taxon, into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = "; ") %>% magrittr::extract2("result") %>%
        dplyr::mutate(dplyr::across(.cols = Domain:Species,
                      .fns = ~stringr::str_remove(.,"[a-z]__"))) %>%
        dplyr::mutate(dplyr::across(.cols = Domain:Species,
                      .fns = ~dplyr::na_if(., "") )) %>%
        dplyr::rename_with(~"asv", 1) %>%
        dplyr::select(-Confidence) %>%
        tibble::as_tibble()
}


#' @export
meta_data_qiime <- function(qza){
    metadata <- read_q2metadata(qza) %>%
        dplyr::rename_with(~"sample_id", 1) %>%
        tibble::as_tibble()

    metadata_names <- metadata %>%
        names() %>%
        #TODO: Add these to all parsing functions
        str_replace_all("-", "_") %>%
        str_replace_all(" ", "_")

    colnames(metadata) <- metadata_names
    metadata
}
