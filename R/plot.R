#' Generate a ggplot2 stacked barplot from a relative abundance table.
#'
#' @param rel_abund_tb A relative abundance table in tibble format.
#' @param x_var The x variable for plotting.
#' @param position "fill" or "stack", should the plotting area be filled in.
#' @param width The width of the bar.
#' @param color A variable to color bars by.
#' @param true_line Logical, whether to show a true abundance line.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' rel_abund_phy(physeq) %>% bar_plot()
#' @importFrom ggplot2 ggplot aes geom_bar geom_point geom_line theme element_text theme
bar_plot <- function(rel_abund_tb, x_var = "sample_id", position = "stack", width = 1, color = NULL, true_line = FALSE, italics = FALSE){
    if(missing(rel_abund_tb)){stop("Please provide rel_abund table.")}

    p <- ggplot(rel_abund_tb, aes(x = !!rlang::sym(x_var), y = rel_abund, fill = taxon))
    if (!is.null(color)) {
        position <- match.arg(position, c("stack", "fill"))
        if (position == "stack") {
            p <- p + geom_bar(stat = "identity", position = position, width = width, aes(color = !!rlang::sym(color)))
        } else if (position == "fill") {
            p <- p + geom_bar(stat = "identity", position = position, width = width, aes(color = !!rlang::sym(color)))
        }
    } else {
        position <- match.arg(position, c("stack", "fill"))
        if (position == "stack") {
            p <- p + geom_bar(stat = "identity", position = position, width = width)
        } else if (position == "fill") {
            p <- p + geom_bar(stat = "identity", position = position, width = width)
        }
    }

    if(true_line == TRUE) {
        new_layer <- sum_rel_abund(rel_abund_tb, !!rlang::sym(x_var))
        p <- p + geom_point(data = new_layer, aes(x = !!rlang::sym(x_var), y = sum ), inherit.aes = FALSE) +
            geom_line(data = new_layer, aes(x = !!rlang::sym(x_var), y = sum, group = 1),
                  inherit.aes = FALSE)
    }
    if(italics == TRUE) {
        p <- p + theme(legend.text = ggtext::element_markdown())
    }

    p + scale_fill_viridis_d(option = "turbo")
}

#' Convert taxon to italics, ignoring threshold, from a relative abundance tibble.
#'
#' @param rel_abund_tb A relative abundance table formatted as a tibble.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#'  rel_abund_phy(physeq) %>%
#'     taxon_italics()
taxon_italics <- function(rel_abund_tb){
   rel_abund_tb %>%
    dplyr::mutate(taxon = dplyr::if_else(
        as.character(taxon) == detect_threshold(rel_abund_tb) | as.character(taxon) == "Unclassified",
        taxon,
        glue::glue("*{taxon}*")))
}


