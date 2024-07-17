#' Generate a ggplot2 stacked barplot from a relative abundance table.
#'
#' @param rel_abund_tb A relative abundance table in tibble format.
#' @param x_var The x variable for plotting.
#' @param position "fill" or "stack", should the plotting area be filled in.
#' @param width The width of the bar.
#' @param color A variable to color bars by.
#' @param true_line Logical, whether to show a true abundance line.
#' @param italics Logical, whether to italicize taxon.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' rel_abund_phy(physeq) %>% bar_plot()
#' @importFrom ggplot2 ggplot aes geom_bar geom_point geom_line theme element_text theme scale_y_continuous element_blank
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

    p + scale_y_continuous(expand = c(0, 0)) +
        ggplot2::scale_fill_viridis_d(option = "turbo") +
        theme(panel.grid = element_blank(),
              panel.border = element_blank())
}

#' Generate a ggplot2 bubble plot
#'
#' @param rel_abund_tb A relative abundance table in tibble format.
#' @param x_var The variable to used on the x-axis.
#' @param color The color.
#' @param italics Logical. Whether to use italics or not.
#'
#' @return A ggplot
#' @export
#'
#' @examples
#' rel_abund_phy(physeq) %>% bubble_plot()
bubble_plot <- function(rel_abund_tb, x_var = "sample_id", color = NULL, italics = FALSE){
    if(missing(rel_abund_tb)){stop("Please provide rel_abund table.")}

    p <- ggplot(rel_abund_tb, aes(x = !!rlang::sym(x_var), y = taxon, size = rel_abund))

    if (!is.null(color)){
        p <- p + geom_point(aes(color = !!rlang::sym(color)))

    } else {
        p <- p + geom_point()
    }

    if(italics == TRUE) {
        p <- p + theme(axis.text.y = ggtext::element_markdown())
    }

    p + theme(panel.grid = element_blank(),
             panel.border = element_blank())
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


