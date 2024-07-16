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
#' @importFrom ggplot2 ggplot aes geom_bar geom_point geom_line theme element_text
bar_plot <- function(rel_abund_tb, x_var = "sample_id", position = "stack", width = 1, color = NULL, true_line = FALSE){
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

    p
}


