#' Generate a ggplot2 stacked barplot from a relative abundance table.
#'
#' @param rel_abund_tb A relative abundance table in tibble format.
#' @param x_var The x variable for plotting.
#' @param position "fill" or "stack", should the plotting area be filled in.
#' @param width The width of the bar.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' rel_abund_phy(physeq1) %>% bar_plot()
#' @importFrom ggplot2 ggplot aes geom_bar theme element_text
bar_plot <- function(rel_abund_tb, x_var = "sample_id", position = "stack", width = 1){
    if(missing(rel_abund_tb)){stop("Please provide rel_abund table.")}

    position <- match.arg(position, c("stack", "fill"))

    p <- ggplot(rel_abund_tb, aes(x = !!rlang::sym(x_var), y = rel_abund, fill = taxon))

    position <- match.arg(position, c("stack", "fill"))
    if(position == "stack") {
        p <- p + geom_bar(stat = "identity", position = position, width = width)
    }
    if(position == "fill") {
        p <- p + geom_bar(stat = "identity", position = position, width = width)
    }
    p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
