#' @importFrom ggplot2 ggplot aes geom_bar theme element_text
#' @export
bar_plot <- function(rel_abund_tab, x_var = "sample_id", position = c("stack", "fill")){

    p <- ggplot(rel_abund_tab, aes(x = !!rlang::sym(x_var), y = rel_abund, fill = taxon))
    position <- match.arg(position)
    if(position == "stack") {
        p <- p + geom_bar(stat = "identity", position = position)
    }
    if(position == "fill") {
        p <- p + geom_bar(stat = "identity", position = position)
    }
    p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
