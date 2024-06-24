#' @importFrom ggplot2 ggplot aes geom_bar theme element_text
#' @export
bar_plot <- function(rel_abund_tab, x_var, position, width){
    if(missing(rel_abund_tab)){stop("Please provide rel_abund table.")}
    if(missing(x_var)){x_var = "sample_id"}
    if(missing(position)) {position = "stack"}
    if(missing(width)){width = 1}

    p <- ggplot(rel_abund_tab, aes(x = !!rlang::sym(x_var), y = rel_abund, fill = taxon))

    position <- match.arg(position, c("stack", "fill"))
    if(position == "stack") {
        p <- p + geom_bar(stat = "identity", position = position, width = width)
    }
    if(position == "fill") {
        p <- p + geom_bar(stat = "identity", position = position, width = width)
    }
    p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
