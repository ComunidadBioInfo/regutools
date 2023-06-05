#' @title Generation of a bar chart
#' @description This function uses the ggplot2 library to generate
#' a bar chart based on data from a data frame.
#' Lets you specify the columns for the x and y axes,
#' as well as axis and plot titles.
#' @author Daniel Pina Acosta, Sergio Valle Modragon
#' @param df The dataframe containing the data for the plot.
#' @param column_x The name of the column to be used on the x-axis.
#' @param column_y The name of the column to be used on the y-axis.
#' @param x_axis_title The title for the x-axis.
#' @param y_axis_title The title for the y-axis.
#' @param plot_title The title for the plot.
#' 
#' @return A bar chart generated using ggplot2.
#'
#' @examples
#' # Example 1: Generates a bar chart of the number of genes by operons
#' generate_graphic_bar_by_promoters <- generate_graphic_bar(get_general_frequency_promoters,cantidad,n_operon,xlab="promoters by operon",ylab="operon frequency",title="Promoters by operon")
#' 
#' @import ggplot2
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @export
generate_graphic_bar <- function(df, column_x, column_y, x_axis_title, y_axis_title, plot_title) {
  max_value <- max(df[[deparse(substitute(column_x))]])  # Get the maximum value of the column
  max_value <- max_value+1 
  graph <- (ggplot(df, aes(x = {{column_x}}, y = {{column_y}}), col = "blue") +
              geom_bar(stat = "identity", color = "blue") +
              ggtitle(plot_title) +
              xlab(x_axis_title) +
              ylab(y_axis_title) +
              scale_x_continuous(limits = c(0, max_value), breaks = seq(1, max_value, by = 1)) +
              theme_classic() +
              theme(
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 15),
                plot.title = element_text(hjust = 0.5, size = 20),
                legend.position = "none",
                text = element_text(size = 13, family = "Times New Roman")
              ))
  print(graph)
}