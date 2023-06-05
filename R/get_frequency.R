#' @title Obtains the frequency of repeated values in a column.
#' @description will get the frequency with which each element of the column is repeated
#' given as the second parameter of the function.
#' @author Pina Acosta Daniel, Valle Mondragon Sergio.
#' @param df The dataset being worked with.
#' @param column_name Column of the dataframe by which the data grouping will be created
#' to get the frequency
#' @param column_total Name of the column where the repeated data will be summed.
#' @return Dataset with the frequency of the data from the second parameter (column_name).
#' @examples
#' # Example 1: Obtain the frequency of the number of transcriptionUnits by operon
#' frecuency_tus <- make_frecuency(df = tus_by_operon, column_name = cantidad, column_total = n_operon)
#' # Example 2:
#' frequency_by_genes <- make_frequency(total_genes_by_tf, cantidad, n_tf)
#' @import dplyr
#' @export
make_frequency <- function(df, column_name, column_total){
  df %>%
    group_by({{column_name}}) %>%
    summarise({{column_total}}:= n())
}

