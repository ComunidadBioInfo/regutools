#' @title It encompasses the count function and make_frequency to obtain the frequency..
#' @description Both the "count_elements" function and the "get_frequency" function are
#' encompassed to obtain a more straightforward dataset, allowing that
#' dataset to be passed as a parameter to the bar and dispersion function.
#' @author Pina Acosta Daniel, Valle Mondragon Sergio.
#' @param df The dataset being worked with.
#' @param column_name Column name from which the rows are to be counted.
#' @param column_name2 Column of the dataframe by which the data grouping will be created
#' to get the frequency
#' @param column_total Name of the column where the repeated data will be summed.
#' @return Dataset with the frequency of the data from the second parameter (column_name).
#' @examples
#' # Example 1: Obtain the frequency of the number of transcriptionUnits by operon
#' frequency_tus <- get_general_frequency(operons_tus, data.getAllOperon.data.operon$name,cantidad,n_operon)
#' # Example 2: Obtain the frequency of the number of genes by tf
#'  frecuency_genes <- get_general_frequency(get_genes_by_tf,data.getAllRegulon.data.transcriptionFactor$name,cantidad,n_tf)
#' @export
get_general_frequency <- function(df, column_name,column_name2,column_total){
  count <- count_elements({{df}}, {{column_name}})
  frequency <- get_frequency(count,{{column_name2}},{{column_total}})
}
