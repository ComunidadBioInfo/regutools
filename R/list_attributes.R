#' @title List attributes/fields from a dataset/table
#' @description List all attributes and their description of a dataset from
#' RegulonDB. The result of this function may
#' be used as parameter 'values' in [list_attributes()] function.
#' @author Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca,
#' José Alquicira Hernández, Joselyn Chavez
#' @keywords data retrieval, attributes
#' @param regulondb A [regulondb()] object.
#' @param dataset Dataset of interest. The name should correspond to a table of
#' the database.
#' @return A character vector with the field names.
#' @examples
#' ## Connect to the RegulonDB database if necessary
#' if (!exists("regulondb_conn")) regulondb_conn <- connect_database()
#'
#' ## Build the regulon db object
#' e_coli_regulondb <-
#'     regulondb(
#'         database_conn = regulondb_conn,
#'         organism = "E.coli",
#'         database_version = "1",
#'         genome_version = "1"
#'     )
#'
#' ## List the transcription factor attributes
#' list_attributes(e_coli_regulondb, "TF")
#'
#' ## List the operon attributes
#' list_attributes(e_coli_regulondb, "OPERON")
#' @export
#' @importFrom DBI dbListFields

list_attributes <- function(regulondb, dataset) {
    if (missing(dataset)) {
        stop("Parameter 'dataset' is missing, please specify\n")
    }
    stopifnot(validObject(regulondb))
    dbListFields(regulondb, dataset)
}
