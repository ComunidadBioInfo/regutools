#' @title List attributes/fields from a dataset/table
#' @description List all attributes and their description of a dataset from
#' RegulonDB. The result of this function may
#' be used as the parameters 'values' in [list_attributes()] function.
#' @author Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca,
#' José Alquicira Hernández, Joselyn Chavez
#' @keywords data retrieval, attributes
#' @param regulondb A regulondb object.
#' @param dataset Dataset of interest. The name should correspond to a table of the database.
#' @param description Logical, indicating whether text with the description of the
#' attributes should be included in the output.
#' @return A character vector with the field names. If description is set to 'TRUE', the output will be a data.frame.
#' @examples
#' ## Download the database if necessary
#' if(!file.exists(file.path(tempdir(), 'regulondb_sqlite3.db'))) {
#'     download_database(tempdir())
#' }
#'
#' ## Build the regulon db object
#' e_coli_regulondb <-
#'     regulondb(
#'         database_path = file.path(tempdir(), "regulondb_sqlite3.db"),
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
#'
#' @export
#' @importFrom DBI dbListFields

list_attributes <- function(regulondb, dataset, description=FALSE ){
    if (missing(dataset))
        stop("Parameter 'dataset' is missing, please specify\n")
    stopifnot(validObject(regulondb))
    rs <- dbListFields(regulondb, dataset)
    if( description ){
        attributeDescription <- get_dataset(
            regulondb = regulondb,
            dataset = "REGULONDB_OBJECTS" )
        attributeDescription <- attributeDescription[attributeDescription$table_name == dataset,]
        rs <- merge( data.frame(attribute=rs), as.data.frame(attributeDescription ) )
        rs <- rs[,c("attribute", "description")]
        rs <- as.data.frame(rs)
    }
    rs
}
