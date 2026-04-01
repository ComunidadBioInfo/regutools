#' @title The regulondb class
#' @description The regulondb class is an extension of the SQLiteConnection,
#' which as the name suggests, consists of an SQLite connection to a database
#' with the table design of the
#' RegulonDb database. In addition to the slots defined in the
#' SQLiteConnection object,
#' the regulondb class also contains additional slots to store
#' information about database versions, organism information and genome build
#' versions.
#' @slot organism A character vector with the name of the organism of the
#' database.
#' @slot genome_version A character vector with the version of the genome
#' build.
#' @slot database_version A character vector with the version of regulondb
#' build.
#' @export
#' @import RSQLite
#' @importFrom methods is new validObject
setClass(
    "regulondb",
    contains = "SQLiteConnection",
    slots = list(
        organism = "character",
        genome_version = "character",
        database_version = "character"
    )
)

setValidity("regulondb", function(object) {
    table_must <- c(
        "DNA_OBJECTS",
        "GENE",
        "NETWORK",
        "OPERON",
        "PROMOTER",
        "REGULONDB_OBJECTS",
        "TF",
        "TU"
    )
    if (!all(table_must %in% dbListTables(object))) {
        table_must <- table_must[!table_must %in% dbListTables(object)]
        flg <- length(table_must) > 1
        sprintf(
            "The following table%s %s missing from the database: %s",
            ifelse(flg, "s", ""),
            ifelse(flg, "are", "is"),
            ifelse(flg, paste(table_must, collapse = ", "), table_must)
        )
    } else if (!is(object@organism, "character")) {
        "The slot 'organism' name must be a character vector"
    } else if (!is(object@database_version, "character")) {
        "The slot 'database_version' must be a character vector"
    } else if (!is(object@genome_version, "character")) {
        "The slot 'genome_version' name must be a character vector"
    } else {
        TRUE
    }
})

#' Constructor function of a regulondb class
#' @aliases build_regulondb
#' @description The build_regulondb function is a constructor function of a
#' regulondb class.
#' @param database_conn A
#' [SQLiteConnection-class][RSQLite::SQLiteConnection-class] connection to
#' the RegulonDB database made with [connect_database()].
#' @param organism A character vector with the name of the organism of the
#' database.
#' @param genome_version A character vector with the version of the genome
#' build.
#' @param database_version A character vector with the version of regulondb
#' build.
#' @return A [regulondb][regutools::regulondb] object.
#' @examples
#'
#' ## Connect to the RegulonDB database if necessary
#' if (!exists("regulondb_conn")) regulondb_conn <- connect_database()
#'
#' ## Build a regulondb object
#' e_coli_regulondb <-
#'     regulondb(
#'         database_conn = regulondb_conn,
#'         organism = "E.coli",
#'         database_version = "1",
#'         genome_version = "1"
#'     )
#' @export
regulondb <-
    function(
      database_conn,
      organism,
      genome_version,
      database_version
    ) {
        stopifnot(is(database_conn, "SQLiteConnection"))
        stopifnot(is(organism, "character"))
        stopifnot(is(genome_version, "character"))
        stopifnot(is(database_version, "character"))
        new(
            "regulondb",
            database_conn,
            organism = organism,
            genome_version = genome_version,
            database_version = database_version
        )
    }

#' @title The regulondb_results class
#' @description The regulondb class is an extension of the DataFrame class,
#' with
#' additional slots that host information of the database used to obtain these
#' results.
#' @slot organism A character string with the name of the organism of the
#' database.
#' @slot genome_version A character string with the version of the genome
#' build.
#' @slot database_version A character string with the version of regulondb
#' build.
#' @slot dataset A character string with the name of the table used for the
#' query in get_dataset().
#' @importClassesFrom S4Vectors DFrame
#' @export
setClass(
    "regulondb_result",
    contains = "DFrame",
    slots = list(
        organism = "character",
        genome_version = "character",
        database_version = "character",
        dataset = "character"
    )
)

setValidity("regulondb_result", function(object) {
    if (!is(object@organism, "character")) {
        "The slot 'organism' name must be a character vector"
    } else if (!is(object@database_version, "character")) {
        "The slot 'database_version' must be a character vector"
    } else if (!is(object@genome_version, "character")) {
        "The slot 'genome_version' name must be a character vector"
    } else if (!is(object@dataset, "character")) {
        "The slot 'dataset' name must be a character vector"
    } else {
        TRUE
    }
})
