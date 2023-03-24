#' Connect to the regulondb database
#' @description This function downloads the RegulonDB SQLite database file
#' prior to making a connection to it. It will cache the database file such
#' that subsequent calls will run faster. This function requires an active
#' internet connection.
#' @param ah An `AnnotationHub` object
#' [AnnotationHub-class][AnnotationHub::AnnotationHub-class]. Can be `NULL`
#' if you want to force to use the backup download mechanism.
#' @param bfc A `BiocFileCache` object
#' [BiocFileCache-class][BiocFileCache::BiocFileCache-class]. Used when
#' `ah` is not available.
#' @return An [SQLiteConnection-class][RSQLite::SQLiteConnection-class]
#' connection to the RegulonDB database.
#' @export
#' @importFrom utils download.file
#' @import AnnotationHub
#' @importFrom AnnotationDbi dbFileConnect
#' @importFrom BiocFileCache BiocFileCache bfcrpath
#'
#' @examples
#'
#' ## Connect to the RegulonDB database if necessary
#' if (!exists("regulondb_conn")) regulondb_conn <- connect_database()
#'
#' ## Connect to the database without using AnnotationHub
#' regulondb_conn_noAH <- connect_database(ah = NULL)
connect_database <-
    function(
    ah = AnnotationHub::AnnotationHub(),
    bfc = BiocFileCache::BiocFileCache()) {
        if (!is.null(ah)) {
            ## Check input
            stopifnot(methods::is(ah, "AnnotationHub"))

            ## Query AH
            q <-
                AnnotationHub::query(
                    ah,
                    pattern = c(
                        paste(
                            "RegulonDB SQLite database version v10.8",
                            "for the regutools Bioconductor package"
                        ),
                        "regutools;RegulonDB;v10.8"
                    )
                )
            if (length(q) == 1) {
                ## Return the connection
                return(q[[1]])
            }
        }


        ## Otherwise, use the Dropbox version and cache it with BiocFileCache
        url <-
            paste0(
                "https://www.dropbox.com/s/ufp6wqcv5211v1w/",
                "regulondb_v10.8_sqlite.db?dl=1"
            )
        destfile <- BiocFileCache::bfcrpath(bfc, url)
        AnnotationDbi::dbFileConnect(destfile)
    }
## comentario

