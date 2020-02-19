#' @title Get the binding sites for a Transcription Factor (TF)
#' @description Retrieve the binding sites and genome location for a given
#' transcription factor.
#' @author José Alquicira Hernández, Jacques van Helden, Joselyn Chávez
#' @param regulondb A regulondb object.
#' @param transcription_factor name of the transcription factor.
#' @param output_format The output object. Can be either a `GRanges` (default) or `Biostrings`.
#' @return Either a GRanges object or a Biostrings object summarizing information
#' about the binding sites of the transcription factors.
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
#' ## Get the binding sites for AraC
#' get_binding_sites(e_coli_regulondb, transcription_factor = "AraC")
#'
#'
#' @export
get_binding_sites <- function( regulondb, transcription_factor, output_format = "GRanges" ){
    if(!output_format %in% c("GRanges", "Biostrings")){
        stop("The parameter 'output_format' must be either 'GRanges' or 'Biostrings'")
    }
    stopifnot(validObject(regulondb))
    tfbs_raw <- get_dataset(
        regulondb = regulondb,
        dataset = "TF",
        attributes = c("name", "tfbs_unique"),
        filters = list(name = transcription_factor) )
    if( nrow( tfbs_raw ) == 0 ){
        return(NULL)
    }
    tfbs_table <-
        as.data.frame(
            strsplit( x = tfbs_raw$tfbs_unique, split = " " ),
            col.names = "res")
    tfbs_table <- strsplit(
        as.character(tfbs_table$res),
        split =  "\t" )
    tfbs_table <- as.data.frame(do.call( rbind, tfbs_table ) )
    colnames(tfbs_table) <-
        c("ID", "left", "right", "strand", "sequence")
    tfbs_table$left <- as.numeric( as.character( tfbs_table$left ) )
    tfbs_table$right <- as.numeric( as.character( tfbs_table$right ) )
    tfbs_table$strand <- ifelse( tfbs_table$strand == "forward", "+", "-" )
    tfbs_table$strand[is.na(tfbs_table$strand)] <- "*"
    if( output_format == "GRanges" ){
        res <- with( tfbs_table,
              GRanges( tfbs_raw@organism,
                       IRanges( start=left, end=right ),
                       strand=strand ) )
        names(res) <- as.character(tfbs_table$ID)
        mcols(res)$sequence <- as.character(tfbs_table$sequence)
        res
    }else{
        res <- DNAStringSet( as.character(tfbs_table$sequence) )
        names( res ) <- as.character( tfbs_table$ID )
        mcols( res )$granges <- with( tfbs_table,
                                      GRanges( tfbs_raw@organism,
                                               IRanges( start=left, end=right ),
                                               strand=strand ) )
        res
    }
}
