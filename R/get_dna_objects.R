#' Retrieve genomic elements from regulonDB
#'
#' @param regulondb A [regulondb()] object.
#' @param genome A valid UCSC genome name.
#' @param grange A [GenomicRanges::GRanges-class()] object indicating position
#'  left and right.
#' @param elements A character vector specifying which annotation elements to
#' plot. It can be any from: `"-10 promoter box"`, `"-35 promoter box"`,
#' `"gene"`, `"promoter"`, `"Regulatory Interaction"`, `"sRNA interaction"`,
#' or `"terminator"`.
#' @author Joselyn Chavez
#' @return [GenomicRanges::GRanges-class()] object with the elements found.
#' @importFrom Gviz GeneRegionTrack plotTracks GenomeAxisTrack AnnotationTrack
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @examples
#' ## Connect to the RegulonDB database if necessary
#' if (!exists("regulondb_conn")) {
#'     regulondb_conn <- connect_database()
#' }
#'
#' ## Build the regulondb object
#' e_coli_regulondb <-
#'     regulondb(
#'         database_conn = regulondb_conn,
#'         organism = "chr",
#'         database_version = "1",
#'         genome_version = "1"
#'     )
#'
#' ## Get all genes from E. coli
#' get_dna_objects(e_coli_regulondb)
#'
#' ## Get genes providing Genomic Ranges
#' grange <- GenomicRanges::GRanges(
#'     "chr",
#'     IRanges::IRanges(5000, 10000)
#' )
#' get_dna_objects(e_coli_regulondb, grange)
#'
#' ## Get aditional elements within genomic positions
#' get_dna_objects(e_coli_regulondb,
#'     grange,
#'     elements = c("gene", "promoter")
#' )
#' @export
get_dna_objects <-
    function(
      regulondb,
      genome = "eschColi_K12",
      grange = GRanges("chr", IRanges(1, 5000)),
      elements = "gene"
    ) {
        valid_elements <- c(
            "-10 promoter box",
            "-35 promoter box",
            "gene",
            "promoter",
            "Regulatory Interaction",
            "sRNA interaction",
            "terminator"
        )
        # Validate elements
        if (!all(elements %in% valid_elements)) {
            non.valid.elements.index <- elements %in% valid_elements
            non.valid.elements <- elements[!non.valid.elements.index]
            stop(
                "Element(s) ",
                paste0('"', paste(non.valid.elements, collapse = ", "), '"'),
                paste(
                    " are not valid. Please provide any or all of these",
                    "valid elements: "
                ),
                paste0('"', paste(valid_elements, collapse = ", "), '"'),
                call. = FALSE
            )
        }
        # search for dna_objects ("-10 promoter box", -35 promoter box", "gene",
        # "promoter", "Regulatory Interaction","sRNA interaction","terminator")
        dna_objects <- regutools::get_dataset(
            regulondb,
            dataset = "DNA_OBJECTS",
            filters = list(
                posright = c(
                    grange@ranges@start,
                    grange@ranges@start +
                        grange@ranges@width
                ),
                type = elements
            ),
            interval = "posright",
            output_format = "GRanges"
        )

        # Return GRanges result
        dna_objects
    }
