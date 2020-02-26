#' Plot genomic elements from regulonDB
#'
#' @param regulondb A [regulondb()] object.
#' @param genome A valid UCSC genome name.
#' @param from A `integer(1)` specifying the left position.
#' @param to A `integer(1)` specifying the right position.
#' @param elements A character vector specifying which annotation elements to
#' plot. It can be any from: `"-10 promoter box"`, `"-35 promoter box"`,
#' `"gene"`, `"promoter"`, `"Regulatory Interaction"`, `"sRNA interaction"`,
#' or `"terminator"`.
#'
#' @author Joselyn Chavez
#' @return [GenomicRanges::GRanges-class()] object with the elements found.
#' @importFrom Gviz GeneRegionTrack plotTracks GenomeAxisTrack AnnotationTrack
#' @examples
#' ## Connect to the RegulonDB database if necessary
#' if(!exists('regulondb_conn')) regulondb_conn <- connect_database()
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
#' get_dna_objects(e_coli_regulondb, from = 5000, to = 10000)
#'
#' ## Get aditional elements within genomic positions
#' get_dna_objects(e_coli_regulondb, from = 5000, to = 10000, elements = c("gene", "promoter"))
#' @export
get_dna_objects <-
    function(regulondb,
             genome = "eschColi_K12",
             from = 0,
             to = 4641628,
             elements = "gene") {
        # validate ranges
        if (!is.numeric(from) || !is.numeric(to)) {
            stop("Parameter 'from' and 'to' must be a number", call. = FALSE)
        }

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
                " are not valid. Please provide any or all of these valid elements: ",
                paste0('"', paste(valid_elements, collapse = ", "), '"'),
                call. = FALSE
            )
        }
        # search for dna_objects ("-10 promoter box", -35 promoter box", "gene", "promoter", "Regulatory Interaction","sRNA interaction","terminator")
        dna_objects <- regutools::get_dataset(
            regulondb,
            dataset = "DNA_OBJECTS",
            filters = list(posright = c(from, to),
                           type = elements),
            interval = "posright",
            output_format = "GRanges"
        )

        # Return GRanges result
        dna_objects
    }