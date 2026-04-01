#' Plot annotation elements within genomic region
#'
#' @param regulondb A [regulondb()] object.
#' @param genome A valid UCSC genome name.
#' @param grange A [GenomicRanges::GRanges-class()] object indicating position
#' left and right.
#' @param elements A character vector specifying which annotation elements to
#' plot. It can be any from: `"-10 promoter box"`, `"-35 promoter box"`,
#' `"gene"`, `"promoter"`, `"Regulatory Interaction"`, `"sRNA interaction"`,
#' or `"terminator"`.
#'
#' @author Joselyn Chavez
#' @return A plot with genomic elements found within a genome region,
#' including genes and regulators.
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
#' ## Plot some genes from E. coli using default parameters
#' plot_dna_objects(e_coli_regulondb)
#'
#' ## Plot genes providing Genomic Ranges
#' grange <- GenomicRanges::GRanges(
#'     "chr",
#'     IRanges::IRanges(5000, 10000)
#' )
#' plot_dna_objects(e_coli_regulondb, grange)
#'
#' ## Plot aditional elements within genomic positions
#' plot_dna_objects(e_coli_regulondb,
#'     grange,
#'     elements = c("gene", "promoter")
#' )
#' @export
plot_dna_objects <-
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
        # search for dna_objects ("-10 promoter box", -35 promoter box",
        # "gene", "promoter", "Regulatory Interaction","sRNA interaction",
        # "terminator")
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

        # set dna_object 'gene' as first element
        dna_objects_type <- unique(sort(dna_objects$type))
        if (grep("gene", dna_objects_type) > 0) {
            dna_objects_type <-
                c("gene", dna_objects_type[!dna_objects_type == "gene"])
        }

        # build tracks
        build_track <- function(x) {
            # select data with type == element
            object_filtered <- dna_objects[dna_objects$type == x]
            # build track
            Gviz::AnnotationTrack(object_filtered,
                genome,
                chromosome = "chr",
                name = x,
                options(ucscChromosomeNames = TRUE),
                transcriptAnnotation = "name",
                background.panel = "#FFFEDB",
                background.title = "brown",
                fontsize = 10
            )
        }

        list_dna_annotation <- lapply(dna_objects_type, build_track)

        # plot
        Gviz::plotTracks(c(
            Gviz::GenomeAxisTrack(),
            list_dna_annotation
        ))
    }
