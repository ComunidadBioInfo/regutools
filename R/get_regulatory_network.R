#' @title Return complete regulatory network.
#' @description This function retrieves all the regulation networks in
#' regulonDB between TF-TF, GENE-GENE or TF-GENE depending on the parameter
#' 'type'.
#' @param regulondb A [regulondb()] object.
#' @param regulator Name of TF or gene that acts as regulator. If `NULL`, the
#' function retrieves all existent networks in the regulonDB.
#' @param type "TF-GENE", "TF-TF", "GENE-GENE"
#' @param cytograph If TRUE, displays network in Cytoscape. This option
#' requires previous instalation and launch of Cytoscape.
#'
#' @keywords regulation retrieval, TF, networks,
#' @author Carmina Barberena Jonas, Jesús Emiliano Sotelo Fonseca,
#' José Alquicira Hernández, Joselyn Chávez
#' @return A [regulondb_result][regutools::regulondb_result-class] object.
#' @examples
#'
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
#' ## Retrieve regulation of 'araC'
#' get_regulatory_network(e_coli_regulondb,
#'     regulator = "AraC",
#'     type = "TF-GENE"
#' )
#'
#' ## Retrieve all GENE-GENE networks
#' get_regulatory_network(e_coli_regulondb, type = "GENE-GENE")
#'
#' ## Retrieve TF-GENE network of AraC and display in Cytoscape
#' ## Note that Cytospace needs to be open for this to work
#' cytoscape_present <- try(RCy3::cytoscapePing(), silent = TRUE)
#' if (!is(cytoscape_present, "try-error")) {
#'     get_regulatory_network(
#'         e_coli_regulondb,
#'         regulator = "AraC",
#'         type = "TF-GENE",
#'         cytograph = TRUE
#'     )
#' }
#' @export
#' @importFrom RCy3 cytoscapePing createNetworkFromDataFrames
#' setEdgeColorMapping setVisualStyle

get_regulatory_network <-
    function(
      regulondb,
      regulator = NULL,
      type = "TF-GENE",
      cytograph = FALSE
    ) {
        # Check type parameter
        if (!type %in% c("GENE-GENE", "TF-GENE", "TF-TF")) {
            stop("Parameter 'type' must be TF-GENE, TF-TF, or GENE-GENE.",
                call. = FALSE
            )
        }

        # Get Network
        if (!is.null(regulator)) {
            network <-
                as.data.frame(get_dataset(
                    regulondb,
                    attributes = c(
                        "regulator_name", "regulated_name",
                        "effect"
                    ),
                    filters = list(
                        "network_type" = type,
                        "regulator_name" = regulator
                    ),
                    dataset = "NETWORK"
                ))
        } else {
            network <-
                as.data.frame(get_dataset(
                    regulondb,
                    attributes = c(
                        "regulator_name", "regulated_name",
                        "effect"
                    ),
                    filters = list("network_type" = type),
                    dataset = "NETWORK"
                ))
        }

        if (cytograph) {
            # CytoScape connection
            tryCatch(
                cytoscapePing(),
                error = function(e) {
                    stop(
                        paste(
                            "To use integration with Cytoscape,",
                            "please launch Cytoscape before running",
                            "get_regulatory_network()"
                        ),
                        call. = FALSE
                    )
                }
            )
            colnames(network) <-
                c("source", "target", "interaction")
            my_new_network <-
                createNetworkFromDataFrames(edges = network)
            # set visual parameters
            RCy3::setNodeShapeDefault(new.shape = "ELLIPSE")
            RCy3::setNodeSizeDefault(35)
            RCy3::setNodeColorDefault(new.color = "#ACE7F9")
            RCy3::setEdgeLabelMapping(table.column = "interaction")
            RCy3::setEdgeColorMapping(
                table.column = "interaction",
                table.column.values = c("activator", "repressor", "dual"),
                colors = c("#339900", "#CC3300", "#0033CC"),
                mapping.type = "d"
            )
        } else {
            colnames(network) <- c("regulator", "regulated", "effect")

            # Change effect to +, - and +/-
            network$effect <-
                sub(
                    pattern = "activator",
                    replacement = "+",
                    x = network$effect
                )
            network$effect <-
                sub(
                    pattern = "repressor",
                    replacement = "-",
                    x = network$effect
                )
            network$effect <-
                sub(
                    pattern = "dual",
                    replacement = "+/-",
                    x = network$effect
                )

            network <-
                dataframe_to_dbresult(network, regulondb, "NETWORK")

            return(network)
        }
    }
