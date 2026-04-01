#' @title Get TFs or genes that regulate the genes of interest
#' @description Given a list of genes (name, bnumber or GI),
#' get all transcription factors or genes that regulate them.
#' The effect of regulators over the gene of interest can be positive (+),
#' negative (-) or dual (+/-)
#' @param regulondb A regulondb class.
#' @param genes Vector of genes (name, bnumber or GI).
#' @param format Output format: multirow, onerow, table
#' @param output.type How regulators will be represented: "TF"/"GENE"
#' @keywords regulation retrieval, TFs, networks,
#' @author Carmina Barberena Jonas, Jesús Emiliano Sotelo Fonseca,
#' José Alquicira Hernández, Joselyn Chávez
#' @return A [regulondb_result][regutools::regulondb_result-class] object.
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
#' ## Get Transcription factors that regulate araC in one row
#' get_gene_regulators(
#'     e_coli_regulondb,
#'     genes = c("araC"),
#'     output.type = "TF",
#'     format = "onerow"
#' )
#'
#' ## Get genes that regulate araC in table format
#' get_gene_regulators(
#'     e_coli_regulondb,
#'     genes = c("araC"),
#'     output.type = "GENE",
#'     format = "table"
#' )
#' @export

get_gene_regulators <-
    function(
      regulondb,
      genes,
      format = "multirow",
      output.type = "TF"
    ) {
        stopifnot(validObject(regulondb))
        # Check genes parameter class
        ##        if (!class(genes) %in% c("vector", "list", "character")) {
        if (!(is(genes, "vector") | is(genes, "list") | is(genes, "character")
        )) {
            stop("Parameter 'genes' must be a character vector or list.",
                call. = FALSE
            )
        }
        # Check format parameter
        if (!format %in% c("multirow", "onerow", "table")) {
            stop("Parameter 'format' must be multirow, onerow, or table.",
                call. = FALSE
            )
        }
        # Check output.type
        if (!output.type %in% c("TF", "GENE")) {
            stop("Parameter 'output.type' must be either 'TF' or 'GENE'",
                call. = FALSE
            )
        }

        # Convert GIs to gene names
        # Assign id per gene
        gene_guesses <- vapply(genes, guess_id,
            regulondb = regulondb,
            FUN.VALUE = character(1)
        )

        # Check that guesses are names

        if (!all(gene_guesses == "name")) {
            split_ids <- split(x = genes, f = gene_guesses)

            # Get synonyms for each group
            names_list <- lapply(names(split_ids), function(id) {
                get_gene_synonyms(regulondb,
                    genes = split_ids[[id]],
                    from = id,
                    to = "name"
                )[["name"]]
            })
            # Cat id list
            genes <- unlist(names_list)
            names(genes) <- NULL
        }

        # Checks for type of network
        if (output.type == "TF") {
            network.type <- "TF-GENE"
        } else if (output.type == "GENE") {
            network.type <- "GENE-GENE"
        }

        # Retrieve data from NETWORK table
        regulation <-
            as.data.frame(get_dataset(
                regulondb,
                attributes = c("regulated_name", "regulator_name", "effect"),
                filters = list(
                    "regulated_name" = genes,
                    "network_type" = network.type
                ),
                dataset = "NETWORK"
            ))
        colnames(regulation) <- c("genes", "regulators", "effect")

        # Change effect to +, - and +/-
        regulation$effect <-
            sub(
                pattern = "activator",
                replacement = "+",
                x = regulation$effect
            )
        regulation$effect <-
            sub(
                pattern = "repressor",
                replacement = "-",
                x = regulation$effect
            )
        regulation$effect <-
            sub(
                pattern = "dual",
                replacement = "+/-",
                x = regulation$effect
            )

        # Format output
        # Multirow
        if (format == "multirow") {
            # Add internal attribute "format" to use in GetSummary function.
            regulation <-
                dataframe_to_dbresult(regulation, regulondb, "NETWORK")
            metadata(regulation)$format <- format
            return(regulation)

            # Onerow
        } else if (format == "onerow") {
            regulation <- lapply(as.list(genes), function(x) {
                genereg <- regulation[regulation[, "genes"] == x, ]
                genereg <-
                    paste(
                        paste(
                            genereg$regulators,
                            genereg$effect,
                            sep = "(",
                            collapse = "), "
                        ),
                        ")",
                        sep = ""
                    )
            })
            regulation <- unlist(regulation)
            genes <- unlist(genes)
            regulation <- data.frame(genes, regulation)
            colnames(regulation) <- c("genes", "regulators")

            # Add internal attribute "format" to use in GetSummary function.
            regulation <-
                dataframe_to_dbresult(regulation, regulondb, "NETWORK")
            metadata(regulation)$format <- format
            return(regulation)

            # Table
        } else if (format == "table") {
            # Empty dataframe
            rtable <-
                data.frame(matrix(nrow = length(genes), ncol = length(c(
                    unique(regulation$regulators)
                ))))
            colnames(rtable) <- unique(regulation$regulators)
            rownames(rtable) <- genes

            # Fill dataframe with regulation
            for (i in seq_len(dim(regulation)[1])) {
                rtable[regulation[i, 1], regulation[i, 2]] <- regulation[i, 3]
            }
            regulation <- rtable

            # Add internal attribute "format" to use in GetSummary function.
            regulation <-
                dataframe_to_dbresult(regulation, regulondb, "NETWORK")
            metadata(regulation)$format <- format
            return(regulation)
        }
    }
