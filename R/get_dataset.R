#' @title Extract data from RegulonDB
#' @description This function retrieves data from RegulonDB. Attributes from
#' datasets can be selected and filtered.
#' @param regulondb A [regulondb()] object.
#' @param dataset Dataset of interest. Use the function list_datasets for an
#' overview of valid datasets.
#' @param attributes Vector of attributes to be retrieved.
#' @param filters List of filters to be used. The names should correspond to
#' the attribute and the values correspond to the condition for selection.
#' @param and Logical argument. If FALSE, filters will be considered under the
#' "OR" operator
#' @param partialmatch name of the condition(s) with a string pattern for full
#' or partial match in the query
#' @param interval the filters whose values will be considered as interval
#' @param output_format A string specifying the output format. Possible options
#' are "regulondb_result", "GRanges", "DNAStringSet" or "BStringSet".
#' @author Carmina Barberena Jonas, Jesús Emiliano Sotelo Fonseca,
#' José Alquicira Hernández, Joselyn Chávez
#' @return By default, a regulon_results object. If specified in the parameter
#' output_format, it can also return either a GRanges object or a Biostrings
#' object.
#' @importFrom IRanges IRanges
#' @importFrom GenomicRanges strand mcols "strand<-" "mcols<-"
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
#' ## Obtain all the information from the "GENE" dataset
#' get_dataset(e_coli_regulondb, dataset = "GENE")
#'
#' ## Get the attributes posright and name from the "GENE" dataset
#' get_dataset(e_coli_regulondb,
#'     dataset = "GENE",
#'     attributes = c("posright", "name")
#' )
#'
#' ## From "GENE" dataset, get the gene name, strand, posright, product name
#' ## and id of all genes regulated with name like "ara", strand as "forward"
#' ## with a position right between 2000 and 40000
#' get_dataset(
#'     e_coli_regulondb,
#'     dataset = "GENE",
#'     attributes = c("name", "strand", "posright", "product_name", "id"),
#'     filters = list(
#'         name = c("ara"),
#'         strand = c("forward"),
#'         posright = c("2000", "40000")
#'     ),
#'     and = TRUE,
#'     partialmatch = "name",
#'     interval = "posright"
#' )
#' @export
#' @importFrom S4Vectors DataFrame
#' @importFrom Biostrings DNAStringSet BStringSet
get_dataset <-
    function(
      regulondb,
      dataset = NULL,
      attributes = NULL,
      filters = NULL,
      and = TRUE,
      interval = NULL,
      partialmatch = NULL,
      output_format = "regulondb_result"
    ) {
        # Check if format specification is valid
        if (!output_format %in% c(
            "regulondb_result",
            "GRanges",
            "DNAStringSet",
            "BStringSet"
        )) {
            stop(
                paste(
                    "Output format must be one of the following:",
                    "regulondb_result, GRanges, DNAStringSet or BStringSet"
                ),
                call. = FALSE
            )
        }

        # Validate if attributes is a vector
        if (!is.null(attributes) & (!is.vector(attributes))) {
            stop("Parameter 'attributes' must be a vector", call. = FALSE)
        }

        # Validate dataset
        if (is.null(dataset)) {
            stop("Non dataset provided", call. = FALSE)
        }
        if (!all(dataset %in% list_datasets(regulondb))) {
            stop("Invalid dataset. See valid datasets in list_datasets()",
                call. = FALSE
            )
        }

        # Validate attributes
        if (!all(attributes %in% list_attributes(regulondb, dataset))) {
            non.existing.attrs.index <-
                attributes %in% list_attributes(regulondb, dataset)
            non.existing.attrs <-
                attributes[!non.existing.attrs.index]
            stop(
                "Attribute(s) ",
                paste0('"', paste(non.existing.attrs, collapse = ", "), '"'),
                " do not exist. See valid attributes in list_attributes()",
                call. = FALSE
            )
        }

        # Validate partialmatch

        if (!all(partialmatch %in% list_attributes(regulondb, dataset))) {
            non.existing.attrs.index <-
                partialmatch %in% list_attributes(regulondb, dataset)
            non.existing.attrs <-
                partialmatch[!non.existing.attrs.index]
            stop("Partialmatch ",
                paste0('"', paste(non.existing.attrs, collapse = ", "), '"'),
                " do not exist.",
                call. = FALSE
            )
        }

        if (!all(partialmatch %in% names(filters))) {
            non.existing.attrs.index <- partialmatch %in% names(filters)
            non.existing.attrs <-
                partialmatch[!non.existing.attrs.index]
            stop("Partialmatch ",
                paste0('"', paste(non.existing.attrs, collapse = ", "), '"'),
                " not defined in 'filters' ",
                call. = FALSE
            )
        }

        # Sets logical operator
        if (and) {
            operator <- "AND"
        } else {
            operator <- "OR"
        }

        if (is.null(filters) & is.null(attributes)) {
            query <- paste0("SELECT * FROM ", dataset, ";")
        } else if (is.null(attributes) & !is.null(filters)) {
            cond <-
                build_condition(
                    regulondb,
                    dataset,
                    filters,
                    operator,
                    interval,
                    partialmatch
                )
            query <-
                paste0("SELECT * FROM ", dataset, " WHERE ", cond, ";")
        } else if (!is.null(attributes) & is.null(filters)) {
            query <-
                paste0(
                    "SELECT ",
                    paste(attributes, collapse = " , "),
                    " FROM ",
                    dataset,
                    ";"
                )
        } else {
            cond <-
                build_condition(
                    regulondb,
                    dataset,
                    filters,
                    operator,
                    interval,
                    partialmatch
                )
            query <-
                paste(
                    "SELECT ",
                    paste(attributes, collapse = " , "),
                    "FROM ",
                    dataset,
                    " WHERE ",
                    cond,
                    ";"
                ) # Construct query
        }
        result <- dbGetQuery(regulondb, query)

        # Check if results exist
        if (!nrow(result)) {
            warning("Your query returned no results.")
        }

        result <- new(
            "regulondb_result",
            DataFrame(result),
            organism = regulondb@organism,
            genome_version = regulondb@genome_version,
            database_version = regulondb@database_version,
            dataset = dataset
        )

        if (output_format == "GRanges") {
            result <- convert_to_granges(result)
        } else if (output_format == "DNAStringSet") {
            result <- convert_to_biostrings(result, seq_type = "DNA")
        } else if (output_format == "BStringSet") {
            result <- convert_to_biostrings(result, seq_type = "product")
        }

        result
    }

#' @title Function to convert output of regulondb queries to GenomicRanges objects
#' @description This function converts, when possible, a regulon_result object into a GRanges object.
#' @param regulondb_result A regulon_result object.
#' @author Alejandro Reyes
#' @importFrom GenomicRanges GRanges
#' @importFrom S4Vectors metadata "metadata<-"
#' @return A [GRanges][GenomicRanges::GRanges-class] object.
#' @export
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
#' ## Obtain all the information from the "GENE" dataset
#' convert_to_granges(get_dataset(e_coli_regulondb, dataset = "GENE"))
convert_to_granges <- function(regulondb_result) {
    if (!is(regulondb_result, "regulondb_result")) {
        stop("The input is not a 'regulondb_result' object.")
    }
    dataset <- regulondb_result@dataset
    if (dataset %in% c("GENE", "DNA_OBJECTS")) {
        posLeft <- "posleft"
        posRight <- "posright"
    } else if (dataset == "OPERON") {
        posLeft <- "regulationposleft"
        posRight <- "regulationposright"
    } else {
        stop(
            sprintf(
                paste(
                    "Can not coerse 'regulondb_result' from dataset %s",
                    "into a GRanges object\n"
                ),
                dataset
            )
        )
    }
    if (all(c(posLeft, posRight) %in% names(regulondb_result))) {
        ### coding of regulondb for missing coordinates ###
        nopos <-
            regulondb_result[[posLeft]] == 9999999999 &
                regulondb_result[[posRight]] == 0
        keep <-
            !(is.na(regulondb_result[[posLeft]]) |
                is.na(regulondb_result[[posRight]]))
        keep <- keep & !nopos
        grdata <- GRanges(
            regulondb_result@organism,
            IRanges(
                start = regulondb_result[[posLeft]][keep],
                end = regulondb_result[[posRight]][keep]
            )
        )
        if ("strand" %in% names(regulondb_result)) {
            stnd <-
                ifelse(regulondb_result$strand[which(keep)] == "forward",
                    "+", "-"
                )
            stnd[which(is.na(stnd))] <- "*"
            strand(grdata) <- stnd
        }
        mcols(grdata) <-
            DataFrame(regulondb_result[keep, !colnames(regulondb_result) %in%
                c(posLeft, posRight, "strand"), drop = FALSE])
        if (sum(!keep) > 0) {
            warning(sprintf(
                "Dropped %s entries where genomic coordinates were NAs",
                sum(!keep)
            ))
        }
        grdata
    } else {
        stop(
            sprintf(
                paste(
                    "Not enough information to convert into a GRanges",
                    "object. Please make sure that the input the following",
                    "columns: \n\t%s"
                ),
                paste(c(posLeft, posRight), collapse = "\n\t")
            ),
            call. = FALSE
        )
    }
    metadata(grdata) <- c(
        metadata(grdata),
        list(
            organism = regulondb_result@organism,
            genome_version = regulondb_result@genome_version,
            database_version = regulondb_result@database_version,
            dataset = regulondb_result@dataset
        )
    )
    grdata
}

#' @title Function to convert output of regulondb queries to Biostrings objects
#' @description This function converts, when possible, a regulon_result object into a Biostrings object.
#' @param regulondb_result A regulon_result object.
#' @param seq_type A character string with either DNA or protein, specyfing what
#' @author Alejandro Reyes
#' @importFrom Biostrings DNAStringSet AAStringSet
#' @return A [XStringSet][Biostrings::XStringSet-class] object.
#' @export
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
#' ## Obtain all the information from the "GENE" dataset
#' convert_to_biostrings(get_dataset(e_coli_regulondb, dataset = "GENE"))
convert_to_biostrings <-
    function(regulondb_result, seq_type = "DNA") {
        if (!is(regulondb_result, "regulondb_result")) {
            stop("The input is not a 'regulondb_result' object.")
        }
        if (!seq_type %in% c("DNA", "product")) {
            stop("'seq_type' must be either 'DNA' or 'product'")
        }
        dataset <- regulondb_result@dataset
        if (dataset == "GENE") {
            if (seq_type == "DNA") {
                func <- Biostrings::DNAStringSet
                col_name <- "dna_sequence"
            } else {
                func <- Biostrings::BStringSet
                col_name <- "product_sequence"
            }
        } else if (dataset == "PROMOTER") {
            func <- Biostrings::DNAStringSet
            col_name <- "promoter_sequence"
        } else {
            stop(
                sprintf(
                    paste(
                        "Can not coerse 'regulondb_result' from dataset %s",
                        "into a Biostrings object\n"
                    ),
                    dataset
                )
            )
        }
        if (!col_name %in% colnames(regulondb_result)) {
            stop(
                sprintf(
                    paste(
                        "Not enough information to convert to a Biostrings",
                        "object.\nPlease add the following column to the",
                        "regulondb_result object: \n\t%s\n"
                    ),
                    col_name
                )
            )
        }
        seq_character <-
            gsub("\\*$", "", regulondb_result[[col_name]])
        keep <- !is.na(seq_character)
        rs <- func(seq_character[which(keep)])
        mcols(rs) <-
            regulondb_result[which(keep), !colnames(regulondb_result) %in%
                col_name, drop = FALSE]
        if (sum(!keep)) {
            warning(sprintf(
                "Dropped %s entries where sequence data were NAs",
                sum(!keep)
            ))
        }
        metadata(rs) <- c(
            metadata(rs),
            list(
                organism = regulondb_result@organism,
                genome_version = regulondb_result@genome_version,
                database_version = regulondb_result@database_version,
                dataset = regulondb_result@dataset
            )
        )
        rs
    }

dataframe_to_dbresult <- function(df, regulondb, dataset) {
    new(
        "regulondb_result",
        DataFrame(df),
        organism = regulondb@organism,
        genome_version = regulondb@genome_version,
        database_version = regulondb@database_version,
        dataset = dataset
    )
}
