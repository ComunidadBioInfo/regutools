#' @title Construct logical condition to query database
#' @description Given a list of filters, this function builds a logical
#' condition to query database.
#' The output is used in [get_dataset()].
#' @author Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca,
#' José Alquicira Hernández, Joselyn Chávez
#'
#' @param dataset dataset of interest
#' @param filters List of filters to be used. The names should correspond to
#' the attribute and the values correspond to the condition for selection.
#' @param operator A string indicating if all the filters (AND) or some of them
#' (OR) should be met
#' @param interval the filters with values considered as interval
#' @param regulondb A [regulondb()] object.
#' @param partialmatch name of the condition(s) with a string pattern for full
#' or partial match in the query
#'
#' @return A `character(1)` with the sql logical condition to query the dataset
#' .
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
#' ## Build the condition for ara
#' build_condition(
#'     e_coli_regulondb,
#'     dataset = "GENE",
#'     filters = list(
#'         name = c("ara"),
#'         strand = c("forward"),
#'         posright = c("2000", "40000")
#'     ),
#'     operator = "AND",
#'     interval = "posright",
#'     partialmatch = "name"
#' )
#' @export

build_condition <-
    function(
      regulondb,
      dataset,
      filters,
      operator,
      interval,
      partialmatch
    ) {
        if (is(filters, "list")) {
            if (!all(names(filters) %in% list_attributes(regulondb, dataset))) {
                non.existing.attrs.index <-
                    names(filters) %in% list_attributes(regulondb, dataset)
                non.existing.attrs <-
                    names(filters)[!non.existing.attrs.index]
                stop("Attribute(s) ",
                    non.existing.attrs,
                    " do not exist.",
                    call. = FALSE
                )
            }
            if (!is.null(interval)) {
                if (!all(interval %in% names(filters))) {
                    non.existing.interv.index <-
                        !(interval %in% names(filters))
                    non.existing.interv <-
                        (interval[non.existing.interv.index])
                    stop(
                        "Intervals ",
                        paste0(
                            '"',
                            paste(non.existing.interv, collapse = ", "),
                            '"'
                        ),
                        " do not exist in the filters you provided",
                        call. = FALSE
                    )
                }

                condition_intervals <-
                    existing_intervals(
                        filters, interval,
                        operator, partialmatch
                    )
                if ((length(filters) == length(interval))) {
                    condition_intervals <-
                        existing_intervals(
                            filters, interval,
                            operator, partialmatch
                        )
                    return(condition_intervals)
                } else {
                    # non-equal case
                    conditions_nonintervals <-
                        non_existing_intervals(
                            filters, interval,
                            operator, partialmatch
                        )
                    conditionall <-
                        paste(condition_intervals,
                            conditions_nonintervals,
                            sep = " AND "
                        )
                    return(conditionall)
                }
            } else {
                # NULL case
                conditions_nonintervals <-
                    non_existing_intervals(
                        filters, interval,
                        operator, partialmatch
                    )
                return(conditions_nonintervals)
            }
        } else {
            stop("The argument filters is not a list", call. = FALSE)
        }
    }
