context("build_condition")

test_that("existing_intervals returns an expected value", {
    ## Connect to the RegulonDB database if necessary
    if (!exists("regulondb_conn")) {
        regulondb_conn <- connect_database()
    }

    ## Build a regulondb object
    regdb <-
        regulondb(
            database_conn = regulondb_conn,
            organism = "prueba",
            genome_version = "prueba",
            database_version = "prueba"
        )

    existing_intervals <- build_condition(
        regdb,
        dataset = "GENE",
        filters = list(
            posright = c("2000", "40000"),
            posleft = c("2000", "40000")
        ),
        operator = NULL,
        interval = c("posright", "posleft"),
        partialmatch = NULL
    )
    expect_match(existing_intervals, ">=")
    # Having partial match
    existing_intervals_and_pm <-
        build_condition(
            regdb,
            dataset = "GENE",
            filters = list(
                name = c("ara"),
                strand = c("forward"),
                posright = c("2000", "40000")
            ),
            operator = "AND",
            interval = "posright",
            partialmatch = "name"
        )
    # Type character
    expect_type(existing_intervals_and_pm, "character")
    # Length 1
    expect_length(existing_intervals_and_pm, 1)


    # Having more that 2 values for intervales
    expect_warning(
        build_condition(
            regdb,
            dataset = "GENE",
            filters = list(
                posright = c("2000", "40000", "50000"),
                name = c("ara")
            ),
            operator = NULL,
            interval = "posright",
            partialmatch = NULL
        ),
        "Only the first two values of interval will be considered."
    )

    # Having only one value
    expect_error(
        build_condition(
            regdb,
            dataset = "GENE",
            filters = list(posright = c("2000")),
            operator = NULL,
            interval = "posright",
            partialmatch = NULL
        ),
        "Two values in the interval filter are required. "
    )
})
