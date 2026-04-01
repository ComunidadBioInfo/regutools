context("build_condition")
test_that("Test that the logical conditions its made as expected", {
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
    condi_interval <- build_condition(
        regdb,
        dataset = "GENE",
        filters = list(posright = c("2000", "40000")),
        operator = NULL,
        interval = "posright",
        partialmatch = NULL
    )
    expect_match(condi_interval, ">=")
    expect_match(condi_interval, "<=")
    # More that one interval
    condi_2_filters_as_interval <-
        build_condition(
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
    expect_match(condi_2_filters_as_interval, ">=")
    expect_match(condi_2_filters_as_interval, "<=")
    # Type character
    expect_type(condi_interval, "character")
    # Length 1
    expect_length(condi_interval, 1)
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

    # Diferent operators
    condi_operar_and <- build_condition(
        regdb,
        dataset = "GENE",
        filters = list(
            name = c("ara"),
            strand = c("forward")
        ),
        operator = "AND",
        interval = NULL,
        partialmatch = NULL
    )
    expect_match(condi_operar_and, "AND")

    condi_no_interval <- build_condition(
        regdb,
        dataset = "GENE",
        filters = list(posright = c("2000", "40000")),
        operator = NULL,
        interval = "posright",
        partialmatch = NULL
    )

    condi_operar_or <- build_condition(
        regdb,
        dataset = "GENE",
        filters = list(
            name = c("ara"),
            strand = c("forward")
        ),
        operator = "OR",
        interval = NULL,
        partialmatch = NULL
    )
    expect_match(condi_operar_or, "OR")

    # errors

    expect_error(
        build_condition(
            regdb,
            dataset = "GENE",
            filters = list(posrig = c("2000", "40000")),
            operator = NULL,
            interval = c("name"),
            partialmatch = NULL
        )
    )

    expect_error(
        build_condition(
            regdb,
            dataset = "GENE",
            filters = list(posright = c("2000", "40000")),
            operator = NULL,
            interval = c("posrigh", "name"),
            partialmatch = NULL
        )
    )
    expect_error(
        build_condition(
            regdb,
            dataset = "GENE",
            filters = "posrigh",
            operator = NULL,
            interval = "posrigh",
            partialmatch = NULL
        ),
        "The argument filters is not a list"
    )
})
