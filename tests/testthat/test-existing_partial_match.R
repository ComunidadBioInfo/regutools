context("build_condition")

test_that("existing_partial_match returns an expected value", {
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

    # Type character
    expect_type(
        existing_partial_match(
            filters = list(name = c("rec")),
            partialmatch = "name",
            operator = NULL
        ),
        "character"
    )
    # Length 1
    expect_length(
        existing_partial_match(
            filters = list(name = c("ara")),
            partialmatch = "name",
            operator = NULL
        ),
        1
    )
})
