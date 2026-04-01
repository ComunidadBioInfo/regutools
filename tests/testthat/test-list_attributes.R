context("list_attributes")
test_that("list_attributes works as expected ", {
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

    # Returns a character vector
    expect_type(
        list_attributes(regdb, dataset = "DNA_OBJECTS"),
        "character"
    )

    # Test that function fails if not enough arguments are given
    expect_error(list_attributes(), "")
    expect_error(list_attributes(regdb), "")
})
