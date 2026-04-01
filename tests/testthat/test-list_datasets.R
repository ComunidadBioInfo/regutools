context("list_datasets")
test_that("function list_dataset works as expected", {
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
    expect_type(list_datasets(regdb), "character")

    # Test that function fails if not database is provided
    expect_error(list_datasets(), "")
})
