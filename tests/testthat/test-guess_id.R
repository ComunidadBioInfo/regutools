test_that("guess_id works ", {
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

    expect_error(guess_id(1, regdb))
    expect_error(guess_id(c("araC", "araG"), regdb))
    expect_length(guess_id("araC", regdb), 1)
    expect_length(guess_id("b0064", regdb), 1)
    expect_length(guess_id("ECK120000050", regdb), 1)
})
