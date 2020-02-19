context("build_condition")

test_that("existing_partial_match returns an expected value", {
    fileToDb <- file.path(tempdir(), "regulondb_sqlite3.db")
    if (!file.exists(fileToDb))
        download_database(tempdir())
    regdb <-
        regulondb(
            fileToDb,
            organism = "prueba",
            genome_version = "prueba",
            database_version = "prueba"
        )

    #No intervals no partial match
    no_intervals <- build_condition(
        regdb,
        dataset = "GENE",
        filters = list(name = c("ara"),
            strand = c("forward")),
        operator = "AND",
        interval = NULL,
        partialmatch = NULL
    )
    # Type character
    expect_type(no_intervals, "character")
    # Length 1
    expect_length(no_intervals, 1)

    #No intervals but partial match
    no_intervals <- build_condition(
        regdb,
        dataset = "GENE",
        filters = list(name = c("ara"),
            strand = c("forward")),
        operator = "AND",
        interval = NULL,
        partialmatch = "name"
    )
    # Type character
    expect_type(no_intervals, "character")
    # Length 1
    expect_length(no_intervals, 1)
    #No intervals only partial match
    no_intervals_pm <- build_condition(
        regdb,
        dataset = "GENE",
        filters = list(name = c("ara")),
        operator = NULL,
        interval = NULL,
        partialmatch = "name"
    )
    # Type character
    expect_type(no_intervals_pm, "character")
    # Length 1
    expect_length(no_intervals_pm, 1)

})