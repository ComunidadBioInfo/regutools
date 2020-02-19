context("list_attributes")
test_that("list_attributes works as expected ", {
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

    # Returns a character vector
    resSimple <- list_attributes(regdb, dataset = "GENE")
    expect_type( resSimple,"character" )

    resComplex <- list_attributes(regdb, dataset = "GENE", description = TRUE )
    expect_s3_class( resComplex, "data.frame" )

    # Test that function fails if not enough arguments are given
    expect_error(list_attributes(), "")
    expect_error(list_attributes(regdb), "")

})
