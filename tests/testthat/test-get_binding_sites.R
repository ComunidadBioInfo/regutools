context("Binding sites")
test_that("Function to get transcription factor binding sites works as expected", {
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
    rs <-
        get_binding_sites(regdb, transcription_factor = "AraC")
    expect_s4_class(rs, "GRanges")
    rs <- get_binding_sites(regdb,
        transcription_factor = "AraC",
        output_format = "Biostrings"
    )
    expect_s4_class(rs, "DNAStringSet")
    expect_error(get_binding_sites(
        regdb,
        transcription_factor = "AraC",
        output_format = "other"
    ))
    expect_null(suppressWarnings(
        get_binding_sites(regdb, transcription_factor = "thisisnotagene")
    ))
})
