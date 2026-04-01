context("plot_dna_objects")
test_that("non-valid genomic elements causes error", {
    ## Connect to the RegulonDB database if necessary
    if (!exists("regulondb_conn")) {
        regulondb_conn <- connect_database()
    }

    ## Build a regulondb object
    regdb <-
        regulondb(
            database_conn = regulondb_conn,
            organism = "chr",
            genome_version = "prueba",
            database_version = "prueba"
        )
    expect_error(plot_dna_objects(
        regdb,
        elements = c("gene", "promotr")
    ))
})


test_that("non-valid granges causes error", {
    expect_error(plot_dna_objects(
        regdb,
        grange = GRanges("chr", IRanges(5000, "a"))
    ))
})
