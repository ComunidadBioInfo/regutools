context("get_dna_objects")
test_that("output is a GRanges object", {
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
    reg_result <- get_dna_objects(regdb)

    expect_equivalent(class(reg_result), "GRanges")
})

test_that("non-valid genomic elements causes error", {
    expect_error(get_dna_objects(
        regdb,
        elements = c("gene", "promotr")
    ))
})


test_that("non-valid granges causes error", {
    expect_error(get_dna_objects(
        regdb,
        grange = GRanges("chr", IRanges(5000, "a"))
    ))
})
