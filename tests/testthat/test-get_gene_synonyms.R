test_that("get_gene_synonyms works ", {
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

    expect_s4_class(
        get_gene_synonyms(regdb, "araC", from = "name"),
        "regulondb_result"
    )
    expect_error(get_gene_synonyms(regdb, 2, from = "name"))
    expect_error(get_gene_synonyms(regdb, "araC", from = "namez"))
    expect_error(get_gene_synonyms(regdb, "araC", from = "name", to = "namez"))
    expect_error(get_gene_synonyms(regdb, "araC", from = c("name", "id")))
})
