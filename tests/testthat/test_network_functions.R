context("get_regulators")
test_that("Functions to retrieve gene regulation work as expected", {
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
    rs1 <- get_gene_regulators(regdb, c("araC", "fis", "crp"))
    expect_s4_class(rs1, "regulondb_result")
    expect_true(metadata(rs1)$format == "multirow")
    rs2 <-
        get_gene_regulators(regdb, c("araC", "fis", "crp"), format = "onerow")
    expect_s4_class(rs2, "regulondb_result")
    expect_true(metadata(rs2)$format == "onerow")
    rs3 <-
        get_gene_regulators(regdb, c("araC", "fis", "crp"), format = "table")
    expect_s4_class(rs3, "regulondb_result")
    expect_true(metadata(rs3)$format == "table")
    expect_s4_class(get_regulatory_network(regdb), "regulondb_result")
    expect_s4_class(
        get_regulatory_summary(regdb, gene_regulators = c("araC", "modB")),
        "regulondb_result"
    )
    expect_error(get_regulatory_summary(regdb, gene_regulators = c(1, 2, 3)))
    regulation_table <-
        get_gene_regulators(regdb,
            genes = c("araC", "modB"),
            format = "table"
        )
    expect_s4_class(
        get_regulatory_summary(regdb, regulation_table),
        "regulondb_result"
    )
    regulation_onerow <-
        get_gene_regulators(regdb,
            genes = c("araC", "modB"),
            format = "onerow"
        )
    expect_s4_class(
        get_regulatory_summary(regdb, regulation_onerow),
        "regulondb_result"
    )

    expect_error(
        get_gene_regulators(regdb, genes = NULL),
        "Parameter 'genes' must be a character vector or list."
    )
    expect_error(
        get_gene_regulators(regdb, genes = c("araC"), output.type = "TFS"),
        "Parameter 'output.type' must be either 'TF' or 'GENE'"
    )
    expect_error(
        get_regulatory_network(regdb, type = "GEN-GENE"),
        "Parameter 'type' must be TF-GENE, TF-TF, or GENE-GENE."
    )
    expect_s4_class(
        get_regulatory_network(regdb, regulator = "Fis"),
        "regulondb_result"
    )
    expect_error(
        get_regulatory_network(regdb, cytograph = TRUE),
        "To use integration with Cytoscape, please launch Cytoscape before running get_regulatory_network()"
    )
})
