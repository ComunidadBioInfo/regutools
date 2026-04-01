context("GRanges_convert")
test_that("Results from regulondb queries can be converted to GRanges", {
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
    query <- get_dataset(
        regdb,
        dataset = "GENE",
        attributes = c("posleft", "posright", "name", "strand"),
        filters = list(name = c("araC", "crp", "lacI"))
    )
    expect_s4_class(convert_to_granges(query), "GRanges")
    expect_error(convert_to_granges(query[, c("posleft", "name")]))
    expect_s4_class(convert_to_granges(query[, c("posleft", "posright")]), "GRanges")
    query <- get_dataset(
        regulondb = regdb,
        dataset = "DNA_OBJECTS"
    )
    hasNAs <- is.na(query$posleft) | is.na(query$posright)
    expect_s4_class(convert_to_granges(query[!hasNAs, ]), "GRanges")
    if (sum(hasNAs) > 0) {
        expect_warning(
            convert_to_granges(query),
            sprintf(
                "Dropped %s entries where genomic coordinates were NAs",
                sum(hasNAs)
            )
        )
    }
    expect_error(convert_to_granges(as.data.frame(query)))
    query <- get_dataset(regulondb = regdb, dataset = "OPERON")
    query$posright
    rs <- suppressWarnings(convert_to_granges(query))
    expect_s4_class(rs, "GRanges")
    metaSlots <-
        setdiff(slotNames(query), slotNames(as(query, "DataFrame")))
    expect_true(all(metaSlots %in% names(metadata(rs))))
    ##  query <- get_dataset( regulondb=regdb, dataset="RI" )
    ##  expect_s4_class( convert_to_granges( query ), "GRanges" )
    query <- get_dataset(
        regulondb = regdb,
        dataset = "TF",
        attributes = "total_regulated_tf",
        filters = list(name = c("Fis", "Rob", "TyrR"))
    )
    expect_error(convert_to_granges(query))
})

test_that("Results from regulondb queries can be converted to Biostrings", {
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
    query <- get_dataset(
        regdb,
        dataset = "GENE",
        attributes = c(
            "posleft",
            "posright",
            "name",
            "strand",
            "dna_sequence",
            "product_sequence"
        ),
        filters = list(name = c("araC", "crp", "lacI"))
    )
    expect_s4_class(convert_to_biostrings(query), "DNAStringSet")
    expect_s4_class(
        convert_to_biostrings(query, seq_type = "product"),
        "BStringSet"
    )
    expect_error(convert_to_biostrings(query, seq_type = "other"))
    query2 <- get_dataset(regdb, dataset = "PROMOTER")
    isNA <- is.na(query2[["promoter_sequence"]])
    rs <- convert_to_biostrings(query2[!isNA, ])
    if (sum(isNA) > 0) {
        expect_warning(
            convert_to_biostrings(query2),
            sprintf(
                "Dropped %s entries where sequence data were NAs",
                sum(isNA)
            )
        )
    }
    expect_s4_class(rs, "DNAStringSet")
    expect_equal(
        length(suppressWarnings(convert_to_biostrings(query2))),
        sum(!is.na(query2$promoter_sequence))
    )
    query3 <-
        get_dataset(regdb,
            dataset = "NETWORK",
            filters = list(regulated_name = "acrD")
        )
    expect_error(convert_to_biostrings(query3))
    expect_error(convert_to_biostrings(data.frame()))
    metaSlots <-
        setdiff(slotNames(query2), slotNames(as(query2, "DataFrame")))
    expect_true(all(metaSlots %in% names(metadata(rs))))
})
