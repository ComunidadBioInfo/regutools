context("get_dataset")
test_that("Function get dataset works as expected", {
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
    query <- get_dataset(
        regdb,
        dataset = "GENE",
        attributes = c("posleft", "posright", "name", "strand"),
        filters = list(name = c("araC", "crp", "lacI"))
    )
    expect_s4_class(query, "regulondb_result")
    expect_error(
        get_dataset(
            regdb,
            dataset = "GENE",
            attributes = c("posleft", "posright", "name", "strand"),
            filters = list(name = c("araC", "crp", "lacI")),
            output_format = "ss"
        )
    )
    expect_s4_class(
        suppressWarnings( get_dataset(
            regdb,
            dataset = "GENE",
            attributes = c("posleft", "posright", "name", "strand"),
            and = F,
            output_format = "GRanges"
        )),
        "GRanges"
    )
    expect_s4_class(get_dataset(
        regdb,dataset = "OPERON",
        filters = list(name = c("araC")),
        output_format = "GRanges"),
        "GRanges")

    expect_s4_class( suppressWarnings( get_dataset(
        regdb,dataset = "OPERON",
        output_format = "GRanges") ),
        "GRanges")

    expect_s4_class(
        get_dataset(
            regdb,
            dataset = "GENE",
            filters = list(name = c("araC", "crp", "lacI")),
            attributes = c("posleft", "posright", "name", "strand", "dna_sequence"),
            output_format = "DNAStringSet"
        ),
        "DNAStringSet")

    expect_s4_class(
        get_dataset(
            regdb,
            dataset = "GENE",
            filters = list(name = c("araC", "crp", "lacI")),
            attributes = c("posleft", "posright", "name", "strand", "dna_sequence"),
            output_format = "DNAStringSet"
        ),
        "DNAStringSet")
    expect_warning(get_dataset(
             regdb,
             dataset = "PROMOTER",
             attributes = c(
                     "name",
                     "strand",
                     "promoter_sequence"
                 ),
             output_format = "BStringSet"
         ),"entries where sequence data were NAs")

    expect_error( suppressWarnings( get_dataset(
        regdb,
        dataset = "PROMOTER",
        filters = list(name = c("araC", "crp", "lacI")),
        attributes = c("name", "strand"),
        output_format = "DNAStringSet"
    ) ), "Not enough information to convert to a Biostrings object.\nPlease add the following column to the regulondb_result object: \n\tpromoter_sequence\n")

    expect_error(
            get_dataset(
                regdb,
                dataset = "GENE",
                filters = list(name = c("araC", "crp", "lacI")),
                attributes = c(
                    "posleft",
                    "posright",
                    "name",
                    "strand"
                ),
                output_format = "BStringSet"))

    expect_error(get_dataset(
        regdb,dataset = "OPERON",
        attributes = c("name"),
        output_format = "GRanges"))

    expect_warning( get_dataset(
        regdb,
        dataset = "GENE",
        attributes = c("posleft", "posright", "name", "strand"),
        filters = list(name = c("thisisnotagene"))) )
    #Errors
    expect_error(get_dataset(
        regdb,
        dataset = "GENE",
        attributes = query,
        filters = list(name = c("thisisnotagene"))),
        "Parameter 'attributes' must be a vector"
    )
    expect_error(get_dataset(
        regdb,
        attributes = c("posleft", "posright", "name", "strand"),
        filters = list(name = c("thisisnotagene"))),
        "Non dataset provided"
    )
    expect_error(get_dataset(
        regdb,
        dataset = "GENe",
        attributes = c("posleft", "posright", "name", "strand"),
        filters = list(name = c("thisisnotagene"))),
        "Invalid dataset. See valid datasets in list_datasets()"
    )
    expect_error(get_dataset(
        regdb,
        dataset = "GENE",
        attributes = c("posleft", "posright", "name", "stran"),
        filters = list(name = c("thisisnotagene")))
    )
    expect_error(get_dataset(
        regdb,
        dataset = "GENE",
        attributes = c("posleft", "posright", "name", "strand"),
        filters = list(name = c("araC")),
        partialmatch="names"),
        "Partialmatch \"names\" do not exist")

    expect_error(get_dataset(
        regdb,
        dataset = "GENE",
        attributes = c("posleft", "posright", "name", "strand"),
        filters = list(name = c("araC")),
        partialmatch="posleft"),
        "Partialmatch \"posleft\" not defined in 'filters'")

})
