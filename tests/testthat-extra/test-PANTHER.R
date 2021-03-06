context("extra | PANTHER")

organisms <- sort(names(.pantherMappings))

## Here's how to obtain supported releases.
## > releases <- setdiff(.pantherReleases, "current_release")

test_that("PANTHER : 11.0", {
    mapply(
        organism = organisms,
        nrow = c(
            "Caenorhabditis elegans" = 15010L,
            "Drosophila melanogaster" = 10610L,
            "Homo sapiens" = 19752L,
            "Mus musculus" = 21093L
        ),
        FUN = function(organism, nrow) {
            object <- PANTHER(organism = organism, release = "11.0")
            expect_s4_class(object, "PANTHER")
            expect_identical(nrow(object), nrow)
        },
        SIMPLIFY = FALSE
    )
})

test_that("PANTHER : 12.0", {
    mapply(
        organism = organisms,
        nrow = c(
            "Caenorhabditis elegans" = 14127L,
            "Drosophila melanogaster" = 10030L,
            "Homo sapiens" = 19558L,
            "Mus musculus" = 20882L
        ),
        FUN = function(organism, nrow) {
            object <- PANTHER(organism = organism, release = "12.0")
            expect_s4_class(object, "PANTHER")
            expect_identical(nrow(object), nrow)
        },
        SIMPLIFY = FALSE
    )
})

test_that("PANTHER : 13.0", {
    mapply(
        organism = organisms,
        nrow = c(
            "Caenorhabditis elegans" = 13494L,
            "Drosophila melanogaster" = 10087L,
            "Homo sapiens" = 18600L,
            "Mus musculus" = 19821L
        ),
        FUN = function(organism, nrow) {
            object <- PANTHER(organism = organism, release = "13.0")
            expect_s4_class(object, "PANTHER")
            expect_identical(nrow(object), nrow)
        },
        SIMPLIFY = FALSE
    )
})

test_that("PANTHER : 13.1", {
    mapply(
        organism = organisms,
        nrow = c(
            "Caenorhabditis elegans" = 13806L,
            "Drosophila melanogaster" = 10349L,
            "Homo sapiens" = 19671L,
            "Mus musculus" = 20929L
        ),
        FUN = function(organism, nrow) {
            ## NOTE C. elegans file here has parsing issues.
            suppressWarnings({
                object <- PANTHER(organism = organism, release = "13.1")
            })
            expect_s4_class(object, "PANTHER")
            expect_identical(nrow(object), nrow)
        },
        SIMPLIFY = FALSE
    )
})

test_that("PANTHER : 14.0", {
    mapply(
        organism = organisms,
        nrow = c(
            "Caenorhabditis elegans" = 14298L,
            "Drosophila melanogaster" = 10547L,
            "Homo sapiens" = 19770L,
            "Mus musculus" = 21058L
        ),
        FUN = function(organism, nrow) {
            object <- PANTHER(organism = organism, release = "14.0")
            expect_s4_class(object, "PANTHER")
            expect_identical(nrow(object), nrow)
        },
        SIMPLIFY = FALSE
    )
})

## NOTE The 15.0 release is messed up on the FTP server.

test_that("PANTHER : 16.0", {
    mapply(
        organism = organisms,
        nrow = c(
            "Caenorhabditis elegans" = 14426L,
            "Drosophila melanogaster" = 10656L,
            "Homo sapiens" = 19517L,
            "Mus musculus" = 20984L
        ),
        FUN = function(organism, nrow) {
            object <- PANTHER(organism = organism, release = "16.0")
            expect_s4_class(object, "PANTHER")
            expect_identical(nrow(object), nrow)
        },
        SIMPLIFY = FALSE
    )
})
