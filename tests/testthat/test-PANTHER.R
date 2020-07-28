context("PANTHER")

skip_if_not(hasInternet())

test_that("organism", {
    for (organism in names(.pantherMappings)) {
        invisible(capture.output({
            object <- PANTHER(organism)
        }))
        expect_s4_class(object, "PANTHER")
    }
})

test_that("show", {
    object <- PANTHER("Homo sapiens")
    expect_output(
        object = show(object),
        regexp = "PANTHER"
    )
})
