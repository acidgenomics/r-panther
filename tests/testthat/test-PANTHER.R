test_that("organism", {
    for (organism in names(.pantherMappings)) {
        invisible(capture.output({
            object <- PANTHER(organism)
        }))
        expect_s4_class(object, "PANTHER")
        expect_output(
            object = show(object),
            regexp = "PANTHER"
        )
    }
})
