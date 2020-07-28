library(testthat)
library(PANTHER)
test_check("PANTHER")
if (isTRUE(getOption("acid.test.extra"))) {
    options(acid.test = FALSE)
    test_dir(file.path("tests", "testthat-extra"))
}
