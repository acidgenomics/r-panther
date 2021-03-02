library(testthat)
library(PANTHER)
test_check("PANTHER")
if (isTRUE(getOption("acid.test.extra"))) {
    test_dir(file.path("tests", "testthat-extra"))
}
