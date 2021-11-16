library(testthat)
library(PANTHER)
test_check("PANTHER")
if (isTRUE(getOption(x = "acid.test.extra"))) {
    test_dir(file.path("tests", "testthat-extra"))
}
