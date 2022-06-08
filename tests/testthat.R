## nolint start
library(testthat)
library(PANTHER)
## nolint end

test_check("PANTHER")

if (isTRUE(getOption(x = "acid.test.extra"))) {
    test_dir(file.path("tests", "testthat-extra"))
}
