#' PANTHER database annotations
#'
#' [PANTHER](http://www.pantherdb.org) gene ontology definitions. PANTHER stands
#' for **P**rotein **AN**alysis **TH**rough **E**volutionary **R**elationships.
#'
#' @note Updated 2019-08-08.
#' @export
#'
#' @return `PANTHER`.
setClass(
    Class = "PANTHER",
    contains = "DataFrame"
)
setValidity(
    Class = "PANTHER",
    method = function(object) {
        validate(
            identical(
                x = colnames(object),
                y = c(
                    "geneID",
                    "goBP",
                    "goCC",
                    "goMF",
                    "pantherClass",
                    "pantherFamilyName",
                    "pantherPathway",
                    "pantherSubfamilyID",
                    "pantherSubfamilyName"
                )
            ),
            isSubset(c("organism", "release"), names(metadata(object)))
        )
    }
)
