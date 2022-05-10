#' PANTHER database annotations
#'
#' [PANTHER](http://www.pantherdb.org) gene ontology definitions. PANTHER stands
#' for **P**rotein **AN**alysis **TH**rough **E**volutionary **R**elationships.
#'
#' @note Updated 2022-05-10.
#' @export
#'
#' @return `PANTHER`.
setClass(
    Class = "PANTHER",
    contains = "DFrame"
)
setValidity(
    Class = "PANTHER",
    method = function(object) {
        ## PANTHER 16.0+ adds "geneName" column.
        validate(
            isSubset(
                x = c(
                    "geneId",
                    "goBp",
                    "goCc",
                    "goMf",
                    "pantherClass",
                    "pantherFamilyName",
                    "pantherPathway",
                    "pantherSubfamilyId",
                    "pantherSubfamilyName"
                ),
                y = colnames(object)
            ),
            isSubset(
                x = c("organism", "packageVersion", "release"),
                y = names(metadata(object))
            )
        )
    }
)
