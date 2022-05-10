## S4 classes ==================================================================

#' @importClassesFrom S4Vectors DFrame
NULL



## S4 generics and methods =====================================================

#' @importFrom AcidGenerics camelCase leftJoin mutateAt showHeader
#' @importFrom AcidGenomes HGNC2Ensembl MGI2Ensembl
#' @importFrom BiocGenerics do.call lapply rbind sort unique unlist unsplit
#' @importFrom IRanges gsub
#' @importFrom S4Vectors head metadata metadata<- split tail
#' @importFrom methods show
#' @importFrom pipette import
#'
#' @importMethodsFrom AcidBase showHeader
#' @importMethodsFrom AcidPlyr leftJoin mutateAt
#' @importMethodsFrom pipette import
#' @importMethodsFrom syntactic camelCase
NULL



## Standard functions ==========================================================

#' @importFrom AcidBase pasteURL showSlotInfo
#' @importFrom AcidCLI abort alert alertWarning
#' @importFrom goalie assert hasInternet hasLength hasNoDuplicates hasRows
#' isOrganism isString isSubset validate
#' @importFrom IRanges CharacterList SplitDataFrameList
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom methods as is new setClass setValidity
#' @importFrom pipette cacheURL
#' @importFrom stringi stri_detect stri_extract_first_regex
#' stri_match_first_regex
#' @importFrom utils packageName packageVersion
NULL
