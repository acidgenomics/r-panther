#' Show an object
#' @name show
#' @inherit methods::show
#' @keywords internal
#' @note Updated 2019-08-27.
NULL



## Updated 2019-07-22.
`show,PANTHER` <-  # nolint
    function(object) {
        showHeader(object)
        showSlotInfo(list(
            organism = metadata(object)[["organism"]],
            release = metadata(object)[["release"]],
            genes = object[["geneID"]]
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("PANTHER"),
    definition = `show,PANTHER`
)
