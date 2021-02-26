#' Show an object
#' @name show
#' @inherit methods::show params return title
#' @keywords internal
#' @note Updated 2020-07-23.
NULL



## Updated 2019-07-22.
`show,PANTHER` <-  # nolint
    function(object) {
        showHeader(object)
        showSlotInfo(list(
            organism = metadata(object)[["organism"]],
            release = metadata(object)[["release"]],
            genes = object[["geneId"]]
        ))
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("PANTHER"),
    definition = `show,PANTHER`
)
