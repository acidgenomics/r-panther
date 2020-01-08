#' @inherit PANTHER-class title description return
#' @note Updated 2019-08-27.
#' @export
#'
#' @section Suported organisms:
#'
#' - *Caenorhabditis elegans*
#' - *Drosophila melanogaster*
#' - *Homo sapiens*
#' - *Mus musculus*
#'
#' @inheritParams acidroxygen::params
#' @param release `character(1)` or `NULL`.
#'   PANTHER release version. If `NULL`, defaults to current release. Consult
#'   the PANTHER website for a list of release versions available from the FTP
#'   server (e.g. `"14.0"`).
#'
#' @examples
#' options(acid.test = TRUE)
#' x <- PANTHER(
#'     organism = "Homo sapiens",
#'     BPPARAM = BiocParallel::SerialParam()
#' )
#' print(x)
PANTHER <- function(  # nolint
    organism,
    release = NULL,
    BPPARAM = BiocParallel::SerialParam(progressbar = TRUE)  # nolint
) {  # nolint
    assert(
        hasInternet(),
        isString(organism)
    )
    organism <- match.arg(
        arg = organism,
        choices = names(.pantherMappings)
    )
    pantherName <-  .pantherMappings[[organism]]
    assert(isString(pantherName))
    if (is.null(release)) {
        release <- "current_release"
    }
    assert(isString(release))
    release <- match.arg(arg = release, choices = .pantherReleases)
    message(sprintf(
        "Downloading PANTHER annotations for '%s' (%s).",
        organism,
        gsub("_", " ", release)
    ))
    if (isTRUE(getOption("acid.test"))) {
        file <- pasteURL(
            basejumpTestsURL, paste0("PTHR13.1_", pantherName, ".gz"),
            protocol = "none"
        )
    } else {
        ## FTP connection is unreliable for CI, so cover locally instead.
        ## nocov start
        file <- transmit(
            remoteDir = pasteURL(
                "ftp.pantherdb.org",
                "sequence_classifications",
                release,
                "PANTHER_Sequence_Classification_files",
                protocol = "ftp"
            ),
            pattern = pantherName,
            compress = TRUE,
            localDir = tempdir()
        )
        ## nocov end
    }
    data <- import(
        file = file,
        format = "tsv",
        colnames = c(
            "pantherID",
            "X2",
            "pantherSubfamilyID",
            "pantherFamilyName",
            "pantherSubfamilyName",
            "goMF",
            "goBP",
            "goCC",
            "pantherClass",
            "pantherPathway"
        )
    )
    data[["X2"]] <- NULL
    data <- as(data, "DataFrame")
    ## Now using base R methods here instead of `tidyr::separate()`.
    idsplit <- strsplit(data[["pantherID"]], split = "|", fixed = TRUE)
    idsplit <- DataFrame(do.call(what = rbind, args = idsplit))
    colnames(idsplit) <- c("organism", "keys", "uniprotKB")
    data[["pantherID"]] <- NULL
    data[["keys"]] <- idsplit[["keys"]]
    ## Using organism-specific internal return functions here.
    fun <- get(paste("", "PANTHER", camelCase(organism), sep = "."))
    assert(is.function(fun))
    data <- fun(data)
    assert(
        is(data, "DataFrame"),
        hasRows(data)
    )
    data[["keys"]] <- NULL
    data <- data[, unique(c("geneID", colnames(data)))]
    keep <- !is.na(data[["geneID"]])
    data <- data[keep, , drop = FALSE]
    data <- unique(data)
    ## Some organisms have duplicate PANTHER annotations per gene ID.
    split <- split(data, f = data[["geneID"]])
    split <- SplitDataFrameList(bplapply(
        X = split,
        FUN = function(x) {
            x <- x[order(x[["pantherSubfamilyID"]]), , drop = FALSE]
            x <- head(x, n = 1L)
            x
        },
        BPPARAM = BPPARAM
    ))
    data <- unsplit(split, f = unlist(split[, "geneID"]))
    data <- data[order(data[["geneID"]]), , drop = FALSE]
    assert(hasNoDuplicates(data[["geneID"]]))
    message("Splitting and sorting the GO terms.")
    data <- mutateAt(
        object = data,
        vars = c(
            "goBP",
            "goCC",
            "goMF",
            "pantherClass",
            "pantherPathway"
        ),
        fun = .splitPANTHERTerms,
        BPPARAM = BPPARAM
    )
    ## Sort columns alphabetically.
    data <- data[, sort(colnames(data)), drop = FALSE]
    rownames(data) <- data[["geneID"]]
    metadata(data) <- list(
        version = packageVersion("PANTHER"),
        date = Sys.Date()
    )
    metadata(data)[["organism"]] <- organism
    metadata(data)[["release"]] <- release
    new(Class = "PANTHER", data)
}



## Updated 2019-07-22.
.pantherMappings <- c(
    "Caenorhabditis elegans" = "nematode_worm",
    "Drosophila melanogaster" = "fruit_fly",
    "Homo sapiens" = "human",
    "Mus musculus" = "mouse"
)



## nolint start
##
## Release versions are here:
## > url <- pasteURL(
## >     "ftp.pantherdb.org",
## >     "sequence_classifications",
## >     protocol = "ftp"
## > )
## > x <- RCurl::getURL(url = paste0(url, "/"), dirlistonly = TRUE)
## > x <- strsplit(x, split = "\n", fixed = TRUE)
##
## nolint end
##
## Updated 2019-08-16.
.pantherReleases <- c(
    "11.0",
    "12.0",
    "13.0",
    "13.1",
    "14.0",
    "14.1",
    "current_release"
)



## Updated 2020-01-09.
.splitPANTHERTerms <- function(x, BPPARAM) {  # nolint
    assert(is.atomic(x))
    if (all(is.na(x))) return(x)
    bplapply(
        X = x,
        FUN = function(x) {
            if (is.na(x)) return(NULL)
            x <- strsplit(x, split = ";")
            x <- unlist(x)
            x <- sort(unique(x))
            x <- gsub("#([A-Z0-9:]+)", " [\\1]", x)
            x <- gsub(">", " > ", x)
            if (hasLength(x)) {
                x
            } else {
                NULL
            }
        },
        BPPARAM = BPPARAM
    )
}



## Updated 2019-08-16.
.PANTHER.homoSapiens <-  # nolint
    function(data) {
        hgnc2ensembl <- HGNC2Ensembl()
        ## Filter Ensembl matches.
        ensembl <- data
        pattern <- "ENSG[0-9]{11}"
        keep <- str_detect(string = ensembl[["keys"]], pattern = pattern)
        ensembl <- ensembl[keep, , drop = FALSE]
        ensembl[["geneID"]] <-
            str_extract(string = ensembl[["keys"]], pattern = pattern)
        ## Filter HGNC matches.
        hgnc <- data
        pattern <- "HGNC=([0-9]+)"
        keep <- str_detect(string = hgnc[["keys"]], pattern = pattern)
        hgnc <- hgnc[keep, , drop = FALSE]
        hgnc[["hgncID"]] <- as.integer(
            str_match(string = hgnc[["keys"]], pattern = pattern)[, 2L]
        )
        hgnc <- leftJoin(hgnc, hgnc2ensembl, by = "hgncID")
        hgnc[["hgncID"]] <- NULL
        keep <- !is.na(hgnc[["geneID"]])
        hgnc <- hgnc[keep, , drop = FALSE]
        hgnc <- unique(hgnc)
        ## Bind and return.
        do.call(what = rbind, args = list(ensembl, hgnc))
    }



## Updated 2019-08-16.
.PANTHER.musMusculus <-  # nolint
    function(data) {
        mgi2ensembl <- MGI2Ensembl()
        ## Filter Ensembl matches.
        ensembl <- data
        pattern <- "ENSG[0-9]{11}"
        keep <- str_detect(string = ensembl[["keys"]], pattern = pattern)
        ensembl <- ensembl[keep, , drop = FALSE]
        ensembl[["geneID"]] <-
            str_extract(string = ensembl[["keys"]], pattern = pattern)
        ## Filter HGNC matches.
        mgi <- data
        pattern <- "MGI=([0-9]+)"
        keep <- str_detect(string = mgi[["keys"]], pattern = pattern)
        mgi <- mgi[keep, , drop = FALSE]
        mgi[["mgiID"]] <- as.integer(
            str_match(string = mgi[["keys"]], pattern = pattern)[, 2L]
        )
        mgi <- leftJoin(mgi, mgi2ensembl, by = "mgiID")
        mgi[["mgiID"]] <- NULL
        keep <- !is.na(mgi[["geneID"]])
        mgi <- mgi[keep, , drop = FALSE]
        mgi <- unique(mgi)
        ## Bind and return.
        do.call(what = rbind, args = list(ensembl, mgi))
    }



## Updated 2019-08-16.
.PANTHER.drosophilaMelanogaster <-  # nolint
    function(data) {
        data[["geneID"]] <- str_extract(
            string = data[["keys"]],
            pattern = "FBgn\\d{7}$"
        )
        data
    }



## Updated 2019-08-16.
.PANTHER.caenorhabditisElegans <-  # nolint
    function(data) {
        data[["geneID"]] <- str_extract(
            string = data[["keys"]],
            pattern = "WBGene\\d{8}$"
        )
        data
    }
