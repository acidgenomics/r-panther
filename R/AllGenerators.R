#' @name PANTHER
#' @inherit PANTHER-class title description return
#' @note Updated 2023-03-01.
#'
#' @section Suported organisms:
#'
#' - *Caenorhabditis elegans*
#' - *Drosophila melanogaster*
#' - *Homo sapiens*
#' - *Mus musculus*
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param release `character(1)` or `NULL`.
#' PANTHER release version. If `NULL`, defaults to current release. Consult
#' the PANTHER website for a list of release versions available from the FTP
#' server (e.g. `"17.0"`).
#'
#' @examples
#' x <- PANTHER(organism = "Homo sapiens")
#' print(x)
NULL



## Updated 2020-10-12.
.pantherMappings <- c(
    "Homo sapiens" = "human",
    "Mus musculus" = "mouse",
    "Caenorhabditis elegans" = "nematode_worm",
    "Drosophila melanogaster" = "fruit_fly"
)



#' Supported PANTHER releases
#'
#' @note Updated 2022-05-10.
#' @noRd
#'
#' @details
#' Release versions are here:
#'
#' ```r
#' url <- pasteURL(
#' "ftp.pantherdb.org",
#' "sequence_classifications",
#' protocol = "ftp"
#' )
#' x <- RCurl::getURL(url = paste0(url, "/"), dirlistonly = TRUE)
#' x <- strsplit(x, split = "\n", fixed = TRUE)
#' ```
#'
#' Refer to the README file for TSV file specifications:
#' ftp://ftp.pantherdb.org/sequence_classifications/16.0/README
## nolint end
.pantherReleases <- c(
    "11.0",
    "12.0",
    "13.0",
    "13.1",
    "14.0",
    "14.1",
    "15.0", # Messed up on the FTP server?
    "16.0",
    "17.0" # 2022-02-22
)



#' @rdname PANTHER
#' @export
PANTHER <- # nolint
    function(organism, release) {
        if (is.null(release)) {
            release <- tail(.pantherReleases, n = 1L)
        }
        assert(
            hasInternet(),
            isOrganism(organism),
            isString(release)
        )
        organism <- match.arg(
            arg = organism,
            choices = names(.pantherMappings)
        )
        release <- match.arg(
            arg = release,
            choices = .pantherReleases
        )
        release <- numeric_version(release)
        pantherName <- .pantherMappings[[organism]]
        assert(isString(pantherName))
        alert(sprintf(
            "Downloading PANTHER %s annotations for {.emph %s}.",
            as.character(release),
            organism
        ))
        url <- pasteURL(
            "ftp.pantherdb.org",
            "sequence_classifications",
            as.character(release),
            "PANTHER_Sequence_Classification_files",
            paste0("PTHR", release, "_", pantherName),
            protocol = "ftp"
        )
        file <- cacheURL(url = url, pkg = .pkgName)
        if (isTRUE(release < 16L)) {
            ## The "geneName" column is not defined in older releases.
            colnames <- c(
                "dbXref",
                "proteinId",
                "pantherSubfamilyId",
                "pantherFamilyName",
                "pantherSubfamilyName",
                "goMf",
                "goBp",
                "goCc",
                "pantherClass",
                "pantherPathway"
            )
        } else {
            colnames <- c(
                "dbXref",
                "proteinId",
                "geneName",
                "pantherSubfamilyId",
                "pantherFamilyName",
                "pantherSubfamilyName",
                "goMf",
                "goBp",
                "goCc",
                "pantherClass",
                "pantherPathway"
            )
        }
        data <- import(
            con = file,
            format = "tsv",
            colnames = colnames,
            engine = "readr"
        )
        ## Harden against messed up files (e.g. 15.0 release).
        if (isTRUE(nrow(data) < 5000L)) {
            abort(sprintf("Invalid URL (missing items): {.url %s}.", url))
        }
        data <- as(data, "DataFrame")
        ## Now using base R methods here instead of `tidyr::separate()`.
        idsplit <- DataFrame(do.call(
            what = rbind,
            args = strsplit(data[["dbXref"]], split = "|", fixed = TRUE)
        ))
        ## FIXME Improve strict camel case here -- use "uniprotKb" instead.
        colnames(idsplit) <- c("organism", "keys", "uniprotKB")
        data[["dbXref"]] <- NULL
        data[["keys"]] <- idsplit[["keys"]]
        ## Using organism-specific internal return functions here.
        fun <- get(paste("", "PANTHER", camelCase(organism), sep = "."))
        assert(is.function(fun))
        data <- fun(data)
        assert(
            is(data, "DataFrame"),
            hasRows(data),
            isSubset("geneId", colnames(data)),
            !all(is.na(data[["geneId"]]))
        )
        data[["keys"]] <- NULL
        data <- data[, unique(c("geneId", colnames(data)))]
        keep <- !is.na(data[["geneId"]])
        data <- data[keep, , drop = FALSE]
        data <- unique(data)
        ## Some organisms have duplicate PANTHER annotations per gene.
        ## This is the case for Homo sapiens 14.0 release.
        keep <- !duplicated(data[["geneId"]])
        if (!all(keep)) {
            dupes <- data[["geneId"]][!keep]
            n <- length(dupes)
            alertWarning(sprintf(
                "%d duplicated gene %s detected: %s.",
                n,
                ngettext(
                    n = n,
                    msg1 = "identifier",
                    msg2 = "identifiers"
                ),
                toString(dupes, width = 100L)
            ))
        }
        data <- data[keep, , drop = FALSE]
        data <- data[order(data[["geneId"]]), , drop = FALSE]
        assert(hasNoDuplicates(data[["geneId"]]))
        alert("Splitting and sorting the GO terms.")
        data <- mutateAt(
            object = data,
            vars = c(
                "goBp",
                "goCc",
                "goMf",
                "pantherClass",
                "pantherPathway"
            ),
            fun = .splitPantherTerms
        )
        ## Sort columns alphabetically.
        data <- data[, sort(colnames(data)), drop = FALSE]
        rownames(data) <- data[["geneId"]]
        metadata(data) <- list(
            "date" = Sys.Date(),
            "organism" = organism,
            "packageVersion" = .pkgVersion,
            "release" = release
        )
        new(Class = "PANTHER", data)
    }

formals(PANTHER)[["release"]] <- # nolint
    tail(.pantherReleases, n = 1L)



## Updated 2022-05-10.
.splitPantherTerms <- function(x) { # nolint
    ## e.g. for C. elegans 13.1 release file.
    if (all(is.na(x))) {
        return(x)
    }
    x <- strsplit(x, split = ";", fixed = TRUE)
    x <- CharacterList(x)
    x <- sort(unique(x))
    x <- gsub(pattern = "#([A-Z0-9:]+)", replacement = " [\\1]", x = x)
    x <- gsub(pattern = ">", replacement = " > ", x = x)
    x
}



## Updated 2022-05-10.
.PANTHER.caenorhabditisElegans <- # nolint
    function(data) {
        data[["geneId"]] <- stri_extract_first_regex(
            str = data[["keys"]],
            pattern = "WBGene\\d{8}$"
        )
        data
    }



## Updated 2022-05-10.
.PANTHER.drosophilaMelanogaster <- # nolint
    function(data) {
        data[["geneId"]] <- stri_extract_first_regex(
            str = data[["keys"]],
            pattern = "FBgn\\d{7}$"
        )
        data
    }



## Updated 2022-05-10.
.PANTHER.homoSapiens <- # nolint
    function(data) {
        ## FIXME Need to rework this.
        h2e <- HGNC2Ensembl()
        assert(identical(colnames(h2e), c("hgncId", "ensemblId")))
        h2e <- as(h2e, "DataFrame")
        colnames(h2e)[colnames(h2e) == "ensemblId"] <- "geneId"
        ## Filter Ensembl matches.
        ensembl <- data
        pattern <- "ENSG[0-9]{11}"
        keep <- grepl(
            pattern = pattern,
            x = ensembl[["keys"]]
        )
        ensembl <- ensembl[keep, , drop = FALSE]
        ensembl[["geneId"]] <- stri_extract_first_regex(
            str = ensembl[["keys"]],
            pattern = pattern
        )
        ## Filter HGNC matches.
        hgnc <- data
        pattern <- "HGNC=([0-9]+)"
        keep <- grepl(pattern = pattern, x = hgnc[["keys"]])
        hgnc <- hgnc[keep, , drop = FALSE]
        hgnc[["hgncId"]] <- stri_match_first_regex(
            str = hgnc[["keys"]],
            pattern = pattern
        )[, 2L]
        hgnc[["hgncId"]] <- as.integer(hgnc[["hgncId"]])
        hgnc <- leftJoin(hgnc, h2e, by = "hgncId")
        hgnc[["hgncId"]] <- NULL
        keep <- !is.na(hgnc[["geneId"]])
        hgnc <- hgnc[keep, , drop = FALSE]
        hgnc <- unique(hgnc)
        ## Bind and return.
        do.call(what = rbind, args = list(ensembl, hgnc))
    }



## Updated 2022-05-10.
.PANTHER.musMusculus <- # nolint
    function(data) {
        ## FIXME Need to rework this.
        suppressWarnings({
            m2e <- MGI2Ensembl()
        })
        assert(
            is(m2e, "MGI2Ensembl"),
            identical(colnames(m2e), c("mgiId", "ensemblId"))
        )
        m2e <- as(m2e, "DataFrame")
        colnames(m2e)[colnames(m2e) == "ensemblId"] <- "geneId"
        ## Filter Ensembl matches.
        ## NOTE PANTHER 16.0 doesn't contain any of these.
        ensembl <- data
        pattern <- "ENSG[0-9]{11}"
        keep <- grepl(pattern = pattern, x = ensembl[["keys"]])
        ensembl <- ensembl[keep, , drop = FALSE]
        ensembl[["geneId"]] <- stri_extract_first_regex(
            str = ensembl[["keys"]],
            pattern = pattern
        )
        ## Filter MGI matches.
        mgi <- data
        pattern <- "MGI=([0-9]+)"
        keep <- grepl(pattern = pattern, x = mgi[["keys"]])
        mgi <- mgi[keep, , drop = FALSE]
        mgi[["mgiId"]] <- stri_match_first_regex(
            str = mgi[["keys"]],
            pattern = pattern
        )[, 2L]
        mgi[["mgiId"]] <- as.integer(mgi[["mgiId"]])
        mgi <- leftJoin(mgi, m2e, by = "mgiId")
        mgi[["mgiId"]] <- NULL
        keep <- !is.na(mgi[["geneId"]])
        mgi <- mgi[keep, , drop = FALSE]
        mgi <- unique(mgi)
        ## Bind and return.
        do.call(what = rbind, args = list(ensembl, mgi))
    }
