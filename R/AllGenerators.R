#' @name PANTHER
#' @inherit PANTHER-class title description return
#' @note Updated 2023-09-25.
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
#' @note Updated 2023-03-01.
#' @noRd
#'
#' @details
#' Release versions are here:
#'
#' ```r
#' url <- pasteUrl(
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
    "18.0",
    "17.0",
    "16.0",
    "15.0",
    "14.1",
    "14.0",
    "13.1",
    "13.0",
    "12.0",
    "11.0"
)



#' @rdname PANTHER
#' @export
PANTHER <- # nolint
    function(organism, release) {
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
            as.character(release), organism
        ))
        url <- pasteUrl(
            "ftp.pantherdb.org",
            "sequence_classifications",
            as.character(release),
            "PANTHER_Sequence_Classification_files",
            paste0("PTHR", release, "_", pantherName),
            protocol = "ftp"
        )
        file <- cacheUrl(url = url, pkg = .pkgName)
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
        data <- import(con = file, format = "tsv", colnames = colnames)
        assert(
            nrow(data) >= 5000L,
            msg = sprintf("Invalid URL: {.url %s}.", url)
        )
        data <- as(data, "DataFrame")
        ## Now using base R methods here instead of `tidyr::separate()`.
        idsplit <- DataFrame(do.call(
            what = rbind,
            args = strsplit(data[["dbXref"]], split = "|", fixed = TRUE)
        ))
        colnames(idsplit) <- c("organism", "keys", "uniprotId")
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
    .pantherReleases[[1L]]



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



## Updated 2023-09-25.
.PANTHER.caenorhabditisElegans <- # nolint
    function(data) {
        data[["geneId"]] <- strExtract(
            x = data[["keys"]],
            pattern = "WBGene\\d{8}$",
            fixed = FALSE
        )
        data
    }



## Updated 2023-09-25.
.PANTHER.drosophilaMelanogaster <- # nolint
    function(data) {
        data[["geneId"]] <- strExtract(
            x = data[["keys"]],
            pattern = "FBgn\\d{7}$",
            fixed = FALSE
        )
        data
    }



## Updated 2023-10-06.
.PANTHER.homoSapiens <- # nolint
    function(data) {
        h2e <- Hgnc()
        h2e <- as(h2e, "DataFrame")
        cols <- c("hgncId", "ensemblGeneId")
        assert(isSubset(cols, colnames(h2e)))
        h2e <- h2e[, cols]
        h2e <- h2e[complete.cases(h2e), , drop = FALSE]
        colnames(h2e)[colnames(h2e) == "ensemblGeneId"] <- "geneId"
        ## Filter Ensembl matches.
        ensembl <- data
        pattern <- "ENSG[0-9]{11}"
        keep <- grepl(
            pattern = pattern,
            x = ensembl[["keys"]]
        )
        ensembl <- ensembl[keep, , drop = FALSE]
        ensembl[["geneId"]] <- strExtract(
            x = ensembl[["keys"]],
            pattern = pattern,
            fixed = FALSE
        )
        ## Filter HGNC matches.
        hgnc <- data
        pattern <- "HGNC=([0-9]+)"
        keep <- grepl(pattern = pattern, x = hgnc[["keys"]])
        hgnc <- hgnc[keep, , drop = FALSE]
        hgnc[["hgncId"]] <- strMatch(
            x = hgnc[["keys"]],
            pattern = pattern,
            fixed = FALSE
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



## Updated 2023-09-25.
.PANTHER.musMusculus <- # nolint
    function(data) {
        m2e <- Mgi()
        m2e <- as(m2e, "DataFrame")
        cols <- c("mgiAccessionId", "ensemblGeneId")
        assert(isSubset(cols, colnames(m2e)))
        m2e <- m2e[, cols]
        m2e <- decode(m2e)
        m2e <- m2e[complete.cases(m2e), , drop = FALSE]
        colnames(m2e)[colnames(m2e) == "mgiAccessionId"] <- "mgiId"
        colnames(m2e)[colnames(m2e) == "ensemblGeneId"] <- "geneId"
        ## Filter Ensembl matches.
        ## NOTE PANTHER 16.0 doesn't contain any of these.
        ensembl <- data
        pattern <- "ENSG[0-9]{11}"
        keep <- grepl(pattern = pattern, x = ensembl[["keys"]])
        ensembl <- ensembl[keep, , drop = FALSE]
        ensembl[["geneId"]] <- strExtract(
            x = ensembl[["keys"]],
            pattern = pattern,
            fixed = FALSE
        )
        ## Filter MGI matches.
        mgi <- data
        pattern <- "MGI=([0-9]+)"
        keep <- grepl(pattern = pattern, x = mgi[["keys"]])
        mgi <- mgi[keep, , drop = FALSE]
        mgi[["mgiId"]] <- strMatch(
            x = mgi[["keys"]],
            pattern = pattern,
            fixed = FALSE
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
