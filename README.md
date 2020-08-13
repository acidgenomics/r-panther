# PANTHER

[PANTHER][] database annotations.

## Installation

### [R][] method

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "PANTHER",
    repos = c(
        "r.acidgenomics.com",
        BiocManager::repositories()
    )
)
```

[panther]: http://www.pantherdb.org/
[r]: https://www.r-project.org/
