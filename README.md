# PANTHER

![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)

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
        "https://r.acidgenomics.com",
        BiocManager::repositories()
    ),
    dependencies = TRUE
)
```

[panther]: http://www.pantherdb.org/
[r]: https://www.r-project.org/
