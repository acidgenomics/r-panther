## PANTHER 0.1.4 (2020-10-28)

### Minor changes

- Improved message consistency, using cli package.
- Sped up package checks for Travis CI inside Docker image.

## PANTHER 0.1.3 (2020-10-12)

### Minor changes

- Updated basejump dependencies.

## PANTHER 0.1.2 (2020-07-23)

### Minor changes

- Maintenance release, upgrading R dependency to 4.0.

## PANTHER 0.1.1 (2020-01-08)

### Minor changes

- Documentation updates, using roxygen2 7.0 release.
- Bug fix in transformer `mutateAll` required, for improved nested list column
  handling following Bioconductor update. Added an internal `as_tibble()` call
  prior to handoff to `DataFrame`, which improves return consistency for nested
  list columns.

## PANTHER 0.1.0 (2019-09-25)

Initial release. Code was split out from basejump package.
