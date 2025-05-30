## PANTHER 0.5.1 (2025-05-25)

Maintenance release.

Minor changes:

- Added support for PANTHER 19.0.
- Dropped minimum R dependency back to 4.0 for better compatibility on legacy
  systems, namely Ubuntu 22 LTS.
- Now using air formatter.
- Updated lintr checks.

## PANTHER 0.5.0 (2023-10-06)

Major changes:

- Added support for new 18.0 release.
- Updated Acid Genomics dependencies. Note that imports now consistently use
  strict camel case for function names.

Minor changes:

- Now using AcidBase string splitting functions internally instead of stringi.
- No longer requiring readr engine for primary generator.
- Need to decode MGI annotations for mouse.

## PANTHER 0.4.0 (2023-08-12)

Major changes:

- Now requiring R 4.3 / Bioconductor 3.17.

Minor changes:

- Reworked main `PANTHER` generator to work with updates in pipette and
  AcidGenomes packages.

## PANTHER 0.3.0 (2022-05-10)

Major changes:

- Now requiring R 4.2 / Bioconductor 3.15.
- Added support for PANTHER 17.0 release.

Minor changes:

- Using stringi internally instead of stringr.

## PANTHER 0.2.2 (2021-09-08)

Minor changes:

- Updated basejump dependencies.
- Removed unnecessary `basejumpTestsURL` from NAMESPACE.
- Now using `abort` (from AcidCLI via basejump) for error messages.

## PANTHER 0.2.1 (2021-03-12)

Minor changes:

- Updated basejump dependencies, and removed now unnecessary stringr import.

## PANTHER 0.2.0 (2021-03-02)

Major changes:

- Simplified internal code, using improved methods defined in basejump v0.14
  release series. In particular, now taking advantage of `CharacterList`
  methods for operations on list columns, which are way faster than attempting
  to process via BiocParallel.
- Updated to support PANTHER 16.0, which has introduced some breaking changes.

## PANTHER 0.1.4 (2020-10-28)

Minor changes:

- Improved message consistency, using cli package.
- Sped up package checks for Travis CI inside Docker image.

## PANTHER 0.1.3 (2020-10-12)

Minor changes:

- Updated basejump dependencies.

## PANTHER 0.1.2 (2020-07-23)

Minor changes:

- Maintenance release, upgrading R dependency to 4.0.

## PANTHER 0.1.1 (2020-01-08)

Minor changes:

- Documentation updates, using roxygen2 7.0 release.
- Bug fix in transformer `mutateAll` required, for improved nested list column
  handling following Bioconductor update. Added an internal `as_tibble()` call
  prior to handoff to `DataFrame`, which improves return consistency for nested
  list columns.

## PANTHER 0.1.0 (2019-09-25)

Initial release. Code was split out from basejump package.
