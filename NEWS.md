# regutools 1.23.0

BUG FIXES

* Fixed URL for the database

# regutools 1.3.2

SIGNIFICANT USER-VISIBLE CHANGES

* The link provided inside connect_database() has been updated in order to connect with the latest version of regulonDB v10.8.

# regutools 1.3.1

BUG FIXES

* Fixed URL for the database. Resolves 
https://github.com/ComunidadBioInfo/regutools/issues/33.

# regutools 1.1.3

SIGNIFICANT USER-VISIBLE CHANGES

* The package CITATION file has been updated now that the online paper on
`regutools` is available at 
https://academic.oup.com/bioinformatics/advance-article-abstract/doi/10.1093/bioinformatics/btaa575/5861528 with DOI 10.1093/bioinformatics/btaa575 and PMID: 32573705.

DOCUMENTATION UPDATES

* roxygen version update was included and documentation was build under the new roxygen version.

# regutools 1.1.1

SIGNIFICANT USER-VISIBLE CHANGES

* The package CITATION file has been updated now that the pre-print on
`regutools` is available at 
https://www.biorxiv.org/content/10.1101/2020.04.29.068551v1 with DOI
 https://doi.org/10.1101/2020.04.29.068551.

# regutools 0.99.14

SIGNIFICANT USER-VISIBLE CHANGES

* Now `connect_database()` as a `bfc` parameter instead of `path`.

# regutools 0.99.13

SIGNIFICANT USER-VISIBLE CHANGES

* Now `connect_database()` uses `BiocFileCache::BiocFileCache()` and
`BiocFileCache::bfcrpath()` to download the database from Dropbox when it's
not available from `AnnotationHub::query()` and caches the file so you
don't have to re-download it multiple times.

# regutools 0.99.12

SIGNIFICANT USER-VISIBLE CHANGES

* Documentation website is now available at
http://comunidadbioinfo.github.io/regutools/. It gets updated with every
commit on the master branch (bioc-devel) using GitHub Actions and pkgdown.


# regutools 0.99.11

BUG FIXES

* Manual adjustment for lines > 80 chr in parameters description.


# regutools 0.99.10

BUG FIXES

* Bump Version to run R CMD check


# regutools 0.99.9

BUG FIXES

* Add a suggested biocView `Transcription`.


# regutools 0.99.8

BUG FIXES

* Change biocViews from AnnotationData to Software 


# regutools 0.99.7

BUG FIXES

* Fix a bug in pkgdown that is currently resolved by
`Rscript -e 'remotes::install_github("r-lib/pkgdown#1276")'`. 
Details at https://github.com/r-lib/pkgdown/pull/1276 and
related issues.


# regutools 0.99.6

BUG FIXES

* Manual adjust for some lines > 80 characters


# regutools 0.99.5

BUG FIXES

* Reindent lines and try line-wrapping for lines > 80 characters


# regutools 0.99.4

NEW FEATURES

* Added a `NEWS.md` file to track changes to the package.


# regutools 0.99.3

SIGNIFICANT USER-VISIBLE CHANGES

* DESCRIPTION file now contains an article-like summary in the field `regutools` package Description.
* DESCRIPTION was updated to show only one maintainer as required by http://bioconductor.org/developers/package-guidelines/#description

BUG FIXES

* Fix a bug in the function `get_regulatory_network()` related with Cytoscape conection.


# regutools 0.99.2

SIGNIFICANT CHANGES

* Removed the regutools.Rproj file (you can still have it, but it's not
version controlled).
* Bump R depends to 4.0 as required by BiocCheck. We'll see if it breaks.


# regutools 0.99.1

BUG FIXES

* Address as many errors, warnings and notes as possible from
http://bioconductor.org/spb_reports/regutools_buildreport_20200302121040.html#malbec2_check_anchor. For the Windows error, it's likely related to `mode = "w"` versus
`mode = "wb"` for `utils::download.file()` as in 
https://github.com/LieberInstitute/spatialLIBD/commit/7ac24dc72cc75e42c9af8382c661517f4a64f08d.


# regutools 0.99.0

NEW FEATURES

* Submitted to Bioconductor
