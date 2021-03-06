% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regulondb-classes.R
\docType{class}
\name{regulondb-class}
\alias{regulondb-class}
\title{The regulondb class}
\description{
The regulondb class is an extension of the SQLiteConnection,
which as the name suggests, consists of an SQLite connection to a database
with the table design of the
RegulonDb database. In addition to the slots defined in the
SQLiteConnection object,
the regulondb class also contains additional slots to store
information about database versions, organism information and genome build
versions.
}
\section{Slots}{

\describe{
\item{\code{organism}}{A character vector with the name of the organism of the
database.}

\item{\code{genome_version}}{A character vector with the version of the genome
build.}

\item{\code{database_version}}{A character vector with the version of regulondb
build.}
}}

