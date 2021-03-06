% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/existing_intervals.R
\name{existing_intervals}
\alias{existing_intervals}
\title{Constructs a particular logical condition to query database}
\usage{
existing_intervals(filters, interval, operator, partialmatch)
}
\arguments{
\item{filters}{List of filters to be used. The names should correspond to
the attribute and the values correspond to the condition for selection.}

\item{interval}{the filters with values considered as interval.}

\item{operator}{A string indicading if all the filters (AND) or some of
them (OR) should be met.}

\item{partialmatch}{name of the condition(s) with a string pattern for full
or partial match in the query.}
}
\value{
A \code{character(1)} with the sql logical condition to query the dataset.
}
\description{
Given a list of filters, this function builds a logical
condition to query database using intervals.
The output is used in \code{\link[=build_condition]{build_condition()}}.
}
\examples{

## Build the SQL query for existing interval partial matches for ara
existing_intervals(
    filters = list(
        name = "ara",
        strand = "for",
        posright = c("2000", "40000")
    ),
    interval = c("posright"),
    operator = "AND",
    partialmatch = c("name", "strand")
)
}
\author{
Carmina Barberena Jonás, Jesús Emiliano Sotelo Fonseca,
José Alquicira Hernández, Joselyn Chávez
}
