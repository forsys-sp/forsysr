% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forsys_functions.R
\name{combine_priorities}
\alias{combine_priorities}
\title{Combine priorities}
\usage{
combine_priorities(
  stands,
  fields = NULL,
  weights = NULL,
  new_field = "combined_priority",
  append_weights = FALSE
)
}
\arguments{
\item{stands}{Data frame containing stand data}

\item{fields}{Field names (2 or more) to combine}

\item{weights}{Numeric vector with weights. Assume equal weighting if NULL}

\item{new_field}{Name to assign combined priority}
}
\description{
Combines 2 or more priorities into a new field.
}
