% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/13-methods_dplyr.R
\name{left_join_wearable_dataset}
\alias{left_join_wearable_dataset}
\title{Left join for wearable_dataset}
\usage{
left_join_wearable_dataset(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE
)
}
\arguments{
\item{x}{(required) wearable_dataset class object.}

\item{y}{A data frame.}

\item{by}{by, see ?_left_join}

\item{copy}{copy, see ?_left_join}

\item{suffix}{suffix, see ?_left_join}

\item{...}{Other arguments, see ?_left_join}

\item{keep}{keep, see ?_left_join}
}
\value{
a wearable_dataset class object.
}
\description{
Left join for wearable_dataset
}
\author{
Xiaotao Shen
\email{shenxt1990@outlook.com}
}
