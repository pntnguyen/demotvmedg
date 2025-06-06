% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitg.R
\name{fitg}
\alias{fitg}
\title{fit data to model}
\usage{
fitg(
  data,
  boot = FALSE,
  mreg = "binomial",
  lreg = c("binomial", "gaussian", "gaussian"),
  yreg = "binomial",
  dof = 3
)
}
\arguments{
\item{data}{processed data}

\item{boot}{bootstrapping}

\item{mreg}{regression of mediator variable}

\item{lreg}{regression of time-varying variable}

\item{yreg}{regression of outcome variable}

\item{dof}{degree of freedom}
}
\value{
model and data
}
\description{
fit data to model
}
