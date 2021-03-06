% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blbglm.R
\name{glm_each_subsample}
\alias{glm_each_subsample}
\title{Linear Regression Model of Each Subsample}
\usage{
glm_each_subsample(formula, data, family, n, B)
}
\arguments{
\item{formula}{the formula to be modelled}

\item{data}{the source data}

\item{family}{the model family}

\item{n}{the sample size}

\item{B}{the number of bootstraps performed}
}
\description{
An internal function to compute linear regression model
estimates for a subsample
}
