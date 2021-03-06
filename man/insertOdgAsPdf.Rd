% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rqudocugraphicsconverter.R
\name{insertOdgAsPdf}
\alias{insertOdgAsPdf}
\title{Inserts an odg draw graphic into a rmarkdown text}
\usage{
insertOdgAsPdf(psOdgFileStem, psOdgDir = "odg", psFigOutDir = ".",
  pbMustGenerate = FALSE, pnPaperWidthScale = NULL)
}
\arguments{
\item{psOdgFileStem}{stem of odg figure file}

\item{psOdgDir}{directory where odg figure file is stored}

\item{psFigOutDir}{directory where output should be placed}

\item{pbMustGenerate}{flag whether pdf must be regenerated}

\item{pnPaperWidthScale}{factor with which graphic is scaled}
}
\description{
\code{insertOdgAsPdf} is just a wrapper to the same function
in package \code{rcoursetools}
}

