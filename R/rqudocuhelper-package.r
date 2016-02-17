#' Helper function for creating Qualitas documentation.
#'
#' @description
#' Documentation inside Qualitas can be a number of different documents such as
#'
#' \itemize{
#' \item Project plans
#' \item Specifications
#' \item Implementation documents
#' }
#'
#' Most of the documentations are just one source text file and sometimes a few
#' source files for diagrams. Hence, it is debatable whether the documentation
#' should be embedded into complete R packages. The big advantage of doing everything
#' in a R package is the absolute standardization and the uniformness with which
#' everything can be dealt with. A negative point is the overhead coming with a
#' R package, that means to put a lot of infrastructure into place that is not
#' used.
#'
#' The goal of this package is to fill in some blanks in constructing documentation
#' and in managing the documents during their lifetime.
#'
#' @details
#' It is important to note that the template documents that are coming with this package
#' cannot be compiled with the development version of \code{rmarkdown} which comes from
#' GitHub.
#'
#' @name rqudocuhelper
#' @docType package
NULL
