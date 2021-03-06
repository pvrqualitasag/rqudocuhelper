% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rqudocuskeletoncreator.R
\name{create_qudocu_skeleton}
\alias{create_qudocu_skeleton}
\title{Create a new Qualitas project document}
\usage{
create_qudocu_skeleton(psDocuName, psPkgPath = ".",
  psDocuSubdir = "vignettes", pbDocuHasOwnSubdir = TRUE,
  pbOverwrite = FALSE, pbEdit = TRUE)
}
\arguments{
\item{psDocuName}{name of the new document}

\item{psPkgPath}{path where package is located under which document should be created}

\item{psDocuSubdir}{subdirectory in which document should be saved to}

\item{pbDocuHasOwnSubdir}{should document be stored in separate subdir}

\item{pbOverwrite}{flag whether existing files are overwritten}

\item{pbEdit}{directly open newly created document}
}
\description{
This function is a wrapper for \code{create_docu_skeleton}
using the fixed template "project_docu" from package
\code{rqudocuhelper}.
}

