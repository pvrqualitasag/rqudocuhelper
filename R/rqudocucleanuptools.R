###
###
###
###   Purpose:   Document tools
###   started:   2016/02/03 (pvr)
###
### ################################### ###

#' Cleaning up a vignette directory
#'
#' \code{cleanup_vignettes} removes all output files that were
#' produced by any tools. The goal is to have only source files
#' after the cleanup process.
#'
#' @param pkg         package directory
#' @param psPattern   that files to be clean match
#' @export
cleanup_vignettes <- function(psPath = "rmd", psPattern = "pdf$"){
  ### # files to be cleaned up
  for (p in psPattern) {
    files_to_remove <- list.files(path = psPath, pattern = psPattern, full.names = TRUE)
    message("Removing files: ", paste(basename(files_to_remove), collapse = ", "))
    file.remove(files_to_remove)
  }
  invisible(TRUE)
}
