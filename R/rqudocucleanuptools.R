###
###
###
###   Purpose:   Document tools
###   started:   2016/02/03 (pvr)
###
### ################################### ###

#' Cleaning up a vignette directory
#'
#' @description
#' \code{cleanup_vignettes} removes all output files that were
#' produced by compiling document sources. The goal is to have
#' only source files after the cleanup process.
#'
#' @details
#' The approach taken in function \code{cleanup_vignettes} is
#' to specify the pattern of the files to be deleted and the
#' pattern of the files to keep. The reason for this is that
#' the number of ouput formats to be specified is much smaller
#' than the number of files that must be kept. The files to be
#' removed are first collected into a vector, then the user
#' is asked whether to remove all files. This is a security
#' feature to avoid deleting files accidentally.
#'
#' @param psPath      path to root directory which should be cleaned
#' @param psPattern   that files to be clean match
#' @export cleanup_vignettes
cleanup_vignettes <- function(psPath = "vignettes", psPattern = "pdf$"){
  ### # files to be cleaned up
  for (p in psPattern) {
    if (!exists("files_to_remove")){
      files_to_remove <- list.files(path = psPath, pattern = p, full.names = TRUE)
    } else {
      files_to_remove <- c(files_to_remove, list.files(path = psPath, pattern = p, full.names = TRUE))
    }
  }
  message("Files to be removed: ", paste(basename(files_to_remove), collapse = ", "))
  sAnswer <- readline(prompt = "Should above files be removed [y/N]: ")
  if (tolower(sAnswer) == "y")
    file.remove(files_to_remove)

  invisible(TRUE)
}
