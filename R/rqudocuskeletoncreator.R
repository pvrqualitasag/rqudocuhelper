###
###
###
###   Purpose:   Create skeleton documentation
###   started:   2016/02/16 (pvr)
###
### ############################################################ ###

#' Create a new rmd document in source directory "rmd"
#'
#' In case it does not exist, the root document source directory
#' "rmd" is created below the parent directory psPath. All pdf
#' documents in the directory "rmd" are set to be ignored by git.
#' The draft document is created by \code{rmarkdown::draft}.
#'
#' @param   psDocuName   name of the new document
#' @param   psPath       parent directory of rmd source dir
#' @export
create_docu_skeleton <- function(psDocuName, psPath = ".", pbEdit = FALSE) {
  ### # create directory for rmarkdown sources
  if (!dir.exists(file.path(psPath, "rmd")))
    dir.create(file.path(psPath, "rmd"))
  ### # add all pdf outputs to .gitignore
  add_git_ignore(psPath, "rmd/*.pdf")
  ### # put together the path for the new document
  sDocuPath <- file.path(psPath, "rmd", paste0(psDocuName, ".Rmd"))
  rmarkdown::draft(file = sDocuPath,
                   template = "project_docu",
                   package = "rdocuhelper",
                   create_dir = FALSE,
                   edit = FALSE)
  if (pbEdit) file.edit(sDocuPath)
  message("Draft document created as ", sDocuPath )
}

#' Adding a string to .gitignore
add_git_ignore <- function (psPath = ".", psIgnores)
{
  sPath <- file.path(psPath, ".gitignore")
  union_write(sPath, psIgnores)
  invisible(TRUE)
}

#' write a string to an existing file
union_write <- function (path, new_lines)
{
  if (file.exists(path)) {
    lines <- readLines(path, warn = FALSE)
  }
  else {
    lines <- character()
  }
  all <- union(lines, new_lines)
  writeLines(all, path)
}
