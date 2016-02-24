###
###
###
###   Purpose:   Creator functions for ODG diagrams
###   started:   2016/02/24 (pvr)
###
### ################################################ ###

### ---- Based on templates create empty Odg diagram ---- ###
#' Initialize an empty Odg diagram
#'
#' @description
#' \code{create_empty_odgdiagram} uses function
#' \code{rmarkdown::draft} to initialize an empty
#' Odg diagram file.
#'
#' @param psOdgDiagramName   Name of odg diagram file
#' @param psOdgDiagramPath   Path to diagram directory
#' @param psOdgTemplate      Name of template to be used
#' @param psTemplatePkg      Package from where template is used
#' @param pbEdit             Should file be opened
#' @export create_empty_odgdiagram
create_empty_odgdiagram <- function(psOdgDiagramName,
                                    psOdgDiagramPath = "vignettes/odg",
                                    psOdgTemplate    = "odg_figure",
                                    psTemplatePkg    = "rqudocuhelper",
                                    pbEdit           = FALSE) {
  ### # in case psOdgDiagramPath does not exist, create it
  if (!dir.exists(psOdgDiagramPath))
    dir.create(psOdgDiagramPath)
  ### # move the template over
  sDocuPath <- file.path(psOdgDiagramPath, paste(psOdgDiagramName, "odg", sep = "."))
  simple_draft(file = sDocuPath,
               template = psOdgTemplate,
               package = psTemplatePkg,
               create_dir = FALSE)
  ### # must rename skeleton
  sTemplSkel <- file.path(psOdgDiagramPath, "skeleton.odg")
  file.rename(from = sTemplSkel, to = sDocuPath)
  ### # direct edit
  if (pbEdit) file.show(sDocuPath)
  ### # put a message at the end
  message("Empty odg diagram created in: ", sDocuPath)
}

## ---- Simplified local function for drafting ---- ##
#' Stripped down version of rmarkdown::draft
#'
#' @description
#' In contrast to \code{rmarkdown::draft}, we are here just
#' copying the files and subdirectories from the skeleton
#' subdirectory in the place that is given by file. We are
#' not doing anything with YAML here and we also want to
#' be independent of Rmd file endings.
#'
#' @param file        name of resulting file
#' @param template    name of template
#' @param package     name of package where template is
#' @param create_dir  should directory be created
simple_draft <- function (file, template,
                          package = NULL,
                          create_dir = FALSE) {
  ### # assign template path
  if (!is.null(package)) {
    template_path = system.file("rmarkdown", "templates",
                                template, package = package)
    if (!nzchar(template_path)) {
      stop("The template '", template, "' was not found in the ",
           package, " package")
    }
  } else {
    template_path <- template
  }

  if (create_dir) {
    file <- tools::file_path_sans_ext(file)
    if (dir_exists(file))
      stop("The directory '", file, "' already exists.")
    dir.create(file)
    file <- file.path(file, basename(file))
  }

  if (file.exists(file))
    stop("The file '", file, "' already exists.")
  skeleton_files <- list.files(file.path(template_path, "skeleton"),
                               full.names = TRUE)
  to <- dirname(file)
  for (f in skeleton_files) {
    if (file.exists(file.path(to, basename(f))))
      stop("The file '", basename(f), "' already exists")
    file.copy(from = f, to = to, overwrite = FALSE, recursive = TRUE)
  }

  invisible(file)
}
