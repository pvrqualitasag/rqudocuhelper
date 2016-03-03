###
###
###
###   Purpose:   Helper functions
###   started:   2016/03/03 (pvr)
###
### ############################### ###

#' Render a document of documentclass scrreprt
#'
#' @description
#' This function \code{render_scrreprt} is just
#' a wrapper for \code{rmarkdown::render}.
#'
#' @param  psInput          input Rmd file to be rendered
#' @param  psTemplateFile   template file given to pandoc
#' @param  plIncludes       list of named includes
#' @param  pbViewResult     flag whether to directly show result
#' @export render_scrreprt
render_scrreprt <- function(psInput, psTemplateFile, plIncludes, pbViewResult = FALSE){
  ### # call render from package rmarkdown
  rmarkdown::render(input = psInput,
                    output_format = rmarkdown::pdf_document(number_sections = FALSE,
                                                            template = psTemplateFile,
                                                            includes = plIncludes))
  if (pbViewResult){
    sOutputFilename <- gsub("Rmd$", "pdf", psInput)
    if (file.exists(sOutputFilename)){
      cat(" * Viewing output file: ", sOutputFilename, "\n")
      file.show(sOutputFilename)
    } else {
      cat(" * Cannot find output file: ", sOutputFilename, "\n")
    }
  }
}
