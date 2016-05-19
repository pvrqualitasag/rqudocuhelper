#' Convert documents from source format into target format used in the document
#'
#' \code{convertLibOToPdf} assumes that graphics or diagrams are produced by
#' LibreOffice Draw. Source files are converted on the fly to pdf which are
#' then included in the source R markdown document
#'
#' @param psLibOFile    name of the libre office graphics file
#' @param psLibODir     source directory of Libre Office files
#' @param psFigOutDir   output directory where figure pdfs are expected to be
#' @export convertLibOToPdf
convertLibOToPdf <- function(psLibOFile, psLibODir = "odg", psFigOutDir = "."){
  sOdgDir <- psLibODir
  sOdgDirWin <- gsub("/", "\\", sOdgDir, fixed = TRUE)
  sConvCmdStem <- ifelse(.Platform$OS.type == "windows",
                         '"C:/Program Files (x86)/LibreOffice 5/program/soffice" --headless --convert-to pdf',
                         "soffice --headless --convert-to pdf")
  sFigFile <- ifelse(.Platform$OS.type == "windows",
                     paste(sOdgDirWin, psLibOFile, sep = "\\"),
                     file.path(sOdgDir, psLibOFile))
  sConvCommand <- paste(sConvCmdStem, sFigFile)
  system(command = sConvCommand)
  sPdfFile <- gsub("odg$", "pdf", psLibOFile)
  sFigOutFile <- file.path(psFigOutDir, sPdfFile)
  file.rename(from = sPdfFile, sFigOutFile)
}


## ---- Insert a Odg draw figure -------------------------------------------
#' Inserts an odg draw graphic into a rmarkdown text
#'
#' \code{insertOdgAsPdf} is just a wrapper to the same function
#' in package \code{rcoursetools}
#'
#' @param  psOdgFileStem       stem of odg figure file
#' @param  psOdgDir            directory where odg figure file is stored
#' @param  psFigOutDir         directory where output should be placed
#' @param  pbMustGenerate      flag whether pdf must be regenerated
#' @param  pnPaperWidthScale   factor with which graphic is scaled
#' @export insertOdgAsPdf
insertOdgAsPdf <- function(psOdgFileStem, psOdgDir = "odg",
                           psFigOutDir = ".",
                           pbMustGenerate = FALSE,
                           pnPaperWidthScale = NULL) {
  rcoursetools::insertOdgAsPdf(psOdgFileStem     = psOdgFileStem,
                               psOdgDir          = psOdgDir,
                               pbMustGenerate    = pbMustGenerate,
                               pnPaperWidthScale = pnPaperWidthScale)
}
