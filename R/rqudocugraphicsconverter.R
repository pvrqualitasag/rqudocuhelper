#' Convert documents from source format into target format used in the document
#'
#' \code{convertLibOToPdf} assumes that graphics or diagrams are produced by
#' LibreOffice Draw. Source files are converted on the fly to pdf which are
#' then included in the source R markdown document
#'
#' @param psLibOFile    name of the libre office graphics file
#' @param psLibODir     source directory of Libre Office files
#' @param psFigDirOut   output directory where figure pdfs are expected to be
#' @export
convertLibOToPdf <- function(psLibOFile, psLibODir = "odg", psFigDirOut = "."){
  sOdgDir <- psLibODir
  sOdgDirWin <- gsub("/", "\\", sOdgDir, fixed = TRUE)
  psFigDirOut <- "."
  sConvCmdStem <- ifelse(.Platform$OS.type == "windows",
                         '"C:/Program Files (x86)/LibreOffice 5/program/soffice" --headless --convert-to pdf',
                         "soffice --headless --convert-to pdf")
  sFigFile <- ifelse(.Platform$OS.type == "windows",
                     paste(sOdgDirWin, psLibOFile, sep = "\\"),
                     file.path(sOdgDir, psLibOFile))
  sConvCommand <- paste(sConvCmdStem, sFigFile)
  system(command = sConvCommand)
  sPdfFile <- gsub("odg$", "pdf", psLibOFile)
  sFigOutFile <- file.path(psFigDirOut, sPdfFile)
  file.rename(from = sPdfFile, sFigOutFile)
}
