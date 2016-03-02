###
###
###
###   Purpose:   Reference class for dcf
###   started:   2016/03/01 (pvr)
###
### ###################################### ###

#' Reference class for DESCRIPTION information
#'
#' @description
#' \code{DcfRefClass} reference objects can be used
#' to represent dcf structured information that is
#' usually stored in DESCRIPTION files of a package.
#' The basic functionality of \code{DcfRefClass}
#' reference objects consists of
#'
#' \itemize{
#'   \item reading a dcf files
#'   \item changing dcf components
#'   \item writing a dcf structure back to a file
#' }
#'
#' @details
#' When creating a new package with \code{devtools::create()}
#' the default description file contains the field Author@R
#' which contains the definition of a \code{person()} object
#' as a string. This field is difficult to change, hence this
#' field Author@R is converted into a simple field Author of
#' type string and in a field Maintainer which is also of type
#' string. This conversion is done immediately when the
#' DESCRIPTION file is read.
#'
#' @field lCurDesc   current dcf read from DESCRIPTION as a list
#' @export DcfRefClass
#' @exportClass DcfRefClass
DcfRefClass <- setRefClass(Class   = "DcfRefClass",
                           fields  = list(
                             lCurDesc = "list"
                             ),
                           methods = list(
                             setTitle = function(psTitle){
                               'setter for title'
                               lCurDesc$Title <<- psTitle
                             },
                             getTitle = function(){
                               'getter for title'
                               return(lCurDesc$Title)
                             },
                             setAuthor = function(psAuthor){
                               'setter for author'
                               lCurDesc$Author <<- psAuthor
                             },
                             getAuthor = function(){
                               'getter for author'
                               return(lCurDesc$Author)
                             },
                             setMaintainer = function(psMaintainer){
                               'setter for maintainer'
                               lCurDesc$Maintainer <<- psMaintainer
                             },
                             getMaintainer = function(){
                               'getter for maintainer'
                               return(lCurDesc$Maintainer)
                             },
                             setDescription = function(psDescription){
                               'setter for description'
                               lCurDesc$Description <<- psDescription
                             },
                             getDescription = function(){
                               'getter for description'
                               return(lCurDesc$Description)
                             },
                             setLicense = function(psLicense){
                               'setter for license'
                               lCurDesc$License <<- psLicense
                             },
                             getLicence = function(){
                               'getter for license'
                               return(lCurDesc$License)
                             },
                             readDcf = function(psDcfFile = "DESCRIPTION"){
                               'Read a description file using read.dcf. Convert resulting dataframe
                                 to a list using columnnames of dataframe as names for list, except
                                 for the field Author@R. See Details for more information'
                               dfCurDesc <- read.dcf(file = psDcfFile)
                               vColsKeep <- 1:ncol(dfCurDesc)
                               ### # in case dcf contains field "Authors@R", remove it
                               if (is.element("Authors@R", colnames(dfCurDesc)))
                                 vColsKeep <- vColsKeep[-c(which(colnames(dfCurDesc) == "Authors@R"))]
                               lCurDesc <<- as.list(dfCurDesc[,c(vColsKeep)])
                               names(lCurDesc) <<- colnames(dfCurDesc)[vColsKeep]
                             },
                             writeDcf = function(psDcfFile = "DESCRIPTION"){
                               'Write a description file using write.dcf'
                               write.dcf(lCurDesc, file = psDcfFile)
                             }))

