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
#' \code{DcfRefClass} objects can be used to read
#' dcf files, to change information of the dcf
#' components and to write the dcf content back
#' to a file.
#'
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
                               'Read a description file using read.dcf'
                               dfCurDesc <- read.dcf(file = psDcfFile)
                               vColsKeep <- 1:ncol(dfCurDesc)
                               vColsKeep <- vColsKeep[-c(which(colnames(dfCurDesc) == "Authors@R"))]

                               lCurDesc <<- as.list(dfCurDesc[,c(vColsKeep)])
                               names(lCurDesc) <<- colnames(dfCurDesc)[vColsKeep]
                             },
                             writeDcf = function(psDcfFile = "DESCRIPTION"){
                               'Write a description file using write.dcf'
                               write.dcf(lCurDesc, file = psDcfFile)
                             }))

