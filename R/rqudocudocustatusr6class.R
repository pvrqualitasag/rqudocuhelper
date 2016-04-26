###
###
###
###   Purpose:   R6 Class representing Document status objects
###   started:   2016/04/18 (pvr)
###
### ############################################################ ###


#' @title R6 Class Representing Version Objects
#'
#' @description
#' Version strings can be separated into three parts
#' \begin{enumerate}
#' \item major version
#' \item minor version
#' \item fix level
#' \end{enumerate}
#' For each of the three parts a private field is created
#'
#' @export R6ClassVersion
R6ClassVersion <- R6::R6Class(classname = "R6ClassVersion",
                              public = list(
                                initialize = function(pnMajorVersion = NULL,
                                                      pnMinorVersion = NULL,
                                                      pnFixLevel = NULL){
                                  ### # by default we start with 0.0.900
                                  private$nMajorVersion <- 0
                                  if (!is.null(pnMajorVersion))
                                    private$nMajorVersion <- pnMajorVersion
                                  ### # minor version
                                  private$nMinorVersion <- 0
                                  if (!is.null(pnMinorVersion))
                                    private$nMinorVersion <- pnMinorVersion
                                  ### # fix level
                                  private$nFixLevel <- 900
                                  if (!is.null(pnFixLevel))
                                    private$nFixLevel <- pnFixLevel
                                },
                                incrementFixLevel = function(pnFixLevelIncrement = 1){
                                  private$nFixLevel <- private$nFixLevel + pnFixLevelIncrement
                                },
                                incrementMinorVersion = function(pnMinorIncrement = 1){
                                  private$nMinorVersion <- private$nMinorVersion + pnMinorIncrement
                                  private$nFixLevel <- 0
                                },
                                incrementMajorVersion = function(pnMajorIncrement = 1){
                                  private$nMajorVersion <- private$nMajorVersion + pnMajorIncrement
                                  private$nMinorVersion <- 0
                                  private$nFixLevel <- 0
                                },
                                to_string = function(){
                                  return(paste(c(private$nMajorVersion, private$nMinorVersion, private$nFixLevel),
                                               sep = "", collapse = "."))
                                },
                                string_parse = function(psVersionString){
                                  sVersionString <- unlist(strsplit(psVersionString, split = ".", fixed = TRUE))
                                  stopifnot(identical(length(sVersionString),3L))
                                  private$nMajorVersion <- sVersionString[1]
                                  private$nMinorVersion <- sVersionString[2]
                                  private$nFixLevel <- sVersionString[3]
                                }
                              ),
                              private = list(
                                nMajorVersion = NULL,
                                nMinorVersion = NULL,
                                nFixLevel     = NULL
                              ))


#' @title R6 Class Representing Document Status Objects
#'
#' @description
#' \code{R6ClassDocuStatus} objects can be used to represent the status
#' of a given document. A core requirement is that the different status
#' records should be persistent across different compilation runs. That
#' makes it necessary to store the intermediate states of an \code{R6ClassDocuStatus}
#' object in a file. Given that requirement we must have methods for
#' reading status information from a file and for writing status information
#' to a file. Furthermore, we need a method to add a document status record
#' and we must be able to display all document status records as a table.
#'
#' @export R6ClassDocuStatus
R6ClassDocuStatus <- R6::R6Class(classname = "R6ClassDocuStatus",
                                 public    = list(
                                   initialize = function(){
                                     version <- R6ClassVersion$new()
                                     date    <- Sys.Date()
                                     author  <- Sys.info()[["user"]]
                                     status  <- ""
                                     project <- ""
                                   },
                                   addStatusRecord = function(psVersion = NULL,
                                                              psDate    = NULL,
                                                              psAuthor  = NULL,
                                                              psStatus  = NULL,
                                                              psProject = NULL){
                                     ### # determine how many version records we have
                                     nNrVersionRecs <- length(private$version)
                                     ### # add version
                                     if (is.null(psVersion)){
                                       if (nNrVersionRecs > 0){
                                         r6Version <- private$version[nNrVersionRecs]
                                         r6Version$incrementFixLevel()
                                       } else {
                                         r6Version <- R6ClassVersion$new()
                                       }
                                     } else {
                                       r6Version <- R6ClassVersion$new()
                                       r6Version$string_parse(psVersion)
                                     }
                                     ### # add date
                                     if (is.null(psDate)){
                                       sCurDate <- Sys.Date()
                                     } else {
                                       sCurDate <- psDate
                                     }
                                     ### # add status
                                     if (is.null(psStatus)){
                                       sStatus <- ""
                                     } else {
                                       sStatus <- psStatus
                                     }
                                     ### # add project
                                     if (is.null(psProject)){
                                       sProject <- ""
                                     } else{
                                       sProject <- psProject
                                     }
                                     ### # add the components
                                     if (nNrVersionRecs > 0){
                                       private$version <- c(private$version, r6Version)
                                       private$date <- c(private$date, sCurDate)
                                       private$status <- c(private$status, sStatus)
                                       private$project <- c(private$project, sProject)
                                     } else {
                                       private$version <- r6Version
                                       private$date <- sCurDate
                                       private$status <- sStatus
                                       private$project <- sProject
                                     }

                                   }
                                 ),
                                 private   = list(version = NULL,
                                                  author = NULL,
                                                  date   = NULL,
                                                  status = NULL,
                                                  project = NULL))

#' R6Class Representing A Generic Table Object
#'
#'
R6ClassGenericTable <- R6::R6Class(classname = "R6ClassGenericTable",
                                   public    = list(
                                     setTableHeader = function(psTableHeader){
                                       private$sTableHeader <- psTableHeader
                                     },
                                     getTableHeader = function(){
                                       return(private$sTableHeader)
                                     },
                                     setTableBody = function(plTableBody){
                                       private$lTableBody <- plTableBody
                                     },
                                     getTableBody = function(){
                                       return(private$lTableBody)
                                     },
                                     addRow = function(plTableRow){
                                       private$lTableBody <- c(private$lTableBody, list(plTableRow))
                                     },
                                     writeTsvToFile = function(psFileName, pbRowNames = FALSE,
                                                               pbColNames = FALSE){
                                       dfTable <- as.data.frame(private$lTableBody)
                                       colnames(dfTable) <- private$sTableHeader
                                       write.table(dfTable, file = psFileName,
                                                   quote = FALSE, sep = "\t",
                                                   row.names = pbRowNames, col.names = pbColNames)
                                     }
                                   ),
                                   private   = list(sTableHeader = "",
                                                    lTableBody   = list()))


