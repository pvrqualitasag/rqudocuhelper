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
                                initialize = function(pnMajorVersion = NA,
                                                      pnMinorVersion = NA,
                                                      pnFixLevel = NA){
                                  ### # by default we start with 0.0.900
                                  private$nMajorVersion <- 0
                                  if (!is.na(pnMajorVersion))
                                    private$nMajorVersion <- pnMajorVersion
                                  ### # minor version
                                  private$nMinorVersion <- 0
                                  if (!is.na(pnMinorVersion))
                                    private$nMinorVersion <- pnMinorVersion
                                  ### # fix level
                                  private$nFixLevel <- 900
                                  if (!is.na(pnFixLevel))
                                    private$nFixLevel <- pnFixLevel
                                },
                                incrementFixLevel = function(){
                                  private$nFixLevel <- private$nFixLevel + 1
                                },
                                incrementMinorVersion = function(){
                                  private$nMinorVersion <- private$nMinorVersion + 1
                                  private$nFixLevel <- 0
                                },
                                incrementMajorVersion = function(){
                                  private$nMajorVersion <- private$nMajorVersion + 1
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
                                  private$nMajorVersion <- as.numeric(sVersionString[1])
                                  private$nMinorVersion <- as.numeric(sVersionString[2])
                                  private$nFixLevel <- as.numeric(sVersionString[3])
                                }
                              ),
                              private = list(
                                nMajorVersion = 0,
                                nMinorVersion = 0,
                                nFixLevel     = 0
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
                                     ### # read status history, if it exists
                                     if (file.exists(private$history_file)){
                                       self$readStatusFromFile()
                                       nNrStatusRecords <- nrow(private$status_history)
                                       self$setVersion(psVersion = private$status_history[nNrStatusRecords, "version"])
                                       private$autoincrement_version()
                                     }
                                   },
                                   setVersion = function(psVersion){
                                     private$version <- psVersion
                                   },
                                   getVersion = function(){
                                     return(private$version)
                                   },
                                   setDate = function(psDate){
                                     private$date <- psDate
                                   },
                                   getDate = function(){
                                     return(private$date)
                                   },
                                   setAuthor = function(psAuthor){
                                     private$author <- psAuthor
                                   },
                                   getAuthor = function(){
                                     return(private$author)
                                   },
                                   setStatus = function(psStatus){
                                     private$status = psStatus
                                   },
                                   getStatus = function(){
                                     return(private$status)
                                   },
                                   setProject = function(psProject){
                                     private$project <- psProject
                                   },
                                   getProject = function(){
                                     return(private$project)
                                   },
                                   writeStatusToFile = function(psFileName = NULL){
                                     dfCurStatus <- private$stat_to_df()
                                     write.csv2(dfCurStatus, file = private$history_file,
                                                quote = FALSE,
                                                row.names = FALSE)
                                   },
                                   readStatusFromFile = function(psFileName = NULL){
                                     sFileName <- psFileName
                                     if (is.null(psFileName))
                                       sFileName <- private$history_file
                                     if (!file.exists(sFileName))
                                       stop("CANNOT FIND Status file: ", sFileName)
                                     private$status_history <- read.csv2(file = sFileName,
                                                                         stringsAsFactors = FALSE)
                                   },
                                   knitr_kable = function(){
                                     dfCurStatus <- private$stat_to_df()
                                     knitr::kable(dfCurStatus)
                                   }
                                 ),
                                 private   = list(version = "0.0.900",
                                                  author = Sys.info()[["user"]],
                                                  date   = as.character(Sys.Date()),
                                                  status = "Init",
                                                  project = "NA",
                                                  status_colnames = c("Version", "Date", "Author","Status","Project"),
                                                  status_history = NULL,
                                                  history_file = "DOCUMENTSTATUS",
                                                  stat_to_df = function() {
                                                    dfCurStatus <- data.frame(version = private$version,
                                                                              author  = private$author,
                                                                              date    = private$date,
                                                                              status  = private$status,
                                                                              project = private$project)
                                                    if (!is.null(private$status_history))
                                                      ### # check whether version number already exists
                                                      cur_stat_col <- which(dfCurStatus$version == private$status_history$version)
                                                      if (length(cur_stat_col) > 0) {
                                                        dfCurStatus <- rbind(private$status_history[-cur_stat_col,], dfCurStatus)
                                                      } else {
                                                        dfCurStatus <- rbind(private$status_history, dfCurStatus)
                                                      }
                                                    return(dfCurStatus)},
                                                  autoincrement_version = function(){
                                                    r6oVersion <- R6ClassVersion$new()
                                                    r6oVersion$string_parse(psVersionString = private$version)
                                                    r6oVersion$incrementFixLevel()
                                                    private$version <- r6oVersion$to_string()
                                                  }))



# addStatusRecord = function(psVersion = NULL,
#                            psDate    = NULL,
#                            psAuthor  = NULL,
#                            psStatus  = NULL,
#                            psProject = NULL){
#   ### # determine how many version records we have
#   nNrVersionRecs <- length(private$version)
#   ### # add version
#   if (is.null(psVersion)){
#     if (nNrVersionRecs > 0){
#       r6Version <- private$version[nNrVersionRecs]
#       r6Version$incrementFixLevel()
#     } else {
#       r6Version <- R6ClassVersion$new()
#     }
#   } else {
#     r6Version <- R6ClassVersion$new()
#     r6Version$string_parse(psVersion)
#   }
#   ### # add date
#   if (is.null(psDate)){
#     sCurDate <- Sys.Date()
#   } else {
#     sCurDate <- psDate
#   }
#   ### # add status
#   if (is.null(psStatus)){
#     sStatus <- ""
#   } else {
#     sStatus <- psStatus
#   }
#   ### # add project
#   if (is.null(psProject)){
#     sProject <- ""
#   } else{
#     sProject <- psProject
#   }
#   ### # add the components
#   if (nNrVersionRecs > 0){
#     private$version <- c(private$version, r6Version)
#     private$date <- c(private$date, sCurDate)
#     private$status <- c(private$status, sStatus)
#     private$project <- c(private$project, sProject)
#   } else {
#     private$version <- r6Version
#     private$date <- sCurDate
#     private$status <- sStatus
#     private$project <- sProject
#   }
#
# }
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


