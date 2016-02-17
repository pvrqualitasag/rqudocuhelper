###
###
###
###   Purpose:   Reference class for section counting
###   started:   2016/02/16 (pvr)
###
### ################################################### ###


#' reference Class for section counting
#' @export
#' @exportClass
sectionCount <- setRefClass(Class   = "SectionCount",
                            fields  = list(nSectionCount       = "numeric",
                                           nSubSectionCount    = "numeric",
                                           nSubSubsectionCount = "numeric",
                                           sHash               = "character",
                                           sCountSep           = "character"),
                            methods = list(
                              initialize = function(){
                                nSectionCount       <<- 0
                                nSubSectionCount    <<- 0
                                nSubSubsectionCount <<- 0
                                sCountSep           <<- "."
                              },
                              setHash = function(psHash){
                                sHash <<- psHash
                              },
                              incrSectionCounts = function(){
                                nNrHash <- nchar(sHash)
                                if (nNrHash == 3){
                                  nSubSubsectionCount <<- nSubSubsectionCount + 1
                                } else if(nNrHash == 2){
                                  nSubSectionCount <<- nSubSectionCount + 1
                                  nSubSubsectionCount <<- 0
                                } else if(nNrHash == 1){
                                  nSectionCount <<- nSectionCount + 1
                                  nSubSubsectionCount <<- 0
                                  nSubSectionCount <<- 0
                                }
                              },
                              sGetSectionNumber = function(){
                                sSectionNumberResult <- NULL
                                if (nSectionCount > 0)
                                  sSectionNumberResult <- as.character(nSectionCount)
                                if (nSubSectionCount > 0)
                                  sSectionNumberResult <- paste(sSectionNumberResult,
                                                                as.character(nSubSectionCount),
                                                                sep = sCountSep)
                                if (nSubSubsectionCount > 0)
                                  sSectionNumberResult <- paste(sSectionNumberResult,
                                                                as.character(nSubSubsectionCount),
                                                                sep = sCountSep)
                                return(sSectionNumberResult)
                              }
                            ))


#' Reference class for automatically enumerating section titles
#' @export
#' @exportClass
SectionEnumerator <- setRefClass(Class   = "SectionEnumerator",
                                 fields  = list(sUnNumSection     = "character",
                                                rcSectionCount    = "SectionCount",
                                                sHash             = "character",
                                                sRemCaption       = "character",
                                                sNumCaptionResult = "character",
                                                sSectionSplit     = "character",
                                                nNrHash           = "numeric"
                                 ),
                                 methods = list(
                                   initialize = function(){
                                     sUnNumSection     <<- ""
                                     sHash             <<- ""
                                     sRemCaption       <<- ""
                                     sNumCaptionResult <<- ""
                                     sSectionSplit     <<- " "
                                     rcSectionCount    <<- sectionCount$new()
                                   },
                                   setUnNumSection = function(psUnNumSection){
                                     sUnNumSection <<- psUnNumSection
                                   },
                                   parseUnNumSection = function(){
                                     vecUnNumCaption <- unlist(strsplit(sUnNumSection,sSectionSplit))
                                     sHash <<- vecUnNumCaption[1]
                                     nNrHash <<- nchar(sHash)
                                     sRemCaption <<- paste0(vecUnNumCaption[2:length(vecUnNumCaption)],
                                                            collapse = " ")
                                     rcSectionCount$setHash(psHash = sHash)
                                     rcSectionCount$incrSectionCounts()
                                     sNumCaptionResult <<- paste(sHash,
                                                                 rcSectionCount$sGetSectionNumber(),
                                                                 sRemCaption)

                                   },
                                   displayNumSection = function(psUnNumSection = NULL){
                                     if (!is.null(psUnNumSection))
                                       sUnNumSection <<- psUnNumSection
                                     .self$parseUnNumSection()
                                     cat(sNumCaptionResult, "\n")
                                   }
                                 ))
