###
###
###
###   Purpose:   Reference class for section counting
###   started:   2016/02/16 (pvr)
###
### ################################################### ###


#' Reference Class for section count
#'
#' The class stores counts for three levels of sections.
#' The field sHash contains the hash signs that are taken
#' from an rmarkdown section title. Based on the number of
#' hash signs the corresponding counter is incremented
sectionCount <- setRefClass(Class   = "SectionCount",
                            fields  = list(nSectionCount       = "numeric",
                                           nSubSectionCount    = "numeric",
                                           nSubSubsectionCount = "numeric",
                                           sHash               = "character",
                                           sCountSep           = "character"),
                            methods = list(
                              initialize = function(){
                                'Initialize count fields and set default for count separator'
                                nSectionCount       <<- 0
                                nSubSectionCount    <<- 0
                                nSubSubsectionCount <<- 0
                                sCountSep           <<- "."
                              },
                              setHash = function(psHash){
                                sHash <<- psHash
                              },
                              incrSectionCounts = function(){
                                'Increment section counts based on number of hash signs'
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
                                'Return section number as string'
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
#'
#' @description
#' The level of a Markdown section headers is indicated using
#' hashes in front of the title. YAML frontmatter allows for
#' to specify section header numbering, but it is not clear
#' how to mix headers with and without numbers in the same
#' document. The \code{SectionEnumerator} reference class allows
#' us to specify section header numbers only for certain titles.
#'
#' @details
#' The main method of a SectionEnumerator reference object
#' is \code{displayNumSection}. This takes as input a string
#' of an unnumbered section, parses that string and computes
#' the section number based on the number of hashes.
#'
#' @export SectionEnumerator
#' @exportClass SectionEnumerator
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
