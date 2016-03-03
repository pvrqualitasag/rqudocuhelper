###
###
###
###   Purpose:   Reference class for section counting
###   started:   2016/02/16 (pvr)
###
### ################################################### ###


#' Reference Class for section counts
#'
#' @description
#' A reference object of reference class \code{SectionCount}
#' represents the numbers in front of a section title.
#'
#' @details
#' The section title number  counts the numbers of different
#' section at any given level up and until a given section
#' title. In a markdown (md) document section levels
#' of titles are denoted by hash (#) signs. Based on the
#' number of hash signs of a given section title, the level of
#' the corresponding section title can be inferred. The more
#' hash signs the lower the level of the section title. Hence
#' one hash means top-level section title, two hashes stand
#' for subsections, three hashes denote subsubsectiones, etc.
#' For a given section title the level determines the corresponding
#' number of the section title. For a top-level section there
#' is just one number, for a subsection there are two numbers
#' separated by a dot (.) and for subsubsections there are
#' three numbers all separated by dots. Each of the numbers
#' that are associated with a given section title count the
#' number of sections for a specific level up
#' and until that given section title.
#'
#' @field vSectionCount   vector with section counts
sectionCount <- setRefClass(Class   = "SectionCount",
                            fields  = list(vSectionCount       = "numeric",
                                           nSectionCount       = "numeric",
                                           nSubSectionCount    = "numeric",
                                           nSubSubsectionCount = "numeric",
                                           sHash               = "character",
                                           sCountSep           = "character",
                                           nNrExtraHash        = "numeric"),
                            methods = list(
                              .lGetDefaults = function(){
                                'default values for given reference object'
                                return(list(nNrSectionLevels = 6L))
                              }
                              initialize = function(){
                                'Initialize count fields and set default for count separator'
                                lRefObjDefaults     <- .self$.lGetDefaults()
                                vSectionCount       <<- vector(mode = "numeric",
                                                               length = lRefObjDefaults$nNrSectionLevels)
                                nSectionCount       <<- 0
                                nSubSectionCount    <<- 0
                                nSubSubsectionCount <<- 0
                                sCountSep           <<- "."
                                nNrExtraHash        <<- 0
                              },
                              setHash = function(psHash){
                                sHash <<- psHash
                              },
                              setNrExtraHash = function(pnNrExtraHash){
                                nNrExtraHash <<- pnNrExtraHash
                              },
                              incrSectionCounts = function(){
                                'Increment section counts based on number of hash signs'
                                # count the number of hash signs in sHash
                                nNrHash <- nchar(sHash)
                                # subtract any number of extra hashes
                                if (nNrExtraHash < nNrHash)
                                  nNrHash <- nNrHash - nNrExtraHash
                                # increment the section count at the appropriate
                                #  level
                                if (nNrHash > length(vSectionCount))
                                  stop(" * Current title seams to have ", nNrHash, " levels. The max. number of levels is: ",
                                        length(vSectionCount))
                                vSectionCount[nNrHash] <- vSectionCount[nNrHash] + 1
                                # set all counts of lower levels to 0
                                if (nNrHash < )
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
#' the section number based on the number of hashes. For some
#' reason, the option \code{number_sections = FALSE} does not
#' work for top level sections, but it only works for subsections.
#' As a work-around, all sections are moved down one level to subsections
#' by ignoring a given number of \code{nNrExtraHash} hash signs.
#' Hence a subsection will be numbered as a section and a subsubsection
#' will be numbered as a subsection etc.
#'
#' @field sUnNumSection       original ununmbered section string
#' @field rcSectionCount      reference object representing section count
#' @field sHash               hashes of caption
#' @field sRemCaption         remaining caption after removing hashes
#' @field sNumCaptionResult   resulting caption string with number
#' @field sSectionSplit       split character between number and title
#' @field nNrExtraHash        number of extra hashes not used for section counts
#' @export SectionEnumerator
#' @exportClass SectionEnumerator
SectionEnumerator <- setRefClass(Class   = "SectionEnumerator",
                                 fields  = list(sUnNumSection     = "character",
                                                rcSectionCount    = "SectionCount",
                                                sHash             = "character",
                                                sRemCaption       = "character",
                                                sNumCaptionResult = "character",
                                                sSectionSplit     = "character",
                                                nNrExtraHash      = "numeric"
                                 ),
                                 methods = list(
                                   initialize = function(){
                                     'Initalisation of object fields'
                                     sUnNumSection     <<- ""
                                     sHash             <<- ""
                                     sRemCaption       <<- ""
                                     sNumCaptionResult <<- ""
                                     sSectionSplit     <<- " "
                                     nNrExtraHash      <<- 0
                                     rcSectionCount    <<- sectionCount$new()
                                   },
                                   setUnNumSection = function(psUnNumSection){
                                     'Setter for unnumbered section string'
                                     sUnNumSection <<- psUnNumSection
                                   },
                                   setNrExtraHash = function(pnNrExtraHash){
                                     nNrExtraHash <<- pnNrExtraHash
                                   },
                                   parseUnNumSection = function(){
                                     'Parse the unnumbered section string and assign the object fields'
                                     vecUnNumCaption <- unlist(strsplit(sUnNumSection,sSectionSplit))
                                     sHash <<- vecUnNumCaption[1]
                                     sRemCaption <<- paste0(vecUnNumCaption[2:length(vecUnNumCaption)],
                                                            collapse = " ")
                                     rcSectionCount$setHash(psHash = sHash)
                                     rcSectionCount$setNrExtraHash(pnNrExtraHash = nNrExtraHash)
                                     rcSectionCount$incrSectionCounts()
                                     sNumCaptionResult <<- paste(sHash,
                                                                 rcSectionCount$sGetSectionNumber(),
                                                                 sRemCaption)

                                   },
                                   displayNumSection = function(psUnNumSection = NULL, pnNrExtraHash = NULL){
                                     'Parsing of unnumbered section string and display the string including the section number'
                                     if (!is.null(pnNrExtraHash))
                                       nNrExtraHash <<- pnNrExtraHash
                                     if (!is.null(psUnNumSection))
                                       sUnNumSection <<- psUnNumSection
                                     .self$parseUnNumSection()
                                     cat(sNumCaptionResult, "\n")
                                   }
                                 ))
