#' Highlight words in a project overview specific to EBFM framework
#'
#' This function highlights text based on the 2023 Ecological Pillar Objectives.
#'
#' @param om a data frame likely from `getData(type='om')`
#' @param salary a data frame likely from `getData(type='salary')`
#' @param id the project_id from the Project Planning Tool
#' @param legend a boolean indicating if a legend should be returned
#' @param file the word file describing the EBM framwork. Defaults to the IN folder
#' @importFrom officer read_docx
#' @importFrom officer docx_summary
#' @importFrom tidyr separate_wider_delim
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom desc description
#' @importFrom dplyr anti_join
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom dplyr right_join
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidytext unnest_tokens
#' @importFrom stringi stri_replace_all_regex
#' @importFrom tidytext get_stopwords
#' @importFrom dplyr filter
#' @importFrom stringr word
#' @importFrom colorspace lighten
#' @importFrom colorspace darken
#' @importFrom dplyr n
#' @importFrom dplyr if_else
#' @export
#' @author Remi Daigle and Jaimie Harbin

highlightOverview <-
  function(om = NULL,salary = NULL,id = NULL,legend = FALSE,
           file="//dcnsbiona01a/BIODataSvc/IN/MSP/PowerBI-Projects/dataSPA/inputs/Stephenson_ebfm.R") {

    if (is.null(om) && is.null(salary)) {
      stop("In highlightOverview must provide an om or salary argument, likely from getData()")
    }
    if (is.null(id)) {
      stop("Must provide an id argument in highlightOverview")
    }

    if (!(is.null(salary))) {
      om <- salary
    }

    pillars <- getEBMpillars(file=file,
                                    n=10,
                                    ties = TRUE)


    keep <- unlist(unique(om$overview[which(om$project_id == id)]))
    highlightedtext <-  stri_replace_all_regex(keep, pattern=pillars$objectives_col$word,
                                               replacement=pillars$objectives_col$hl_word,
                                               vectorize_all=FALSE)
    if (!(legend)) {
      highlightedtext
    } else {
      # Create legend
      cat("EBM Colour Legend")
      paste0("<span style='background-color: ",pillars$pal,"'>",unique(pillars$objectives$pillar),"</span>")
    }

  }
