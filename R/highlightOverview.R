#' Highlight words in a project overview specific to EBFM framework
#'
#' This function highlights text based on the 2023 Ecological Pillar Objectives.
#'
#' @param om a data frame likely from `getData(type='om')`
#' @param id the project_id from the Project Planning Tool
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

highlightOverview <- function(om=NULL, id=NULL) {
  if (is.null(om)) {
    stop("In highlightOverview must provide an om argument, likely from getData(type='om')")
  }
  if (is.null(id)) {
    stop("Must provide an id argument in highlightOverview")
  }

  subpoint <- words <-  NULL

  file <- "//dcnsbiona01a/BIODataSvc/IN/MSP/PowerBI-Projects/dataSPA/inputs/Ecological Pillar Objectives 20 April 2023.docx"
  if (file.exists(file)) {
  docx <- (read_docx(file) %>%
             docx_summary())[-c(1:4,18),]
  } else {
    stop("You are either not on the VPN, or do not have access to the IN folder needed for this function")
  }

  pillars <- data.frame()
  for(i in seq_along(docx$text)){
    # if(i==47) browser()
    if(is.na(docx$level[i])) {
      pillar <- docx$text[i]
    }else if(docx$level[i]==1){
      objective <- docx$text[i]
    }else if(docx$level[i]==2){
      point <- docx$text[i]
      if(docx$level[i]>=docx$level[i+1]){
        pillars <- rbind(pillars,
                         data.frame(pillar=pillar,
                                    objective=objective,
                                    point=point,
                                    subpoint="",
                                    level=docx$level[i]))
      }
    }else if(docx$level[i]>2){
      # browser()
      pillars <- rbind(pillars,
                       data.frame(pillar=pillar,
                                  objective=objective,
                                  point=point,
                                  subpoint=docx$text[i],
                                  level=docx$level[i]))
    }

  }

  pillars <- pillars %>%
    separate_wider_delim(pillar,":",names=c("pillar","description"))%>%
    mutate(pillar=gsub(".. ","",pillar))


  objectives <- pillars %>%
    mutate(objective=paste0(pillar,": ",objective)) %>%
    group_by(pillar,description,objective) %>%
    summarise(words=paste(point,subpoint,collapse=" "),
              .groups = 'drop') %>%
    group_by(pillar,description,objective) %>%
    unnest_tokens(word,words) %>%
    anti_join(get_stopwords(),by="word") %>%
    filter(!nchar(word)<=3) %>%
    group_by(pillar,description,objective,word) %>%
    summarise(n=n(),
              .groups = 'drop') %>%
    ungroup()


  pal <- c(lighten("#1b9e77",0.4),
           lighten("#1b9e77",0.1),
           darken("#1b9e77",0.1),
           darken("#1b9e77",0.4),
           lighten("#7570b3",0.4),
           "#7570b3",
           darken("#7570b3",0.4),
           lighten("#d95f02",0.2),
           darken("#d95f02",0.2))


  objectives_col <- objectives %>%
    right_join(data.frame(objective=as.factor(unique(objectives$objective)),
                          color=pal),
               by="objective")%>%
    group_by(word) %>%
    summarise(hl_word=if_else(n()==1,
                              paste0("<span style='background-color: ",color, "'>", word, "</span>",collapse = ""),
                              lapply(seq(length(color)),function(c){
                                seg <- length(word)
                                len <- nchar(unique(word))
                                len %/% seg
                                if(c==1){
                                  paste0("<span style='background-color: ",color[c], "'>",
                                         substr(unique(word),1,len %/% seg + len %% seg),
                                         "</span>",collapse = "")
                                } else {
                                  paste0("<span style='background-color: ",color[c], "'>",
                                         substr(unique(word),
                                                (len %/% seg)*(c-1) + len %% seg +1,
                                                (len %/% seg)*(c-1) + len %% seg + len %/% seg),
                                         "</span>",collapse = "")
                                }
                              }) %>%
                                unlist() %>%
                                paste0(collapse = "")
    )
    )



keep <- unlist(unique(om$overview[which(om$project_id == id)]))
highlightedtext <-  stri_replace_all_regex(keep, pattern=objectives_col$word,
                                           replacement=objectives_col$hl_word,
                                           vectorize_all=FALSE)
highlightedtext

}
