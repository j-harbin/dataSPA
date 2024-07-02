#' Get the discriminating words used to identify EBM pillars or objectives.
#'
#' Provides words and colours to be used in highlighting. Defaults to Stephenson et al. 2018 https://onlinelibrary.wiley.com/doi/full/10.1111/faf.12296
#'
#' @param n highlights the n words with the highest term frequency–inverse document frequency (i.e. most important words that discriminate amongst pillars)
#' @param ties Should ties be kept together? The default, TRUE, may return more rows than you request. Use FALSE to ignore ties, and return the first n rows.
#' @param file the word or R file describing the EBM framework. Defaults to the IN folder
#'
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
#' @importFrom tidyr separate_wider_delim separate
#' @importFrom tidytext unnest_tokens
#' @importFrom stringi stri_replace_all_regex
#' @importFrom tidytext get_stopwords
#' @importFrom dplyr filter
#' @importFrom stringr word
#' @importFrom colorspace lighten
#' @importFrom colorspace darken
#' @importFrom dplyr n
#' @importFrom dplyr if_else
#' @importFrom dplyr slice_max
#' @export
#' @author Remi Daigle and Jaimie Harbin
getEBMpillars <- function(file="//dcnsbiona01a/BIODataSvc/IN/MSP/PowerBI-Projects/dataSPA/inputs/Stephenson_ebfm.R",n,ties=TRUE) {


    if(endsWith(file,"docx")){
      if (file.exists(file)) {
      subpoint <- words <-  color <- NULL
      message("You are using Bundy et al. (unpublished) for pillar definitions")
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
  } else if(endsWith(file,"R")){

    if (file.exists(file)) {
      message("You are using Stephenson et al. 2019 for pillar definitions")

      local_env <- new.env()
      source(file, local_env)

      pillars <- local_env$TABLE3 %>%
        separate(table3,sep="---",into = c("pillar","objective","subobj","indicator"))

      objectives <- pillars %>%
        group_by(pillar) %>%
        summarise(words=paste(objective,subobj,indicator,collapse=" "),
                  .groups = 'drop') %>%
        group_by(pillar) %>%
        unnest_tokens(word,words) %>%
        anti_join(get_stopwords(),by="word") %>%
        filter(!nchar(word)<=3,word!='shall') %>%
        group_by(pillar,word) %>%
        summarise(n=n(),
                  .groups = 'drop') %>%
        ungroup() %>%
        bind_tf_idf(word,pillar,n) %>%
        arrange(desc(tf_idf)) %>%
        group_by(pillar) %>%
        slice_max(tf_idf,
                  n=10,
                  with_ties = ties) #TODO think about how to handle ties?

      pal <- c("#7fc97f",
               "#beaed4",
               "#fdc086",
               "#ffff99")

      objectives_col <- objectives %>%
        right_join(data.frame(pillar=as.factor(unique(objectives$pillar)),
                              color=pal),
                   by="pillar")%>%
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


    } else {
      stop("You are either not on the VPN, or do not have access to the IN folder needed for this function")
    }
  }
  return(list(objectives_col=objectives_col,objectives=objectives,pal=pal))
}
