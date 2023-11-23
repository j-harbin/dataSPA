#' Get required data from the PPT API
#'
#' This function obtains information from the PPT for
#' either the O&M or salary investment.
#'
#' @param type the type of data that is wished to be extracted
#' (either `om`, `om_date`, `salary`, `salary_date`, `collaboration`,
#' `statusReport`, or `tags`).The types that end in `_date` will return the
#' date of creation of a locally stored file.
#'
#' @param cookie a sessionid and csrftoken from a Department of
#' Fisheries and Oceans Canada (DFO) employee in the following
#' format: csrftoken=YOURTOKEN; sessionid=YOURSESSIONID
#'
#' @param keep logical value to optionally keep `om` data on the hard disk.
#' Default is FALSE and will not save data on the hard disk. Value of TRUE
#' will save `om` to disk in `path` unless there is an `om` file already in
#' `path` that was created more recently than `age` (number of days)
#'
#' @param age maximum age in number of days that a file may be loaded.
#' Set to `0` to download new data every time.
#'
#' @param path path to save file. Default is in the shared IN folder.
#'
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  the function will limit the page count to the value of `debug`.
#'
#' @return dataframe
#'
#' @importFrom httr2 request
#' @importFrom httr2 req_headers
#' @importFrom httr2 req_retry
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_body_json
#' @importFrom httr2 resp_body_json
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract
#' @importFrom stats median
#' @examples
#' \dontrun{
#' library(dataSPA)
#' data(salaries)
#' cookie <- "csrftoken=YOURTOKEN; sessionid=YOURSESSIONID"
#' data <- getData(type="salary", cookie=cookie)
#' head(data,2)
#' }
#' @author Jaimie Harbin and Remi Daigle
#' @export
#' @examples
#' \dontrun{
#' # Fix File ( ./dataSPA_om.rds ) does not exist error
#'  # Notice keep = TRUE saves the file, and the path argument says where to save it
#' om <- getData(type="om", cookie=cookie, keep=TRUE, path=".")
#' # Notice sal and om are saved in the same path
#' sal <- getData(type="salary", cookie=cookie, keep=TRUE, path=".")
#'  # Notice om_date is saved in the same path and done AFTER type=”om”
#' omdate <- getData(type="om_date", cookie=cookie, keep=TRUE, path=".")
#' }

getData <- function(type=NULL, cookie=NULL, debug=0, keep=FALSE, age = 7, path="//dcnsbiona01a/BIODataSVC/IN/MSP/PowerBI-Projects/dataSPA/") {

  if (is.null(type)) {
    stop("Must provide a type argument of either 'om', 'om_date', 'salary','salary_date', 'collaboration', 'statusReport', or 'tags'")
  }

  if (is.null(cookie)) {
    stop("Must provide a cookie argument in the following format:csrftoken=YOURTOKEN; sessionid=YOURSESSIONID")
  }

  if (!(type %in% c("om", "salary", "om_date", "salary_date", "collaboration", "statusReport", "tags"))) {
    stop("Must provide a type argument of either 'om', 'om_date', 'salary','salary_date', 'collaboration', 'statusReport', or 'tags'")
  }
  if (debug > 0) {
    message("type = ", type)
  }

  # LOADING CACHED DATA
  if (type %in% c("om", "om_date")) {
    if (age > 0) {
      # Look for files in path, only return the most recent file the matches pattern
      fn <- file.path(path, "dataSPA_om.rds")
      if (file.exists(fn)) {
        # Load file if more recent than `keep` days old
        d <- as.Date(file.info(fn)$mtime)
        if ((Sys.Date() - d) < age) {
          if (type == "om") {
            om <- readRDS(file = fn)
            return(om)
          } else if (type == "om_date") {
            #message(paste0("returning date from file on disk(",fn,")"))
            return(d)
          }

        }
      }  else if (type == "om_date") {
        stop(
          paste(
            "File (",
            fn,
            ") does not exist. User must first save om type AND be on the VPN. See examples in ?getData for how to fix this."
          ))}}}


  if(age > 0 && type %in% c("salary", "salary_date")){
    # Look for files in path, only return the file the matches pattern
    fn <- file.path(path,"dataSPA_SAL.rds")

    if(file.exists(fn)){
      # Load file if more recent than `keep` days old
      d <- as.Date(file.info(fn)$mtime)
      if((Sys.Date()-d)<age){
        if(type=="salary"){
          SAL <- readRDS(file = fn)
          message(paste0("loading file from disk(",fn,")"))
          return(SAL)
        } else if(type=="salary_date"){
          message(paste0("returning date from file on disk(",fn,")"))
          return(d)
        }

      }
    } else if(type=="salary_date"){
      stop(paste("File (",fn,") does not exist. File must exist on disk for type 'salary_date' to return a date of file creation"))
    }
  }

  # FIXME: can't yet cache for collaborations or statusReport

  # 1. LISTING LINKS

  if (type %in% c("salary", "salary_date")) {
    links <- c("http://dmapps/api/ppt/om-costs","http://dmapps/api/ppt/project-years", "http://dmapps/api/ppt/activities-full/","http://dmapps/api/ppt/staff", "http://dmapps/api/ppt/divisions/", "http://dmapps/api/ppt/sections/", "http://dmapps/api/ppt/regions/", "http://dmapps/api/ppt/tags/", "http://dmapps/api/ppt/funding-sources/")
  } else if (type %in% c("om", "om_date")) {
    links <- c("http://dmapps/api/ppt/om-costs","http://dmapps/api/ppt/project-years", "http://dmapps/api/ppt/activities-full/", "http://dmapps/api/ppt/divisions/","http://dmapps/api/ppt/sections/","http://dmapps/api/ppt/regions/", "http://dmapps/api/ppt/tags/", "http://dmapps/api/ppt/funding-sources/")
  } else if (type == "collaboration") {
    links <- c("http://dmapps/api/ppt/collaborations/")
  } else if (type == "statusReport") {
    links <- c("http://dmapps/api/ppt/status-reports/", "http://dmapps/api/ppt/project-years/")
  } else if (type == "tags") {
    links <- c("http://dmapps/api/ppt/tags/")
  }

  API_DATA <- NULL

  for (i in seq_along(links)) {
    req <- httr2::request(links[i])

    # Add custom headers
    req <- req %>% httr2::req_headers("Cookie" = cookie)
    req <- req %>% httr2::req_headers("Accept" = "application/json")

    # Automatically retry if the request fails
    req <- req %>% httr2::req_retry(max_tries = 5)

    # Get the requested data by querying the API
    resp <- try(httr2::req_perform(req), silent=TRUE)

    if (inherits(resp, "try-error")) {
      stop("Make sure 1) you are on the VPN, 2) Your cookie is up to date, and 3) your cookie is in the following format: csrftoken=YOURTOKEN; sessionid=YOURSESSIONID")
    }

    # Read the returned data as a JSON file
    page_data <- httr2::resp_body_json(resp)
    if (type == "collaboration" && links[i] == "http://dmapps/api/ppt/collaborations/") {
      api_data <- page_data
    }

    if (type == "statusReport" && links[i] == "http://dmapps/api/ppt/status-reports/") {
      api_data <- page_data
    }

    if (type %in% c("om", "salary") && links[i] %in% c("http://dmapps/api/ppt/divisions/", "http://dmapps/api/ppt/sections/","http://dmapps/api/ppt/regions/", "http://dmapps/api/ppt/tags/", "http://dmapps/api/ppt/funding-sources/")) {
      api_data <- page_data
    }

    if (type == "tags" && links[i] == "http://dmapps/api/ppt/tags/") {
      return(data.frame(Reduce(rbind,page_data),row.names = NULL))
    }

    # Create a list to hold the list of full API results
    if (!(type %in% c("collaboration"))) {
      if (!(links[i] %in% c("http://dmapps/api/ppt/status-reports/", "http://dmapps/api/ppt/divisions/", "http://dmapps/api/ppt/sections/","http://dmapps/api/ppt/regions/","http://dmapps/api/ppt/tags/", "http://dmapps/api/ppt/funding-sources/"))) {
        api_data <- page_data$results

        # Get the information about the next page in the API results
        next_page <- page_data$`next`
        cat(paste0(next_page, '\n'))

        cat(paste0('Number of API records = ', length(api_data), '\n'))

        # Check if the next page is not null (end of pages) before extract the data from
        # next page.
        while (!is.null(next_page)) {
          # Modifying API Call
          req <- httr2::request(next_page)
          # Add custom headers
          req <- req %>% httr2::req_headers("Cookie" = cookie)
          req <- req %>% httr2::req_headers("Accept" = "application/json")
          # Automatically retry if the request fails
          req <- req %>% httr2::req_retry(max_tries = 5)
          # Get the requested data by querying the API
          resp <- httr2::req_perform(req)
          # Read the returned data as a JSON file
          page_data <- httr2::resp_body_json(resp)
          # Add current page data to full list
          api_data <- c(api_data, page_data$results)
          cat(paste0('Number of API records = ', length(api_data), '\n'))

          # Get the information about the next page in the API results
          next_page <- page_data$`next`
          if(debug>0){
            if(exists("debug_page")){
              debug_page <- debug_page-1
            } else {
              debug_page <- debug-1
            }

            if(debug_page==1) {
              debug_page <- debug
              next_page <- NULL
              }
          }
          cat(paste0(next_page, '\n'))
        }
      }
    }

    API_DATA[[i]] <- api_data
  }
  names(API_DATA) <- links

  # LINK 1: Dealing with "http://dmapps/api/ppt/om-costs"
  if ("http://dmapps/api/ppt/om-costs" %in% names(API_DATA)) {
    api_data <- API_DATA[[1]]

    # Fix NULL descriptions
    for (i in seq_along(api_data)) {
      if (length(api_data[[i]]$description) == 0) {
        api_data[[i]]$description <- 0
      } else if (is.na(api_data[[i]]$description)) {
        api_data[[i]]$description <- 0
      }
    }

    p <- NULL
    for (i in seq_along(api_data)) {
      p[[i]] <- as.data.frame(api_data[[i]][c("project_id", "category_display", "project_year_id", "amount", "funding_source_display", "id", "category_type", "description")])
    }

    if (type=="om") {
      om <- do.call(rbind, p)
    }
  }

  ## LINK 2: Dealing with "http://dmapps/api/ppt/project-years"
  if ("http://dmapps/api/ppt/project-years" %in% names(API_DATA)) {
    api_data2 <- API_DATA[[2]]
  }


  ## LINK 3: Dealing with "http://dmapps/api/ppt/activities-full/"
  if ("http://dmapps/api/ppt/activities-full/" %in% names(API_DATA)) {
    api_data3 <- API_DATA[[3]]
  }


  ## Putting "http://dmapps/api/ppt/activities-full/" into a data frame
  if ("http://dmapps/api/ppt/activities-full/" %in% names(API_DATA)) {

    deliv <- lapply(api_data3, function(x) x[c("type_display", "description")])
    titles <- lapply(api_data3, function(x) x$project_year_obj)
    Dyears <- lapply(titles, function(x) x$display_name)
    titles <- lapply(titles, function(x) x$project_title)
    names <- lapply(api_data3, function(x) x$name)
    ddf <- NULL
    for (i in seq_along(deliv)){
      if (is.null(deliv[[i]]$description)) {
        deliv[[i]]$description <- 0
      }
      DF <- c(deliv[[i]], titles[[i]], Dyears[[i]], names[[i]])
      names(DF) <- c("type", "description", "title", "year", "name")
      ddf[[i]] <- as.data.frame(DF)
    }
    DDF <- do.call(rbind, ddf)
    # If no description, fill in name
    DDF$description[which(DDF$description == "")] <- DDF$name[which(DDF$description == "")]
    #1. isolate specific title. #2. isolate year. #3. Combine deliverables. #4. Combine milestones
    DELIVERABLES <- vector(mode = "list", length(unique(DDF$title)))
    MILESTONES <- vector(mode = "list", length(unique(DDF$title)))

    for (i in seq_along(unique(DDF$title))) {
      d <- DDF[which(DDF$title == unique(DDF$title)[[i]]),] #1
      for (j in seq_along(unique(d$year))) {
        d2 <- d[which(d$year == unique(d$year)[[j]]),] #2
        DELIV <- d2[which(d2$type == "Deliverable"),] #3
        MS <- d2[which(d2$type == "Milestone"),] #4
        DELIVERABLES[[i]][j] <- paste0(unique(DELIV$description), collapse="|-----|")
        MILESTONES[[i]][j] <- paste0(MS$description, collapse="|-----|")
      }
    }
    names(DELIVERABLES) <- unique(DDF$title)
    names(MILESTONES) <- unique(DDF$title)
  }

  ## Putting "http://dmapps/api/ppt/project-years" into a data frame
  if ("http://dmapps/api/ppt/project-years" %in% names(API_DATA)) {

    t <- lapply(api_data2, function(x) x$project$years)

    # Add objectives and overview
    p <- lapply(api_data2, function(x) x$project)
    j <- lapply(api_data2, function(x) x$project$lead_staff)

    for (i in seq_along(p)) {
      if (length(p[[i]]$overview) == 0) {
        p[[i]]$overview <- 0
      } else if (is.na(p[[i]]$overview)) {
        p[[i]]$overview <- 0
      }

      if (length(p[[i]]$objectives) == 0) {
        p[[i]]$objectives <- as.numeric(0)
      } else if (is.na(p[[i]]$objectives)) {
        p[[i]]$objectives <- as.numeric(0)
      }

      if (length(p[[i]]$activity_type) == 0) {
        p[[i]]$activity_type <- as.numeric(0)
      } else if (is.na(p[[i]]$activity_type)) {
        p[[i]]$activity_type <- as.numeric(0)
      }

      if (length(p[[i]]$tags_display) == 0) {
        p[[i]]$tags_display <- as.numeric(0)
      } else if (is.na(p[[i]]$tags_display)) {
        p[[i]]$tags_display <- as.numeric(0)
      }

    }

    lov <- list()
    for (i in 1:length(p))  {
      lov <- c(lov, p[[i]])
    }

    for (i in seq_along(p)) {
      if (length(j[[i]]) == 0) {
        j[[i]] <- 0
      } else if (is.na(j[[i]])) {
        j[[i]]
      }
    }
    pp <- lapply(p, function(x) as.data.frame(x[c("id","objectives", "overview", "section_display", "functional_group", "activity_type", "tags_display")]))

    for (i in seq_along(pp)) {
      pp[[i]]$lead_staff <- j[[i]]
    }
    ppp <- do.call(rbind, pp)

    ## Fixing section_display for sections

    ## Dealing with tag_ids
    tags_id <- lapply(p, function(x) x$tags)
    tag_id <- NULL
    for (i in seq_along(tags_id)) {
      tag_id[[i]] <- paste0(c(unlist(tags_id[[i]])), collapse=",")
    }
    tag_id <- unlist(tag_id)

    sd <- unique(ppp$section_display[which(!(str_count(ppp$section_display,"\\-") == 3))])
    SD <- sd
    # NEXT STEP: IDENTIFY WHICH SECTION_DISPLAY HAS CENTRE FOR SCIENCE ADVICE - SCIENCE
    # AND REMOVE THE -
    bad2 <- sd[which(grepl("Centre for Science Advice -  Maritimes", sd))]
    if (!(length(bad2) == 0)) {
      bad2 <- paste0(sub('-[^-]*$', '', bad2), "Maritimes")
      sd[which(grepl("Centre for Science Advice -  Maritimes", sd))] <- bad2
    }

    # NEXT STEP: IDENTIFY WHICH SECTION_DISPLAY HAS REGIONAL DIRECTOR SCIENCE - OFFICE
    # AND REMOVE THE -
    bad2 <- sd[which(grepl("Regional Director Science - Office", sd))]
    if (!(length(bad2) == 0)) {
      bad2 <- paste0(sub('-[^-]*$', '', bad2), "Office")
      sd[which(grepl("Regional Director Science - Office", sd))] <- bad2
    }

    # NEXT STEP: IDENTIFY WHICH SECTION_DISPLAY HAS ATLANTIC - SUPPORT SECTION
    # AND REMOVE THE -
    bad2 <- sd[which(grepl("Atlantic  - Support Section", sd))]
    if (!(length(bad2) == 0)) {
      bad2 <- paste0(sub('-[^-]*$', '', bad2), "Support Section")
      sd[which(grepl("Atlantic  - Support Section", sd))] <- bad2
    }

    # NEXT STEP: IDENTIFY WHICH SECTION_DISPLAY HAS ATLANTIC - Field Survey
    # AND REMOVE THE -
    bad2 <- sd[which(grepl("Atlantic  - Field Surveys", sd))]
    if (!(length(bad2) == 0)) {
      bad2 <- paste0(sub('-[^-]*$', '', bad2), "Field Surveys")
      sd[which(grepl("Atlantic  - Field Surveys", sd))] <- bad2
    }


    # NEXT STEP: IDENTIFY WHEN DIVISIONS ARE ENTERED TWICE AND REMOVE THE DUPLICATE
    ss <- NULL
    for (i in seq_along(sd)) {
      if (!(grepl("Regional Director Science Office", sd[i]))) {
        ss[[i]] <- unique(strsplit(sd[i], " - ", fixed=TRUE)[[1]])
      } else {
        ss[[i]] <- strsplit(sd[i], " - ", fixed=TRUE)[[1]]

      }
    }
    ss <- unique(ss)
    SS <- NULL
    for (i in seq_along(ss)) {
      SS[[i]] <- toString(paste0(ss[[i]], collapse=" - "))
    }
    SS <- unlist(SS)

    # NEXT STEP: REDEFINE THE SECTION_DISPLAY IN THE OM DATAFRAME
    for (i in seq_along(sd)) {
      ppp$section_display[which(ppp$section_display == SD[i])] <- SS[i]
    }


    lov <- list()
    for (i in 1:length(p))  {
      lov <- c(lov, p[[i]])
    }

    listofvectors <- list()
    for (i in 1:length(t))  {
      listofvectors <- c(listofvectors, t[[i]])
    }

    tt <- lapply(listofvectors, function(x) as.data.frame(x[c("display_name", "id", "project_title", "status_display")]))

    ttt <- do.call(rbind, tt)
  }

  # CONCLUSION: id from ttt is equal to project_year_id in om
  # All om$project_year_id are in ttt$id
  # Adding fiscal year to om data
  om$tags <- 0
  om$tag_id <- 0
  if (type == "om") {
    for (i in seq_along(om$project_year_id)) {
      replace <- ttt$display_name[which(ttt$id == om$project_year_id[i])][1]
      replace2 <- ttt$project_title[which(ttt$id == om$project_year_id[i])][1]
      replace3 <- ttt$status_display[which(ttt$id == om$project_year_id[i])][1]
      replace4 <- ppp$overview[which(ppp$id == om$project_id[i])][1]
      replace5 <- ppp$objectives[which(ppp$id == om$project_id[i])][1]
      replace7 <- ppp$lead_staff[which(ppp$id == om$project_id[i])][1]
      replace8 <- ppp$section_display[which(ppp$id == om$project_id[i])][1]
      replace9 <- ppp$functional_group[which(ppp$id == om$project_id[i])][1]
      replace10 <- ppp$activity_type[which(ppp$id == om$project_id[i])][1]
      replace11 <- unique(ppp$tags_display[which(ppp$id == om$project_id[i])][1])
      replace12 <- unique(unlist(tag_id[which(ppp$id == om$project_id[i])][1]))


      om$fiscal_year[i] <- replace
      om$project_title[i] <- replace2
      om$status[i] <- replace3
      om$overview[i] <- replace4
      om$objectives[i] <- replace5
      om$section_display[i] <- replace8
      om$lead_staff[i] <- replace7
      om$functional_group[i] <- replace9
      om$activity_type[i] <- replace10
      om$tags[i] <- replace11
      om$tag_id[i] <- replace12
    }

    om$activity_type[which(om$activity_type == 1)] <- "Monitoring"
    om$activity_type[which(om$activity_type == 2)] <- "Research"
    om$activity_type[which(om$activity_type == 3)] <- "Other"
    om$activity_type[which(om$activity_type == 4)] <- "Data Management"
    om$activity_type[which(om$activity_type == 5)] <- "Assessment"
  }
  ## Getting divisions ids ("http://dmapps/api/ppt/divisions/")
  division_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/divisions/")]], function(x) x$id))
  full_name <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/divisions/")]], function(x) x$display))
  division_name <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/divisions/")]], function(x) x$name))

  duplicatedNamesDivision <- division_name[duplicated(division_name)]
  DIVISION_NAME <- division_name
  for (i in seq_along(duplicatedNamesDivision)) {
    dn <- duplicatedNamesDivision[i]
    fullname <- full_name[which(division_name == dn)]
    regions <- sub(".*\\(", "(", fullname)
    finalName <- paste0(dn," ", regions)
    DIVISION_NAME[which(division_name == dn)] <- finalName
  }

  ## /sections/
   section_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/sections/")]], function(x) x$id))
   full_name <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/sections/")]], function(x) x$full_name))
   section_name <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/sections/")]], function(x) x$name)) # Aquatic Animal Health, Species at Risk Program
   if (any(section_name == "Centre for Science Advice -  Maritimes")) {
     section_name[which(section_name == "Centre for Science Advice -  Maritimes")] <- "Centre for Science Advice Maritimes"
   }
   if (any(section_name == "Regional Science Director's Office")) {
     section_name[which(section_name == "Regional Science Director's Office")] <- "Regional Director Science Office"
   }
   duplicatedNamesSection <- section_name[duplicated(section_name)]
   SECTION_NAME <- section_name
   for (i in seq_along(duplicatedNamesSection)) {
   dn <- duplicatedNamesSection[i]
   fullname <- full_name[which(section_name == dn)]
   regions <- trimws(unlist(lapply(fullname, function(x) strsplit(x, "-")[[1]][1])), "right")
   finalName <- paste0(dn, " (", regions, ")")
   SECTION_NAME[which(section_name == dn)] <- finalName
   }

   ## /regions/
   region_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/regions/")]], function(x) x$id))
   region_name <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/regions/")]], function(x) x$name))

   ## /funding_sources/
   fs_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/funding-sources/")]], function(x) x$id))
   fs_name <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/funding-sources/")]], function(x) x$name))




  ## LINK 4: GETTING THEME DIFFERENTLY
  # Theme
  # Obtaining OM data from the API
  if (type %in% c("om", "salary")) {
    req <- httr2::request("http://dmapps/api/ppt/themes/")
    # Add custom headers
    req <- req %>% httr2::req_headers("Cookie" = cookie)
    req <- req %>% httr2::req_headers("Accept" = "application/json")

    # Automatically retry if the request fails
    req <- req %>% httr2::req_retry(max_tries = 5)
    # Get the requested data by querying the API
    resp <- httr2::req_perform(req)
    # Read the returned data as a JSON file
    page_data <- httr2::resp_body_json(resp)
    themeNumbers <- unlist(lapply(page_data, function(x) x$id))
    themeNames <- unlist(lapply(page_data, function(x) x$name))

    # Now obtain which project_ids have each theme
    # Obtaining OM data from the API
    project_ids <- NULL
    for (i in seq_along(themeNumbers)) {
      req <- httr2::request(paste0("http://dmapps/api/ppt/project-years/?theme=", themeNumbers[i]))
      # Add custom headers
      req <- req %>% httr2::req_headers("Cookie" = cookie)
      req <- req %>% httr2::req_headers("Accept" = "application/json")
      # Automatically retry if the request fails
      req <- req %>% httr2::req_retry(max_tries = 5)
      # Get the requested data by querying the API
      resp <- httr2::req_perform(req)

      # Read the returned data as a JSON file
      page_data <- httr2::resp_body_json(resp)

      # Create a list to hold the list of full API results
      api_data <- page_data$results

      # Get the information about the next page in the API results
      next_page <- page_data$`next`
      cat(paste0(next_page, '\n'))
      cat(paste0('Number of API records = ', length(api_data), '\n'))

      # Check if the next page is not null (end of pages) before extract the data from
      # next page.
      while (!is.null(next_page)) {
        # Modifying API Call
        req <- httr2::request(next_page)
        # Add custom headers
        req <- req %>% httr2::req_headers("Cookie" = cookie)
        req <- req %>% httr2::req_headers("Accept" = "application/json")
        # Automatically retry if the request fails
        req <- req %>% httr2::req_retry(max_tries = 5)
        # Get the requested data by querying the API
        resp <- httr2::req_perform(req)
        # Read the returned data as a JSON file
        page_data <- httr2::resp_body_json(resp)

        # Add current page data to full list
        api_data <- c(api_data, page_data$results)
        cat(paste0('Number of API records = ', length(api_data), '\n'))

        # Get the information about the next page in the API results
        next_page <- page_data$`next`
        if(debug>0){
          if(exists("debug_page")){
            debug_page <- debug_page-1
          } else {
            debug_page <- debug-1
          }

          if(debug_page==1) {
            debug_page <- debug
            next_page <- NULL
          }
        }
        cat(paste0(next_page, '\n'))
      }
      project_ids[[i]] <- unlist(lapply(api_data, function(x) x$project$id))
    }
    names(project_ids) <- themeNames
  }
  if (type == "om") {
    om$theme <- 0
    for (i in seq_along(project_ids)) {
      for (j in seq_along(project_ids[[i]])) {
        om$theme[which(om$project_id == project_ids[[i]][[j]])] <- themeNames[[i]]
      }
    }

    # Adding in milestones and deliverables
    om$deliverables <- rep(0, length(om$project_id))
    om$milestones <- rep(0, length(om$project_id))
    for (j in seq_along(unique(DDF$title))) {
      value <-
        om[which(om$project_title == unique(DDF$title)[j]), ] # Look at one project
      d <- DDF[which(DDF$title == unique(DDF$title)[j]),]
      for (k in seq_along(unique(d$year))) {
        value2 <-
          value[which(value$fiscal_year == unique(d$year)[k]), ] # Look at one year
        om$deliverables[which(
          om$project_title == unique(value2$project_title) &
            om$fiscal_year == unique(d$year)[k]
        )] <- DELIVERABLES[[j]][k]
        om$milestones[which(
          om$project_title == unique(value2$project_title) &
            om$fiscal_year == unique(d$year)[k]
        )] <- MILESTONES[[j]][k]
      }
    }
    om$milestones[which(om$milestones == "")] <- 0 # This means there was no milestones
    om$deliverables[which(om$deliverables == "")] <- 0
  }

  #if(type == "om") return(om)
  ## WORKING WITH SALARY DATA FRAME
  salaries <- NULL
  load(file.path(system.file(package="dataSPA"),"data", "salaries.rda"))

  ## DEALING WITH "http://dmapps/api/ppt/staff"
  if (type == "salary") {
    api_data3 <- API_DATA[[4]]

    j <- lapply(api_data3, function(x) x[c('id', 'overtime_hours', 'smart_name', 'duration_weeks', 'level_display', 'funding_source_display', 'employee_type_display')])

    for (i in seq_along(j)) {
      if (length(j[[i]]$overtime_hours) == 0) {
        j[[i]]$overtime_hours <- 0
      } else if (!(is.finite(j[[i]]$overtime_hours))) {
        j[[i]]$overtime_hours <- 0
      }

      if (length(j[[i]]$duration_weeks) == 0) {
        j[[i]]$duration_weeks <- 0
      } else if (!(is.finite(j[[i]]$duration_weeks))) {
        j[[i]]$duration_weeks_weeks <- 0
      }
      if (length(j[[i]]$level_display) == 0) {
        j[[i]]$level_display <- 0
      } else if (is.na(j[[i]]$level_display)) {
        j[[i]]$level_display <- 0
      }
    }

    list <- NULL
    for (i in seq_along(j)) {
      #message("This is for ",i)
      list[[i]] <- as.data.frame(j[[i]])
    }

    SAL <- do.call(rbind, list)

    ## Obtain information from project_year_obj

    pyo <- lapply(api_data3, function(x) x$project_year_obj) # list of data frames

    pyoo <- lapply(pyo, function(x) as.data.frame(x[c("id", "project", "display_name", "project_title")]))
    pyo2 <- do.call(rbind, pyoo) # All of the data

    # In conclusion, the id in project_year_obj was project_year_id and the project
    # was the project_id (e.g. 1093)

    SAL$project_year_id <- pyo2$id
    SAL$project_id <- pyo2$project
    SAL$fiscal_year <- pyo2$display_name
    SAL$project_title <- pyo2$project_title

    SAL$level_display[which(SAL$level_display == 0)] <- "PC-02"

    # Make level classification in SAL match salaries format
    # AS-01
    SAL$level_display[which(SAL$level_display == "EL 02")] <- "EL-02"
    SAL$level_display[which(SAL$level_display == "En sur 3")] <- "EN-SUR-03"
    SAL$level_display[which(SAL$level_display == "En sur 5")] <- "EN-SUR-05"
    SAL$level_display[which(SAL$level_display == "RES-01")] <- "SE-RES-01"
    SAL$level_display[which(SAL$level_display == "RES-02")] <- "SE-RES-02"
    SAL$level_display[which(SAL$level_display == "RES-03")] <- "SE-RES-03"
    SAL$level_display[which(SAL$level_display == "RES-04")] <- "SE-RES-04"
    SAL$level_display[which(SAL$level_display == "RES-05")] <- "SE-RES-05"

    level <- sub('-[^-]*$', '', SAL$level_display) # Grabs everything until last -

    levels <- NULL
    for (i in seq_along(level)) {
      if (grepl("RES", level[i])) {
        levels[[i]] <- paste0(level[i], "-")
      } else if (grepl("SUR", level[i])) {
        levels[[i]] <- paste0(level[i], "-")
      } else if (grepl("MAN", level[i])) {
        levels[[i]] <- paste0(level[i], "-")
      } else if (grepl("STS", level[i])) {
        levels[[i]] <- paste0(level[i], "-")
      } else {
        levels[[i]] <- paste0(level[i], "--")
      }
    }
    levels <- unlist(levels) # Some have --, some have - based on excel sheet

    class <- str_extract(SAL$level_display, '\\b\\w+$') # extract everything after -

    fundingLevel <- NULL
    for (i in seq_along(levels)) {
      fundingLevel[[i]] <- paste0(levels[i], class[i])
    }
    fundingLevel <- unlist(fundingLevel)
    SAL$level_display <- fundingLevel

    # Now sub the excel sheet

    SAL$median_salary <- rep(NA, length(SAL$id))
    SAL$salary_per_week <- rep(NA, length(SAL$id))
    SAL$amount_week <- rep(NA, length(SAL$id))
    SAL$amount_overtime <- rep(NA, length(SAL$id))
    SAL$amount_total <- rep(NA, length(SAL$id))
    fundingLevel[which(fundingLevel == "IT--03")] <- "CS--03"

    for (i in seq_along(fundingLevel)) {
      j <- salaries[which(grepl(fundingLevel[i], salaries$`Level and Step`)),] # keeping relevant salaries from excel
      excelyear <- sub('.* ', '', j$`Level and Step`) # extract everything after space in excel for year
      #for (j in seq_along(SAL$fiscal_year)) {
      #message("This is for i = ", i, " ie. fundingLevel= ", fundingLevel[i], " and j = ", j, " with fiscal year = ", SAL$fiscal_year[i])
      sy <- SAL$fiscal_year[i]
      #}
      sy2 <- sub("([^-]*).*", "\\1", sy) # Remove everything before - in SAL year (2021-2022)
      sy3 <- as.character(as.numeric(sy2)- round(as.numeric(sy2),-2))# Extract the last 2 digits
      # if sy3 is in excel year .. do this... if not.. find the closest
      if (sy3 %in% excelyear) {
        jj <- j[which(grepl(sy3, j$`Level and Step`)),] # Takes the appropriate year
      } else {
        mins <- abs(as.numeric(excelyear) - as.numeric(sy3)) # Get the difference between numbers
        dw <- sort(unique(mins))[1] # the difference you want
        # sometimes there may be some years that are the equal difference (ie. 2016-2018 from 2017)
        if (!(length(unique(excelyear[mins]) == 1))) {
          # Multiple years have the same distance
          jj <- j[which(grepl(unique(excelyear[mins])[1], j$`Level and Step`)),]
        } else {
          jj <- j[which(mins == dw),] # Takes the appropriate year
        }
      }
      if (is.na(max(excelyear))) {
        stop("This stopped at ", i, " and fundinglevel =", fundingLevel[i])
      }
      #jj <- j[which(grepl(max(excelyear), j$`Level and Step`)),] # Takes the most recent data
      SAL$median_salary[i] <- as.numeric(median(jj$`Annual Salary`))
      SAL$salary_per_week[i] <- SAL$median_salary[i]/52 # 52 weeks in a year
      SAL$amount_week[i] <- SAL$salary_per_week[i]*SAL$duration_weeks[i]
      SAL$amount_overtime[i] <- (SAL$salary_per_week[i]/37.5)*SAL$overtime_hours[i] # 40 hours in a work week
      SAL$amount_total[i] <- ifelse(SAL$overtime_hours[i] == 0, SAL$amount_week[i], (SAL$amount_week[i] + SAL$amount_overtime[i]))
    }
    bad <- which(grepl("EX", SAL$level_display)) # Removing identified EX
    SAL <- SAL[-bad,]
  }

  if (type == "salary") {
    SAL$theme <- 0
    for (i in seq_along(project_ids)) {
      for (j in seq_along(project_ids[[i]])) {
        SAL$theme[which(SAL$project_id == project_ids[[i]][[j]])] <- themeNames[[i]]
      }
    }
    SAL$activity_type <- 0
    SAL$functional_group <- 0
    SAL$section_display <- 0
    for (i in seq_along(ppp$title)) {
      if (any(SAL$project_title == ppp$title[i])) {
        #SAL$activity_type[which(SAL$project_title == ppp$title[i])] <- ppp$activity_type[i]
        SAL$section_display[which(SAL$project_title == ppp$title[i])] <- ppp$section_display[i]
      }
    }

    SAL$activity_type[which(SAL$activity_type == 1)] <- "Monitoring"
    SAL$activity_type[which(SAL$activity_type == 2)] <- "Research"
    SAL$activity_type[which(SAL$activity_type == 3)] <- "Other"
    SAL$activity_type[which(SAL$activity_type == 4)] <- "Data Management"
    SAL$activity_type[which(SAL$activity_type == 5)] <- "Assessment"
    SAL$activity_type[which(is.na(SAL$activity_type))] <- "0"
    SAL$activity_type[which(is.null(SAL$activity_type))] <- "0"

    ## ADDING IN OVERVIEW/ OBJECTIVES
    # ppp has overview ppp$overview[which(ppp$id == om$project_id[i])][1]
    SAL$overview <- 0
    SAL$objectives <- 0
    SAL$tag <- 0
    SAL$tag_id <- 0
    for (i in seq_along(ppp$id)) {
      SAL$overview[which(SAL$project_id == ppp$id[i])] <- ppp$overview[i]
      SAL$objectives[which(SAL$project_id == ppp$id[i])] <- ppp$objectives[i]
      SAL$activity_type[which(SAL$project_id == ppp$id[i])] <- ppp$activity_type[i]
      SAL$tag_id[which(SAL$project_id == ppp$id[i])] <- tag_id[i]
      SAL$tag[which(SAL$project_id == ppp$id[i])] <- ppp$tags_display[i]
    }
    SAL$activity_type[which(SAL$activity_type == 1)] <- "Monitoring"
    SAL$activity_type[which(SAL$activity_type == 2)] <- "Research"
    SAL$activity_type[which(SAL$activity_type == 3)] <- "Other"
    SAL$activity_type[which(SAL$activity_type == 4)] <- "Data Management"
    SAL$activity_type[which(SAL$activity_type == 5)] <- "Assessment"
    SAL$activity_type[which(is.na(SAL$activity_type))] <- "0"
    SAL$activity_type[which(is.null(SAL$activity_type))] <- "0"

    ## STATUS
    # status display is in ttt

    ## LEAD STAFF
    # j HAS THIS

    SAL$status <- 0
    SAL$lead_staff <- 0
    for (i in seq_along(SAL$smart_name)) {
      replace3 <- ttt$status_display[which(ttt$id == SAL$project_year_id[i])][1]
      replace7 <- ppp$lead_staff[which(ppp$id == SAL$project_id[i])][1]
      replace8 <- ppp$section_display[which(ppp$id == SAL$project_id[i])][1]
      replace2 <- ppp$functional_group[which(ppp$id == SAL$project_id[i])][1]

      SAL$status[i] <- replace3
      SAL$lead_staff[i] <- replace7
      SAL$section_display[i] <- replace8
      SAL$functional_group[i] <- replace2
    }

    ## ADDING IN DELIVERABLES / MILESTONES
    # DDF activities_full
    SAL$deliverables <- 0
    SAL$milestones <- 0

    for (j in seq_along(unique(DDF$title))) {
      value <-
        SAL[which(SAL$project_title == unique(DDF$title)[j]),] # Look at one project
      d <- DDF[which(DDF$title == unique(DDF$title)[j]),]
      if (!(length(value$id) == 0)) {
        for (k in seq_along(unique(d$year))) {
          value2 <-
            value[which(value$fiscal_year == unique(d$year)[k]),] # Look at one year
          SAL$deliverables[which(
            SAL$project_title == unique(value2$project_title) &
              SAL$fiscal_year == unique(d$year)[k]
          )] <- DELIVERABLES[[j]][k]
          SAL$milestones[which(
            SAL$project_title == unique(value2$project_title) &
              SAL$fiscal_year == unique(d$year)[k]
          )] <- MILESTONES[[j]][k]
        }
      }
    }
    ## Dealing with staff_id
    #JAIM
    staff_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/staff")]], function(x) x$id))
    staff_name <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/staff")]], function(x) x$smart_name))
  }


  # Adding in IDs to both om and salary
  if (type == "om") {
    index <- om
  } else if (type == "salary") {
    index <- SAL
  }
  index$section_id <- 0
  # Adding ids in SAL
  # section_id (TO-BE DELETE still a problem)
  for (i in seq_along(section_name)) {
    # Account for duplicated names
    if (section_name[i] %in% duplicatedNamesSection) {
      # find what region is the section display for the identified
      # SAL and see what SECTION_NAME has that region and then fill in relevant section_id
      keep <- which(grepl(section_name[i], index$section_display))
      if (!(length(keep)) == 0) {
      r <- index$section_display[keep]
      r <- trimws(strsplit(r, "-")[[1]][1], "right")
      if (grepl(r, SECTION_NAME[i])) {
        index$section_id[keep] <- section_id[i]
      }
      }
    } else {
      index$section_id[which(grepl(section_name[i], index$section_display))] <- section_id[i]
    }
  }

  # division_id (DONE)
  index$division_id <- 0
  for (i in seq_along(division_name)) {
    # Account for duplicated names
    if (division_name[i] %in% duplicatedNamesDivision) {
      keep <- which(grepl(division_name[i], index$section_display))
      r <- index$section_display[keep]
      r <- trimws(strsplit(r, "-")[[1]][3], "right")
      r <- trimws(r, "left")
      if (grepl(r, DIVISION_NAME[i])) {
        index$division_id[keep] <- division_id[i]
      }
    } else {
      keep <- which(grepl(division_name[i], index$section_display))
      if (!(length(keep) == 0)) {
      index$division_id[keep] <- division_id[i]
      } else {
        # Find if any division names end in (Pacific)
        # Find if there is a ( in the string
        paren <- grepl("\\(", division_name[i])
        if (paren) {
          k1 <- strsplit(division_name[i], " \\(")[[1]][1]
          k2 <- strsplit(division_name[i], " \\(")[[1]][2]
          k2 <- strsplit(k2, "\\)")[[1]][1]
          k3 <- paste0(k1, " \\(", k2, "\\)")
          keep <- which(grepl(k3, index$section_display))
          index$division_id[keep] <- division_id[i]
        }
      }
    }
  }

  # region_id (DONE)
  regions <- unlist(lapply(index$section_display, function(x) trimws(strsplit(x, "-")[[1]][1], "right")))
  index$region_id <- 0
  for (i in seq_along(region_name)) {
    index$region_id[which(region_name[i] == regions)] <- region_id[i]
  }

  # fs_id (DONE)
  index$funding_id <- 0
  for (i in seq_along(fs_name)) {
    keep <- which(grepl(fs_name[i], index$funding_source_display))
    if (!(length(keep) == 0)) {
    index$funding_id[keep] <- fs_id[i]
    } else {
      # Look to see if there is B-base at the end (see issue 1)
      keep <- which(index$funding_source_display == paste0(fs_name[i], " (B-base)"))
      index$funding_id[keep] <- fs_id[i]
    }
  }

  # theme_id
  index$theme_id <- 0
  for (i in seq_along(themeNumbers)) {
    keep <- which(index$theme == themeNames[i])
    if (!(length(keep) == 0)) {
    index$theme_id[keep] <- themeNumbers[i]
    }
  }

  ## tags / tag_id
  project_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/project-years")]], function(x) x$project$id))
  project_title <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/project-years")]], function(x) x$project$title))
  tags_name <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/project-years")]], function(x) x$project$tags_display))

  ## om_id
  if (type == "om") {
  om_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/om-costs")]], function(x) x$id))
  project_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/om-costs")]], function(x) x$project_id))
  project_year_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/om-costs")]], function(x) x$project_year_id))
  om_amount <-  unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/om-costs")]], function(x) x$amount))
  category_display <-  unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/om-costs")]], function(x) x$category_display))


  index$om_id <- 0
  for (p in seq_along(project_id)) {
    index$om_id[which(index$project_id == project_id[p] & index$project_year_id == project_year_id[p] & index$category_display == category_display[p] & index$amount == om_amount[p])] <- om_id[which(project_id == project_id[p] & project_year_id == project_year_id[p] & category_display == category_display[p] & om_amount == om_amount[p])]
  }
  }

  if (type == "salary") {
    staff_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/staff")]], function(x) x$id))
    project_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/staff")]], function(x) x$project_year_obj$project))
    project_year_id <- unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/staff")]], function(x) x$project_year_obj$id))
    name_display <-  unlist(lapply(API_DATA[[which(names(API_DATA) == "http://dmapps/api/ppt/staff")]], function(x) x$smart_name))

    index$staff_id <- 0

    for (p in seq_along(project_id)) {
      index$staff_id[which(index$project_id == project_id[p] & index$project_year_id == project_year_id[p] & index$smart_name == name_display[p])] <- staff_id[which(project_id == project_id[p] & project_year_id == project_year_id[p] & name_display == name_display[p])]
    }

  }



  if (type == "salary") {
    SAL <- index
    #return(SAL)
  } else if (type == "om") {
    om <- index
    #return(om)
  }

  if(keep && type == "om"){
    fn <- file.path(path,"dataSPA_om.rds")
    if(file.exists(fn)){
      date <- as.Date(file.info(fn)$mtime)
      file.rename(fn,
                  file.path(path,paste0("dataSPA_om_",date,".rds")))
      message(paste0("Renaming the pre-existing dataSPA_om.rds to: ",paste0("dataSPA_om_",date,".rds")))
    }

    saveRDS(om,file = fn)
  }

  if(keep && type == "salary") {
    fn <- file.path(path,"dataSPA_SAL.rds")
    if(file.exists(fn)){
      date <- as.Date(file.info(fn)$mtime)
      file.rename(fn,
                  file.path(path,paste0("dataSPA_SAL_",date,".rds")))
      message(paste0("Renaming the pre-existing dataSPA_SAL.rds to: ",paste0("dataSPA_SAL_",date,".rds")))
    }

    saveRDS(SAL,file = fn)
  }

  if (type == "salary") {
    return(SAL)
  } else if (type == "om") {
    return(om)
  }




  if (type == "collaboration") {
    coll <- data.frame(matrix(NA, nrow = length(API_DATA[[1]]), ncol = 5), row.names = NULL)
    names(coll) <- c("project_id", "new_or_existing", "type", "critical", "organization")
    coll$project_id <- lapply(API_DATA[[1]], function(x) x$project_id)
    coll$new_or_existing <- unlist(lapply(API_DATA[[1]], function(x) x$new_or_existing_display))
    coll$type <- unlist(lapply(API_DATA[[1]], function(x) x$type_display))
    coll$critical <- unlist(lapply(API_DATA[[1]], function(x) x$critical))
    for (i in seq_along(API_DATA[[i]])) {
      if (is.null(API_DATA[[1]][[i]]$organization)) {
        API_DATA[[1]][[i]]$organization <- 0
      }
    }
    coll$organization <- unlist(lapply(API_DATA[[1]], function(x) x$organization))
    return(coll)
  }

  if (type == "statusReport") {
    st <- data.frame(matrix(NA, nrow = length(API_DATA[[1]]), ncol = 15), row.names = NULL)
    names(st) <- c("project_id","target_completion_date_display", "status_display", "supporting_resources",
                   "major_accomplishments", "major_issues", "excess_funds_comment", "excess_funds_amt", "excess_funds",
                   "insufficient_funds", "insufficient_funds_amt", "insufficient_funds_comment",
                   "rationale_for_modified_completion_date", "general_comment", "project_year")


    for (i in seq_along(API_DATA[[1]])) {
      for (j in seq_along(names(st))) {
        parameter <- API_DATA[[1]][[i]][[names(st[j])]]
        if (is.null(parameter)) {
          API_DATA[[1]][[i]][[names(st[j])]] <- 0
        }
        st[[names(st[j])]][[i]] <- API_DATA[[1]][[i]][[names(st[j])]]
      }
    }
    st$general_comment <- unlist(lapply(API_DATA[[1]], function(x) x$general_comment))
    st$rationale_for_modified_completion_date <- unlist(lapply(API_DATA[[1]], function(x) x$rationale_for_modified_completion_date))
    # Now add fiscal year in from the project_year id

    # Dealing with [[2]]
    # Dealing with project
    df <- lapply(API_DATA[[2]], function(x) x$project)

    for (i in seq_along(df)) {
      for (j in seq_along(names(df[[1]]))) {
        parameter <- df[[i]][[names(df[[1]][j])]]
        if (is.null(parameter)) {
          df[[i]][[names(df[[1]][j])]] <- 0
        }
      }
    }
    # Turning into data frames for project_id
    DF <- data.frame(matrix(NA, nrow = length(API_DATA[[2]]), ncol = 2), row.names = NULL)
    names(DF) <- c("title", "project_id")
    DF$title <- unlist(lapply(df, function(x) x$title))
    DF$project_id <- unlist(lapply(df, function(x) x$id))

    # Dealing with year
    df2 <- lapply(API_DATA[[2]], function(x) x$project$years)
    df22 <- do.call(c, df2)
    for (i in seq_along(df22)) {
      for (j in seq_along(names(df22[[1]]))) {
        parameter <- df22[[i]][[names(df22[[1]][j])]]
        if (is.null(parameter)) {
          df22[[i]][[names(df22[[1]][j])]] <- 0
        }
      }
    }
    DF2 <- data.frame(matrix(NA, nrow = length(df22), ncol = 4), row.names = NULL)
    names(DF2) <- c("year_id", "fiscal_year", "project_title", "project_id")
    DF2$year_id <- unlist(lapply(df22, function(x) x$id))
    DF2$project_title <- unlist(lapply(df22, function(x) x$project_title))
    DF2$fiscal_year <- unlist(lapply(df22, function(x) x$display_name))
    DF2$project_id <- unlist(lapply(df22, function(x) x$project))

    st_id <- st$project_id
    st$project_title <- 0
    st$fiscal_year <- 0
    for (i in seq_along(st_id)) {
      st$project_title[i] <- unique(DF$title[which(DF$project_id == st_id[i])])
    }
    for (i in seq_along(st_id)) {
      st$fiscal_year[i] <- unique(DF2$fiscal_year[which(DF2$year_id == st$project_year[i])])

    }

    return(st)
  }
}
