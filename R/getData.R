#' Get required data from the PPT API
#'
#' If `type` is `om`, this function extracts data from the
#' om-costs project planning tool (PPT) API (http://dmapps/api/ppt/om-costs/)
#' and combines it with the fiscal year from the project_years API
#' (http://dmapps/api/ppt/). If `type` is `salary`, this function
#' obtains the data from the staff API (http://dmapps/api/ppt/staff/)
#' and combines it with information from a Human Resources (HR) spreadsheet.
#'
#' @param type the type of data that is wished to be extracted
#' (either `om`, `om_date`, `salary`, or `salary_date`). The types that end in `_date` will return the date of creation of a locally stored file.
#'
#' @param cookie a sessionid and csrftoken from a Department of
#' Fisheries and Oceans Canada (DFO) employee in the following
#' format: csrftoken=YOURTOKEN; sessionid=YOURSESSIONID
#'
#' @param keep logical value to optionally keep `om` data on the hard disk. Default is FALSE and will not save data on the hard disk. Value of TRUE will save `om` to disk in `path` unless there is an `om` file already in `path` that was created more recently than `age` (number of days)
#'
#' @param age maximum age in number of days that a file may be loaded. Set to 0 to download new data every time.
#'
#' @param path path to save file. Default is in the shared IN folder
#'
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @return A) If `type` is `om`: a data frame containing the following
#'  information: project_id, category_display, project_year_id,        "amount"
#' funding_source_display, id, category_type, description, fiscal_year,
#'project_title, status,overview, objectives, deliverables. B) If `type` is `salary`:
#'  a data frame containing the following information: id, overtime_hours,
#'  smart_name, duration_weeks, level_display, funding_source_display,
#'  employee_type_display, project_year_id, project_id, fiscal_year,
#'  project_title, median_salary, salary_per_week, amount_week,
#'  amount_overtime, amount_total. C) If `type` is `om_date` or
#'  `salary_date` a string indicating the date in which the relevant
#'  data was stored locally
#'
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
    stop("Must provide a type argument of either 'om' or 'salary'")
  }

  if (is.null(cookie)) {
    stop("Must provide a cookie argument in the following format:csrftoken=YOURTOKEN; sessionid=YOURSESSIONID")
  }

  if (!(type %in% c("om", "salary", "om_date", "salary_date"))) {
    stop("Must provide a type argument of either 'om', 'salary', 'om_date', or 'salary_date'")
  }
  if (debug > 0) {
    message("type = ", type)
  }

  if (type %in% c("om","om_date")) {
    if(age>0){
      # Look for files in path, only return the most recent file the matches pattern
      fn <- file.path(path,"dataSPA_om.rds")
      if(file.exists(fn)){
        # Load file if more recent than `keep` days old
        d <- as.Date(file.info(fn)$mtime)
        if((Sys.Date()-d)<age){
          if(type=="om"){
            om <- readRDS(file = fn)
            #message(paste0("loading file from disk(",fn,")"))
            return(om)
          } else if(type=="om_date"){
            #message(paste0("returning date from file on disk(",fn,")"))
            return(d)
          }

        }
      }  else if(type=="om_date"){
        stop(paste("File (",fn,") does not exist. User must first save om type. See examples in ?getData for how to fix this."))
      }
    }
    # Obtaining OM data from the API
    req <- httr2::request("http://dmapps/api/ppt/om-costs")

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
      cat(paste0(next_page, '\n'))

    }

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
      #message("This is for ", i)
      p[[i]] <- as.data.frame(api_data[[i]][c("project_id", "category_display", "project_year_id", "amount", "funding_source_display", "id", "category_type", "description")])
    }

    om <- do.call(rbind, p) # This is the same as om. It does not have fiscal year.

    ## 2. OBTAIN FISCAL YEAR DATA FROM API

    # Initializing API Call
    req2 <- httr2::request("http://dmapps/api/ppt/project-years")

    # Add custom headers
    req2 <- req2 %>% httr2::req_headers("Cookie" = cookie)
    req2 <- req2 %>% httr2::req_headers("Accept" = "application/json")

    # Automatically retry if the request fails
    req2 <- req2 %>% httr2::req_retry(max_tries = 5)

    # Get the requested data by querying the API
    resp2 <- httr2::req_perform(req2)

    # Read the returned data as a JSON file
    page_data2 <- httr2::resp_body_json(resp2)

    # Create a list to hold the list of full API results
    api_data2 <- page_data2$results

    # Get the information about the next page in the API results
    next_page2 <- page_data2$`next`
    cat(paste0(next_page2, '\n'))

    cat(paste0('Number of API records = ', length(api_data2), '\n'))

    # Check if the next page is not null (end of pages) before extract the data from
    # next page.
    while (!is.null(next_page2)) {

      # Modifying API Call
      req2 <- httr2::request(next_page2)

      # Add custom headers
      req2 <- req2 %>% httr2::req_headers("Cookie" = cookie)
      req2 <- req2 %>% httr2::req_headers("Accept" = "application/json")

      # Automatically retry if the request fails
      req2 <- req2 %>% httr2::req_retry(max_tries = 5)

      # Get the requested data by querying the API
      resp2 <- httr2::req_perform(req2)

      # Read the returned data as a JSON file
      page_data2 <- httr2::resp_body_json(resp2)

      # Add current page data to full list
      api_data2 <- c(api_data2, page_data2$results)

      cat(paste0('Number of API records = ', length(api_data2), '\n'))

      # Get the information about the next page in the API results
      next_page2 <- page_data2$`next`
      cat(paste0(next_page2, '\n'))

    }

    #  OBTAINING MILESTONE AND DELIVERABLE ACTIVITIES DATA
    # Initializing API Call
    req3 <- httr2::request("http://dmapps/api/ppt/activities-full/")

    # Add custom headers
    req3 <- req3 %>% httr2::req_headers("Cookie" = cookie)
    req3 <- req3 %>% httr2::req_headers("Accept" = "application/json")

    # Automatically retry if the request fails
    req3 <- req3 %>% httr2::req_retry(max_tries = 5)

    # Get the requested data by querying the API
    resp3 <- httr2::req_perform(req3)

    # Read the returned data as a JSON file
    page_data3 <- httr2::resp_body_json(resp3)

    # Create a list to hold the list of full API results
    api_data3 <- page_data3$results

    # Get the information about the next page in the API results
    next_page3 <- page_data3$`next`
    cat(paste0(next_page3, '\n'))

    cat(paste0('Number of API records = ', length(api_data3), '\n'))

    # Check if the next page is not null (end of pages) before extract the data from
    # next page.
    while (!is.null(next_page3)) {

      # Modifying API Call
      req3 <- httr2::request(next_page3)

      # Add custom headers
      req3 <- req3 %>% httr2::req_headers("Cookie" = cookie)
      req3 <- req3 %>% httr2::req_headers("Accept" = "application/json")

      # Automatically retry if the request fails
      req3 <- req3 %>% httr2::req_retry(max_tries = 5)

      # Get the requested data by querying the API
      resp3 <- httr2::req_perform(req3)

      # Read the returned data as a JSON file
      page_data3 <- httr2::resp_body_json(resp3)

      # Add current page data to full list
      api_data3 <- c(api_data3, page_data3$results)

      cat(paste0('Number of API records = ', length(api_data3), '\n'))

      # Get the information about the next page in the API results
      next_page3 <- page_data3$`next`
      cat(paste0(next_page3, '\n'))

    }
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

    #  if (length(p[[i]]$section_display == 0)) {
    #    p[[i]]$section_display <- as.numeric(0)
    #  } else if (is.na(p[[i]]$section_display)) {
    #    p[[i]]$section_display <- as.numeric(0)
    #  }


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

    pp <- lapply(p, function(x) as.data.frame(x[c("id","objectives", "overview", "section_display", "functional_group")]))

    for (i in seq_along(pp)) {
      pp[[i]]$lead_staff <- j[[i]]
    }
    ppp <- do.call(rbind, pp)

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

    # CONCLUSION: id from ttt is equal to project_year_id in om
    # All om$project_year_id are in ttt$id
    # Adding fiscal year to om data
    for (i in seq_along(om$project_year_id)) {
      replace <- ttt$display_name[which(ttt$id == om$project_year_id[i])][1]
      replace2 <- ttt$project_title[which(ttt$id == om$project_year_id[i])][1]
      replace3 <- ttt$status_display[which(ttt$id == om$project_year_id[i])][1]
      replace4 <- ppp$overview[which(ppp$id == om$project_id[i])][1]
      replace5 <- ppp$objectives[which(ppp$id == om$project_id[i])][1]
      replace7 <- ppp$lead_staff[which(ppp$id == om$project_id[i])][1]
      replace8 <- ppp$section_display[which(ppp$id == om$project_id[i])][1]
      replace9 <- ppp$functional_group[which(ppp$id == om$project_id[i])][1]
      om$fiscal_year[i] <- replace
      om$project_title[i] <- replace2
      om$status[i] <- replace3
      om$overview[i] <- replace4
      om$objectives[i] <- replace5
      om$section_display[i] <- replace8
      #om$deliverables[i] <- replace6
      om$lead_staff[i] <- replace7
      om$functional_group[i] <- replace9

    }
    # Theme

    # Obtaining OM data from the API
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
        cat(paste0(next_page, '\n'))
      }
      project_ids[[i]] <- unlist(lapply(api_data, function(x) x$project$id))



    }



    names(project_ids) <- themeNames
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

    if(keep){
      fn <- file.path(path,"dataSPA_om.rds")
      if(file.exists(fn)){
        date <- as.Date(file.info(fn)$mtime)
        file.rename(fn,
                    file.path(path,paste0("dataSPA_om_",date,".rds")))
        message(paste0("Renaming the pre-existing dataSPA_om.rds to: ",paste0("dataSPA_om_",date,".rds")))
      }

      saveRDS(om,file = fn)
    }

    return(om)
  } else if (type %in% c("salary","salary_date")) {
    salaries <- NULL
    load(file.path(system.file(package="dataSPA"),"data", "salaries.rda"))

    if(age>0){
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

    # Initializing API Call
    req3 <- request("http://dmapps/api/ppt/staff")

    # Add custom headers
    req3 <- req3 %>% req_headers("Cookie" = cookie)
    req3 <- req3 %>% req_headers("Accept" = "application/json")

    # Automatically retry if the request fails
    req3 <- req3 %>% req_retry(max_tries = 5)

    # Get the requested data by querying the API
    resp3 <- req_perform(req3)

    # Read the returned data as a JSON file
    page_data3 <- resp_body_json(resp3)

    # Create a list to hold the list of full API results
    api_data3 <- page_data3$results

    # Get the information about the next page in the API results
    next_page3 <- page_data3$`next`
    cat(paste0(next_page3, '\n'))

    cat(paste0('Number of API records = ', length(api_data3), '\n'))

    # Check if the next page is not null (end of pages) before extract the data from
    # next page.
    while (!is.null(next_page3)) {

      # Modifying API Call
      req3 <- request(next_page3)

      # Add custom headers
      req3 <- req3 %>% req_headers("Cookie" = cookie)
      req3 <- req3 %>% req_headers("Accept" = "application/json")

      # Automatically retry if the request fails
      req3 <- req3 %>% req_retry(max_tries = 5)

      # Get the requested data by querying the API
      resp3 <- req_perform(req3)

      # Read the returned data as a JSON file
      page_data3 <- resp_body_json(resp3)

      # Add current page data to full list
      api_data3 <- c(api_data3, page_data3$results)

      cat(paste0('Number of API records = ', length(api_data3), '\n'))

      # Get the information about the next page in the API results
      next_page3 <- page_data3$`next`
      cat(paste0(next_page3, '\n'))
    }

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
    # Theme

    # Obtaining OM data from the API
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
        cat(paste0(next_page, '\n'))
      }
      project_ids[[i]] <- unlist(lapply(api_data, function(x) x$project$id))



    }



    names(project_ids) <- themeNames







    SAL$theme <- 0
    for (i in seq_along(project_ids)) {
      for (j in seq_along(project_ids[[i]])) {
        SAL$theme[which(SAL$project_id == project_ids[[i]][[j]])] <- themeNames[[i]]
      }
    }
    if(keep){
      fn <- file.path(path,"dataSPA_SAL.rds")
      if(file.exists(fn)){
        date <- as.Date(file.info(fn)$mtime)
        file.rename(fn,
                    file.path(path,paste0("dataSPA_SAL_",date,".rds")))
        message(paste0("Renaming the pre-existing dataSPA_SAL.rds to: ",paste0("dataSPA_SAL_",date,".rds")))
      }

      saveRDS(SAL,file = fn)
    }
    return(SAL)

  }
}
