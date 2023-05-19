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
#' (either `om` or `salary`)
#'
#' @param cookie a sessionid and csrftoken from a Department of
#' Fisheries and Oceans Canada (DFO) employee in the following
#' format: csrftoken=YOURTOKEN; sessionid=YOURSESSIONID
#'
#' @param salaries a built in data frame obtained from the `dataSPA`
#' package that contains information about pay levels for
#' different DFO positions
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
#'  amount_overtime, amount_total.
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
#' data <- getData(type="salary", cookie=cookie, salaries=salaries)
#' head(data,2)
#' }
#' @export

getData <- function(type=NULL, cookie=NULL, debug=0, salaries=NULL) {
  if (is.null(type)) {
    stop("Must provide a type argument of either 'om' or 'salary'")
  }

  if (is.null(cookie)) {
    stop("Must provide a cookie argument in the following format:csrftoken=YOURTOKEN; sessionid=YOURSESSIONID")
  }

  if (!(type %in% c("om", "salary"))) {
    stop("Must provide a type argument of either 'om' or 'salary'")
  }
  if (debug > 0) {
    message("type = ", type)
  }

  if (type == "om") {
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

    t <- lapply(api_data2, function(x) x$project$years)

    # Add objectives and overview
    p <- lapply(api_data2, function(x) x$project)

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

    }

    lov <- list()
    for (i in 1:length(p))  {
      lov <- c(lov, p[[i]])
    }

    pp <- lapply(p, function(x) as.data.frame(x[c("id","objectives", "overview")]))

    ppp <- do.call(rbind, pp)
    # JAIM test

    # Add deliverables

    for (i in seq_along(api_data2)) {
      if (length(api_data2[[i]]$deliverables) == 0) {
        api_data2[[i]]$deliverables <- 0
      } else if (is.na(api_data2[[i]]$deliverables)) {
        api_data2[[i]]$deliverables <- 0
      }
    }

    d <- unlist(lapply(api_data2, function(x) x$deliverables))
    dd <- unlist(lapply(api_data2, function(x) x$project$id))
    D <- data.frame(matrix(NA, nrow = length(d), ncol = 1), header=TRUE)
    names(D) <- c("deliverables", "project_id")
    D$deliverables <- d
    D$project_id <- dd

    # END TEST

    listofvectors <- list()
    for (i in 1:length(t))  {
      listofvectors <- c(listofvectors, t[[i]])
    }

    tt <- lapply(listofvectors, function(x) as.data.frame(x[c("display_name", "id", "project_title", "status_display")]))

    ttt <- do.call(rbind, tt)

    # Testing snow crab

    # CONCLUSION: id from ttt is equal to project_year_id in om
    # All om$project_year_id are in ttt$id

    # Adding fiscal year to om data
    for (i in seq_along(om$project_year_id)) {
      replace <- ttt$display_name[which(ttt$id == om$project_year_id[i])][1]
      replace2 <- ttt$project_title[which(ttt$id == om$project_year_id[i])][1]
      replace3 <- ttt$status_display[which(ttt$id == om$project_year_id[i])][1]
      replace4 <- ppp$overview[which(ppp$id == om$project_id[i])][1]
      replace5 <- ppp$objectives[which(ppp$id == om$project_id[i])][1]
      replace6 <- D$deliverables[which(D$project_id == om$project_id[i])][1]

      #message("replace is ", replace, " for ", i)
      om$fiscal_year[i] <- replace
      om$project_title[i] <- replace2
      om$status[i] <- replace3
      om$overview[i] <- replace4
      om$objectives[i] <- replace5
      om$deliverables[i] <- replace6

    }
    return(om)
  } else if (type == "salary") {
    if (is.null(salaries)) {
      stop("Must load built in data-set using data(salaries) and set salaries=salaries (see examples))")
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

    # FIXME: TEMPORARY. If no level funding available, they are treated like PC2
    SAL$level_display[which(SAL$level_display == 0)] <- "PC-02"

    #salaries <- read_excel(payscale) # Trish's table

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

    # Now sub the excel sheet

    SAL$median_salary <- rep(NA, length(SAL$id))
    SAL$salary_per_week <- rep(NA, length(SAL$id))
    SAL$amount_week <- rep(NA, length(SAL$id))
    SAL$amount_overtime <- rep(NA, length(SAL$id))
    SAL$amount_total <- rep(NA, length(SAL$id))


    for (i in seq_along(fundingLevel)) {
      j <- salaries[which(grepl(fundingLevel[i], salaries$`Level and Step`)),] # keeping relevant salaries from excel
      excelyear <- sub('.* ', '', j$`Level and Step`) # extract everything after space in excel for year
      if (is.na(max(excelyear))) {
        stop("This stopped at ", i, " and fundinglevel =", fundingLevel[i])
      }
      jj <- j[which(grepl(max(excelyear), j$`Level and Step`)),] # Takes the most recent data
      SAL$median_salary[i] <- as.numeric(median(jj$`Annual Salary`))
      SAL$salary_per_week[i] <- SAL$median_salary[i]/52 # 52 weeks in a year
      SAL$amount_week[i] <- SAL$salary_per_week[i]*SAL$duration_weeks[i]
      SAL$amount_overtime[i] <- (SAL$salary_per_week[i]/37.5)*SAL$overtime_hours[i] # 40 hours in a work week
      SAL$amount_total[i] <- ifelse(SAL$overtime_hours[i] == 0, SAL$amount_week[i], (SAL$amount_week[i] + SAL$amount_overtime[i]))
    }
    return(SAL)

  }
}
