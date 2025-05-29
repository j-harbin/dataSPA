link <- "https://dmapps/api/ppt/om-allocations/"
#link <- "https://dmapps/api/ppt/om-costs/"


req <- httr2::request(link)

# Add custom headers
req <- req |> httr2::req_headers("Cookie" = cookie)
req <- req |> httr2::req_headers("Accept" = "application/json")

# Automatically retry if the request fails
req <- req |> httr2::req_retry(max_tries = 5)

# Get the requested data by querying the API
resp <- try(httr2::req_perform(req), silent=TRUE)
page_data <- httr2::resp_body_json(resp)


next_page <- page_data$`next`
cat(paste0(next_page, '\n'))

cat(paste0('Number of API records = ', length(api_data), '\n'))

# Check if the next page is not null (end of pages) before extract the data from
# next page.
api_data <- page_data$results

while (!is.null(next_page)) {

# Modifying API Call
req <- httr2::request(next_page)
# Add custom headers
req <- req |> httr2::req_headers("Cookie" = cookie)
req <- req |> httr2::req_headers("Accept" = "application/json")
# Automatically retry if the request fails
req <- req |> httr2::req_retry(max_tries = 5)
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



# om_data
# allocated_data

oo <- do.call(rbind, lapply(om_data, function(x) {
  x[sapply(x, is.null)] <- NA  # Replace NULLs with NA
  as.data.frame(x, stringsAsFactors = FALSE)
}))


for (i in seq_along(allocated_data)) {
  message(paste0(length(unique(om$project_id[which(oo$project_year_id == allocated_data[[i]]$project_year_id)]))), " for ", i)
}

# Conclusion: Using the above for loop I am able to get to the project level using the project year id (they are unique to projects)
# I can get to the level of detail that tells us how much investment fer funding type was allocated to certain projects.
# I cannot tell where this funding went.
# I will now instead being using the allocated_amount from the original API I am pulling. I will check to see if these numbers align.

