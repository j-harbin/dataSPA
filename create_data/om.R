# om built in

if (!FALSE) {
  library(dataSPA)

  namesOM <- c("project_id","category_display","project_year_id",
               "amount","funding_source_display", "id", "category_type", "description","fiscal_year",
               "project_title","status", "overview", "objectives", "deliverables")

  om <- data.frame(matrix(NA, nrow = 10, ncol = length(namesOM)))
  names(om) <- namesOM
  om$project_id <- rep(1234, length(om$project_id))
  om$category_display[1:3] <- "Field Travel"
  om$category_display[4:5] <- "Training, domestic conferences"
  om$category_display[6:9] <- "Vessels, Boats"
  om$category_display[9:10] <- "Lab Equipment"
  om$project_year_id[c(1,4,8,9)] <- 123
  om$project_year_id[c(2,5,8,10)] <- 456
  om$project_year_id[c(3,6,7)] <- 789
  om$amount[1:3] <- 100000
  om$amount[4:7] <- 500000
  om$amount[7:10] <- 40000
  om$funding_source_display[c(1:4)] <- "Core (A-base)"
  om$funding_source_display[c(4:7)] <- "NCP (Oceans) (B-base)"
  om$funding_source_display[c(8:10)] <- "CRSB (B-base)"
  for (i in seq_along(om$id)) {
    om$id[i] <- i
  }
  om$category_type[1:3] <- "Travel"
  om$category_type[4:5] <- "Material and Supplies"
  om$category_type[6:9] <- "Equipment Purchase"
  om$category_type[9:10] <- "Equipment Purchase"

  om$fiscal_year[c(1,4,8,9)] <- "2021-2022"
  om$fiscal_year[c(2,5,8,10)] <- "2022-2023"
  om$fiscal_year[c(3,6,7)] <- "2023-2024"
  om$project_title <- "Example Project"
  om$status <- "Approved"
  om$overview <- "This project is an example for dataSPA test case"
  om$objectives <- "To check quality of our code"
  om$deliverables <- "Validated code"
  om$description <- "Used to validate our code"

  save(om, file="om.rda")
  tools::resaveRdaFiles('om.rda')
}


