# om built in
if (!FALSE) {
  #library(dataSPA)
  namesOM <- c("project_id","category_display","project_year_id",
               "amount","funding_source_display", "id", "category_type", "description", "tags", "tag_id", "fiscal_year",
               "project_title","status", "overview", "objectives", "section_display", "lead_staff","functional_group","activity_type", "theme", "deliverables", "milestones",
                "section_id","division_id", "region_id", "funding_id",
               "theme_id","om_id")

  om <- data.frame(matrix(NA, nrow = 10, ncol = length(namesOM)))
  names(om) <- namesOM
  om$project_id <- rep(1234, length(om$project_id))
  om$category_display <- c("Field Travel","Training, domestic conferences",
  "Field Equipment", "IM/IT - computers, hardware, software",
  "Field","Field", "Contracts", "Translation","Vessels, Boats", "Vessels, Boats")
  om$project_year_id <- 111
  om$section_display <- rep("Maritimes - Science - Ocean and Ecosystem Science Division - Test Section")
  om$amount <- c(15000, 1000, 4000, 1000, 15000, 3000, 65000, 1000, 430000, 70000)
  om$funding_source_display <- c("Unspecified (C-base)", "Unspecified (C-base)", "Unspecified (C-base)", "Unspecified (C-base)",
                                 "Unspecified (C-base)", "Unspecified (C-base)", "Unspecified (C-base)", "Unspecified (C-base)",
                                 "Unspecified (C-base)", "NCP (A-base)")
  om$category_type <- c("Travel","Travel","Equipment Purchase",
                         "Equipment Purchase", "Material and Supplies","Material and Supplies",
                         "Contracts, Leases, Services", "Contracts, Leases, Services", "Contracts, Leases, Services",
                         "Contracts, Leases, Services")
  om$fiscal_year <- "2014-2015"
  om$functional_group <- rep("Test functional group", length(om$project_id))
  om$theme <- rep("Test theme", length(om$project_id))

  for (i in seq_along(om$id)) {
    om$id[i] <- i
  }
  om$activity_type <- "Research"
  om$project_title <- "Fake project"
  om$status <- "Approved"
  om$overview <- "This project is an example for dataSPA test case"
  om$objectives <- "To check quality of our code"
  om$deliverables <- "Validate code"
  om$description <- "Used to validate our code"
  om$lead_staff <- "John Smith"
  om$milestones <- "Create package"
  om$section_id <- 1:10
  om$division_id <- 1:10
  om$region_id <- 1:10
  om$funding_id <- 1:10
  om$theme_id <- 1:10
  om$tag_id <- 1:10
  om$tags <- "test"
  om$om_id <- 1:10

  save(om, file="om.rda")
  tools::resaveRdaFiles('om.rda')
}


