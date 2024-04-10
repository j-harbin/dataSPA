library(dataSPA)
cookie <- "csrftoken=AiS7y5VkbQfiT7zoW2bTSbDq7WNuwDJY; sessionid=0a5md5v1oh1vxi0szflgvlo3c6r79ixg"
# Note for salaryDat, omDat, and om_date I made the age 100 to avoid getting new data.
# You may want to change this to get new data, but if you do I would recommend
# Changing it for all three.
omDat <- getData(type = "om", cookie = cookie, age=100,path = "./data")
salaryDat <- getData(type = "salary", cookie = cookie, age=100, path = "./data")
om_date <- getData(type= "om_date", cookie = cookie, age=100,path = "./data")

# Subset om and salary only for PSSI (b-base) funded projects
om <- omDat[which(omDat$funding_source_display == "PSSI (B-base)"),]
sal <- salaryDat[which(salaryDat$funding_source_display == "PSSI (B-base)"),]

# Subset om and salary only for the last fiscal year (23-24)
om <- om[which(om$fiscal_year == "2023-2024"),]
sal <- sal[which(sal$fiscal_year == "2023-2024"),]

# Subsetting only for Approved projects
om <- om[which(om$status == "Approved"),]
sal <- sal[which(sal$status == "Approved"),]


IDS <- unique(c(unique(om$project_id), unique(sal$project_id))) # getting the unique project IDS

# Create financial reports for projects funded by PSSI (b-base)
createReport(om= om, salary=sal, id = IDS,
             cookie = cookie, destdir = "./reports", path="./data")


# Status Reports
statusReport <- getData(type="statusReport", cookie = cookie, path = "./data")
sr <- statusReport[which(statusReport$fiscal_year == "2023-2024"),]
sr <- sr[which(sr$project_id %in% IDS),]

createReport(statusReport = sr, cookie=cookie, destdir="./reports", path="./data")

