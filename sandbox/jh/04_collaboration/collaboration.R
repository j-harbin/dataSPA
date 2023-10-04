library(dataSPA)
# Step 1: Obtain information from the PPT
# Must define cookie information
om <- getData(type="om", cookie=cookie, age=100)
salary <- getData(type="salary", cookie=cookie, age=100)

collaboration <- getData(type="collaboration", cookie=cookie, age=100)

# Step 2: Determine which collaborative organizations
# pertain to Dal, Dalhousie, OTN, and Ocean Tracking Network

colab2 <- collaboration[which(grepl("Dal", collaboration$organization, ignore.case = TRUE) | grepl("OTN", collaboration$organization, ignore.case = TRUE) |
                                   grepl("Ocean Tracking Network", collaboration$organization, ignore.case = TRUE)),]

# Step 3: Make data frame that only has project_id and organization
colab3 <- data.frame(matrix(NA, nrow = length(colab2$project_id), ncol = 3), row.names = NULL)
names(colab3) <- c("project_id", "title", "organization")
colab3$project_id <- colab2$project_id
colab3$organization <- colab2$organization

# Step 4: Obtain project_id title from the om data frame

for (i in seq_along(colab3$project_id)) {
  if (any(om$project_id == colab3$project_id[i])) {
  title <- unique(om$project_title[which(om$project_id == colab3$project_id[i])])
  } else {
    title <- unique(salary$project_title[which(salary$project_id == colab3$project_id[i])])
  }
  colab3$title[i] <- title
}

# Step 5: Remove duplication (likely from different years of a project)
# Find duplicated rows
duplicated_rows <- duplicated(colab3)

# Remove duplicated rows, keeping the first occurrence
df <- colab3[!duplicated_rows, ]
df2 = data.frame(lapply(df, as.character), stringsAsFactors=FALSE)

write.csv(x=df2, file="collaborations.csv")


