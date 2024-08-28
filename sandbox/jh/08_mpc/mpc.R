library(dataSPA)
om <- getData(type="om", cookie="hi", age=1000)
sal <- getData(type="om", cookie="hi", age=1000)
om <- om[which(om$functional_group == "Marine Conservation Targets"),]
sal <- sal[which(sal$functional_group == "Marine Conservation Targets"),]
df <- data.frame(project_id=unique(c(om$project_id, sal$project_id)))
df$project_title <- 0
df$lead_staff <- 0
for (i in seq_along(df$project_id)) {
  df$project_title[i] <- unique(om$project_title[which(om$project_id == df$project_id[i])])
  df$lead_staff[i] <- unique(om$lead_staff[which(om$project_id == df$project_id[i])])
}
write.csv(df, file="mpc.csv")
