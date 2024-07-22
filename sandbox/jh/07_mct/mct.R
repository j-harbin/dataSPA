library(dataSPA)
om <- getData(type="om", cookie="hi", age=1000)
sal <- getData(type="om", cookie="hi", age=1000)
om <- om[which(om$funding_source_display == "MCT (Science) (B-base)" & om$activity_type == "Monitoring" & grepl("Maritimes", om$section_display)),]
sal <- sal[which(sal$funding_source_display == "MCT (Science) (B-base)" & sal$activity_type == "Monitoring" & grepl("Maritimes", sal$section_display)),]

allProjects <- unique(c(om$project_id, sal$project_id)) #23
leadStaff <- unique(c(om$lead_staff, sal$lead_staff))
ls <- unique(sub(",.*", "", leadStaff))

projects <- NULL
for (i in seq_along(ls)) {
  keep <- which(sub(",.*", "", om$lead_staff) == ls[i])
  project_id <- unique(om$project_id[keep])
  project_title <- unique(om$project_title[keep])
  projects[[i]] <- data.frame("project"=project_id, "title"=project_title)
}
names(projects) <- ls
