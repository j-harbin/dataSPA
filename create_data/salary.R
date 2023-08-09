if (!FALSE) {
namesSal <- c("id","overtime_hours","smart_name","duration_weeks",
"level_display","funding_source_display","employee_type_display",
"project_year_id","project_id","fiscal_year","project_title","median_salary",
"salary_per_week", "amount_week", "amount_overtime", "amount_total", "theme", "activity_type")


salary <- data.frame(matrix(NA, nrow = 8, ncol = length(namesSal)))
names(salary) <- namesSal
salary$id <- 1:8
salary$overtime_hours <- c(0,550,550, 0,225,0,0,0)
salary$smart_name <- c("John1", "John2", "John3", "John4", "Jane1", "Jane2", "Jane3", "Jane4")
salary$duration_weeks <- c(10.0, 30.0, 30.0,  0.1, 15.0 ,1.0,  0.0,  1.0)
salary$level_display <- c("BI-03","EG-04","EG-05","SE-RES-02", "EG-04", "PC-02",
                          "PC-02","EG-03")
salary$funding_source_display <- c("Core (A-base)","Snow Crab (C-base)", "Core (A-base)",
"Core (A-base)",  "Snow Crab (C-base)", "Snow Crab (C-base)",
"Snow Crab (C-base)", "Snow Crab (C-base)")
salary$theme <- "test theme"
salary$employee_type_display <- "Indeterminate Employee"
salary$project_year_id <- 111
salary$project_id <- 1234
salary$fiscal_year <- "2014-2015"
salary$project_title <- "Fake project"
salary$median_salary <- c(97029.5, 70297.0, 77322.5, 95903.0, 70297.0, 81960.5, 81960.5, 63903.0)
salary$salary_per_week <- c(1865.952, 1351.865, 1486.971, 1844.288, 1351.865, 1576.163, 1576.163, 1228.904)
salary$amount_week <- c(18659.5192, 40555.9615, 44609.1346, 184.4288, 20277.9808, 1576.1635,0.0000,
                        1228.9038)
salary$amount_overtime <- c(0.000, 19827.359, 21808.910,0.000,  8111.192, 0.000,0.000, 0.000)
salary$amount_total <- c(18659.5192, 60383.3205, 66418.0449,184.4288, 28389.1731, 1576.1635, 0.0000,
                         1228.9038)
salary$activity_type <- "Other"

save(salary, file="salary.rda")
tools::resaveRdaFiles('salary.rda')
}

