library(tibble)
groups <- c("AI", "AO", "AV", "CS", "CX", "EC", "EL", "FB", "FI", "FS", "LP","NR", "PA", "RE", "RO", "SO", "SP", "TC", "TR", "UT", "SV")
salaries <- formatSalaries(groups=groups)
salaries <- as_tibble(salaries)
salaries$`Annual Salary` <- as.numeric(salaries$`Annual Salary`)
#save(salaries, file="./data/salaries.rda")

# OLD
# library(readxl)
# salaries <- read_excel("../../../Code (Not for GitHub)/05_pay_scale/Salaries_JH.xlsx")




# REMOVE
d <- salaries[which(salaries$Classification == "EG"),]
dd <- d[which(grepl("EG--04", d$`Level and Step`)),]
ddd <- dd[which(grepl("21", dd$`Level and Step`)),]
median(ddd$`Annual Salary`)/52*30
