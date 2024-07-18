install.packages("dataSPA", repos = NULL, type="source")

library(dataSPA)
library(stringr)
data(salaries)

datadir <- "/home/mar-spa/"
siteroot <- "/home/mar-spa/mar-spa"
unlink(siteroot,recursive = TRUE)
dir.create(siteroot)
# file.copy(file.path(datadir,"dataSPA_om.rds"),file.path(siteroot,"dataSPA_om.rds"))
# file.copy(file.path(datadir,"dataSPA_SAL.rds"),file.path(siteroot,"dataSPA_SAL.rds"))


zip(zipfile = file.path(siteroot,"dataSPA.zip"),files = "dataSPA")
# remotes::install_local(file.path(siteroot,"dataSPA.zip"))


reportdir <- file.path(siteroot,"reports")
functionalGroupdir <- file.path(siteroot,"functionalGroup")
themedir <- file.path(siteroot,"themes")
divisionsdir <- file.path(siteroot,"divisions")
sectionsdir <- file.path(siteroot,"sections")

dir.create(reportdir)
dir.create(functionalGroupdir)
dir.create(themedir)
dir.create(divisionsdir)
dir.create(sectionsdir)

om <- getData(type="om",cookie=cookie,keep=TRUE,age=14,path = datadir)
om <- om[which(str_trim(str_extract(om$section_display, "[^-]+")) == "Maritimes"),] # Hard coded in for maritimes
om <- om[om$status=="Approved",]
div_sec <- strsplit(om$section_display," - ",fixed=TRUE)
divisions <- sapply(div_sec,"[",3)
sections <- sapply(div_sec,"[",4)
salary <- getData(type="salary", cookie=cookie, keep=TRUE,age=14,path = datadir)
filedate <- getData(type="om_date", cookie=cookie, keep=TRUE,age=14,path = datadir)
filedate <- paste0(filedate, "using [this](https://github.com/j-harbin/dataSPA/tree/",system("git -C dataSPA/ rev-parse HEAD", intern=TRUE),") version of the dataSPA package.")

# create all reports
print("creating reports")
for(i in unique(om$project_id)){
   try(createReport(om=om, salary=salary, id=i, cookie=cookie,path = getwd(),destdir = reportdir))
}

# create all functional groups
print("creating functional groups")
for(i in unique(om$functional_group)){
  try(createReport(om=om, salary=salary, functionalGroup = i, cookie=cookie,path = getwd(),destdir = functionalGroupdir))
}

# create all themes
print("creating themes")
for(i in unique(om$theme)){
  try(createReport(om=om, salary=salary, theme = i, cookie=cookie,path = getwd(),destdir = themedir))
}

# create all divisions
print("creating divisions")
for(i in unique(divisions)){
  try(createReport(om=om, salary=salary, division = i, cookie=cookie,path = getwd(),destdir = divisionsdir))
}

# create all sections
print("creating sections")
for(i in unique(sections)){
  try(createReport(om=om, salary=salary, section = i, cookie=cookie,path = getwd(),destdir = sectionsdir))
}

# render hub
print("rendering index")
try(rmarkdown::render(file.path(datadir,'dataSPA/inst/hub/dataSPA.Rmd'),output_file = file.path(siteroot,"index.html")))
print("rendering table")
try(rmarkdown::render('dataSPA/inst/hub/table.Rmd',output_dir = siteroot))
print("rendering summary")
try(rmarkdown::render('dataSPA/inst/hub/summary.Rmd',output_dir = siteroot))
