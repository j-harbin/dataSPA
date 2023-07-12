#' Create a fact sheet for a specific project
#'
#' This function creates a fact sheet report for the
#' specified project id.
#'
#' @param om a data frame created by `getData(type='om')`
#' @param salary a data frame created by `getData(type='salary')`
#' @param id the project_id from the Project Planning Tool (PPT)
#' @param cookie a sessionid and csrftoken from a Department of
#' Fisheries and Oceans Canada (DFO) employee in the following
#' format: csrftoken=YOURTOKEN; sessionid=YOURSESSIONID
#' @param destdir parameter indicating where to save the html report
#' @return A fact sheet about the specified project id.
#' @importFrom rmarkdown render
#' @examples
#' \dontrun{
#' library(dataSPA)
#' data <- getData(type="om",cookie=cookie)
#' data2 <- getData(type="salary", cookie=cookie)
#' createReport(om=data, salary=data2, id=1234, cookie=cookie)
#' }
#' @export

createReport <- function(om=NULL, salary=NULL, cookie=NULL, id=NULL, destdir=".") {

  if (is.null(id)) {
    stop("In createReport() must provide an id argument pertaining to the project_id")
  }

  if (is.null(om)) {
    stop("In createReport() must provide an om argument")
  }
  if (is.null(cookie)) {
    stop("In createReport() Must provide a cookie argument in the following format:csrftoken=YOURTOKEN; sessionid=YOURSESSIONID")
  }

  if (is.null(salary)) {
    stop("In createReport() must provide an salary argument")
  }

  if (!(identical(c("project_id","category_display","project_year_id","amount","funding_source_display", "id", "category_type", "description",
                    "fiscal_year", "project_title", "status", "overview", "objectives","section_display", "lead_staff", "deliverables", "milestones"), names(om)))) {
    stop("Must obtain data for om using getData(type='om')")
  }
  Rmdpath <- file.path(system.file(package="dataSPA"),"rmarkdown","templates","word_document","skeleton")

  if (!(identical(c("id","overtime_hours","smart_name","duration_weeks",
                    "level_display","funding_source_display","employee_type_display",  "project_year_id",
                    "project_id","fiscal_year", "project_title","median_salary",
                    "salary_per_week","amount_week","amount_overtime", "amount_total"), names(salary)))) {
    stop("Must obtain data for salary using getData(type='salary')")
  }

  if (!(class(om) == "data.frame")) {
    stop("om must be a data frame created from getData(type='om')")
  }

  if (!(class(salary) == "data.frame")) {
    stop("salary must be a data frame created from getData(type='salary')")
  }

  # Dealing with om
  for (i in seq_along(id)) {
  #message("Now working with id = ", id[i], " in createReport()")
  index <- om[which(om$project_id == id[i]),]

  # Dealing with salary
  index2 <- salary[which(salary$project_id == id[i]),]

  ## Move into Rmd
    rmarkdown::render(file.path(Rmdpath, "skeleton.Rmd"), output_dir=destdir, output_file=id[i], output_format = "html_document")
  }
}
