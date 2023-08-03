#' Create a fact sheet for a specific project
#'
#' This function creates a fact sheet report for the
#' specified project id or theme.
#'
#' @param om a data frame created by `getData(type='om')`
#' @param salary a data frame created by `getData(type='salary')`
#' @param id the project_id from the Project Planning Tool (PPT)
#' @param theme theme classification of projects of either `Mitigation of Ecosystem Stressors`,
#' `Marine Spatial Planning and Conservation`, `Ecosystem Assessment and Climate Change`,
#' `Population Assessment and Recovery`, `Other`, `Technology Development and Application`, and
#'  `Pacific Salmon`
#' @param path path to look for saved om_date. This must be the save as the path given when doing
#'  `getData(type='om_date', keep=TRUE)`. This allows the report to print when the data was obtained
#'   from the PPT. See Examples for more information.
#' @param cookie a sessionid and csrftoken from a Department of
#' Fisheries and Oceans Canada (DFO) employee in the following
#' format: csrftoken=YOURTOKEN; sessionid=YOURSESSIONID
#' @param destdir parameter indicating where to save the html report
#' @return A fact sheet about the specified project id.
#' @importFrom rmarkdown render
#' @examples
#' \dontrun{
#' om <- getData(type="om", cookie=cookie, keep=TRUE, path=".")
#' sal <- getData(type="salary", cookie=cookie, keep=TRUE, path=".")
#' omdate <- getData(type="om_date", cookie=cookie, keep=TRUE, path=".")
#' createReport(om=om, salary=sal, cookie=cookie, id=1093, path=".")
#' }
#' @export

createReport <- function(om=NULL, salary=NULL, cookie=NULL, id=NULL, theme=NULL, destdir=".", path="//dcnsbiona01a/BIODataSVC/IN/MSP/PowerBI-Projects/dataSPA/") {

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
                    "fiscal_year", "project_title", "status", "overview", "objectives","section_display", "lead_staff","functional_group","activity_type", "theme", "deliverables", "milestones"), names(om)))) {
    stop("Must obtain data for om using getData(type='om')")
  }
  Rmdpath <- file.path(system.file(package="dataSPA"),"rmarkdown","templates","word_document","skeleton")

  if (!(identical(c("id","overtime_hours","smart_name","duration_weeks",
                    "level_display","funding_source_display","employee_type_display",  "project_year_id",
                    "project_id","fiscal_year", "project_title","median_salary",
                    "salary_per_week","amount_week","amount_overtime", "amount_total", "theme"), names(salary)))) {
    stop("Must obtain data for salary using getData(type='salary')")
  }

  if (!(class(om) == "data.frame")) {
    stop("om must be a data frame created from getData(type='om')")
  }

  if (!(class(salary) == "data.frame")) {
    stop("salary must be a data frame created from getData(type='salary')")
  }

  if(!(is.null(id)) && (!(is.null(theme)))) {
    message("Both id and theme given. id argument was ignored.")

    id <- NULL
  }

  # Dealing with om
  if (!(is.null(id))) {
    for (i in seq_along(id)) {
      index <- om[which(om$project_id == id[i]), ]
      #browser()

      # Dealing with salary
      index2 <- salary[which(salary$project_id == id[i]), ]

      ## Move into Rmd
      rmarkdown::render(
        file.path(Rmdpath, "skeleton.Rmd"),
        output_dir = destdir,
        output_file = id[i],
        output_format = "html_document"
      )
    }
  } else {
    for (i in seq_along(theme)) {
      index <- om[which(om$theme == theme[i]),]

      # Dealing with salary
      index2 <- salary[which(salary$theme == theme[i]), ]

      ## Move into Rmd
      rmarkdown::render(
        file.path(Rmdpath, "skeleton2.Rmd"),
        output_dir = destdir,
        output_file = theme[i],
        output_format = "html_document"
      )
    }

  }
}
