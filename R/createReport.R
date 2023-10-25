#' Create a fact sheet for a specific project
#'
#' This function creates a fact sheet report for the
#' specified project id, theme, functional group, section,
#' or division. Each summary fact sheet is broken up into
#' different tabs: Summary, Research, Monitoring, Advice,
#' and Other. By default, this only looks at the Approved
#' projects.
#'
#' @param om a data frame created by `getData(type='om')`
#' @param salary a data frame created by `getData(type='salary')`
#' @param statusReport a data frame created by `getData(type='statusReport')`
#' @param id the project_id from the Project Planning Tool (PPT)
#' @param theme theme classification of projects of either `Mitigation of Ecosystem Stressors`,
#' `Marine Spatial Planning and Conservation`, `Ecosystem Assessment and Climate Change`,
#' `Population Assessment and Recovery`, `Other`, `Technology Development and Application`, and
#'  `Pacific Salmon`
#' @param functionalGroup classification of projects. Too see options do
#' `unique(om$function_group)`, where `om`is the output from `getData(type="om")`
#' @param path path to look for saved om_date. This must be the save as the path given when doing
#'  `getData(type='om_date', keep=TRUE)`. This allows the report to print when the data was obtained
#'   from the PPT. See Examples for more information.
#' @param section classification of projects referring to the sections at DFO. For
#' more details do `unique(om$section_display)`, where `om`is the output from
#' `getData(type="om")`
#' @param division classification of projects referring to the divisions at DFO. For
#' more details do `unique(om$section_display)`, where `om`is the output from
#' `getData(type="om")`
#' @param region parameter to specific specific region of either `Gulf`, `Maritimes`,
#' `Pacific`, `Quebec`, or `Ontario and Prairie`
#' @param status a character string indicating indicating which project
#' status to include (Approved,Reviewed, Draft, Submitted, Not Approved,
#' Recommended, and Cancelled). If NULL, all projects are included.
#' are included
#' @param cookie a sessionid and csrftoken from a Department of
#' Fisheries and Oceans Canada (DFO) employee in the following
#' format: csrftoken=YOURTOKEN; sessionid=YOURSESSIONID
#' @param destdir parameter indicating where to save the html report
#' @return A fact sheet about the specified project id.
#' @importFrom rmarkdown render
#' @examples
#' \dontrun{
#' # Example 1
#' om <- getData(type="om", cookie=cookie, keep=TRUE, path=".")
#' sal <- getData(type="salary", cookie=cookie, keep=TRUE, path=".")
#' omdate <- getData(type="om_date", cookie=cookie, keep=TRUE, path=".")
#' createReport(om=om, salary=sal, cookie=cookie, id=1093, path=".")
#' }
#' @export

createReport <- function(om=NULL, salary=NULL, statusReport=NULL, cookie=NULL, id=NULL, theme=NULL,functionalGroup=NULL, section=NULL, division=NULL, region=NULL, status=NULL, destdir=".", path="//dcnsbiona01a/BIODataSVC/IN/MSP/PowerBI-Projects/dataSPA/") {
  Rmdpath <- file.path(system.file(package="dataSPA"),"rmarkdown","templates","word_document","skeleton")

  if(!(is.null(statusReport))) {
    if (!(identical(c("project_id","target_completion_date_display","status_display","supporting_resources",
                      "major_accomplishments","major_issues", "excess_funds_comment", "excess_funds_amt",
                      "excess_funds","insufficient_funds", "insufficient_funds_amt","insufficient_funds_comment",
                      "rationale_for_modified_completion_date", "general_comment", "project_year", "project_title",
                      "fiscal_year"), names(statusReport)))) {
      stop("Must obtain data for salary using getData(type='statusReport')")
    }

    if (is.null(id)) {
      stop("Must provide an id for statusReport type")
    }

    if (is.null(om)) {
      stop("Must provide an om for statusReport type")
    }

    if (is.null(salary)) {
      stop("Must provide an salary for statusReport type")
    }

    for (i in seq_along(id)) {
      index <- om[which(om$project_id == id[i]), ]
      index2 <- salary[which(salary$project_id == id[i]), ]
      indexS <- statusReport[which(statusReport$project_id == id[i]), ]
      # Dealing with salary
      if (length(indexS$project_id) == 0) {
        stop("No statusReport projects have id = ", id)
      }
      ## Move into Rmd
      rmarkdown::render(
        file.path(Rmdpath, "statusReport.Rmd"),
        output_dir = destdir,
        output_file = paste0(id[i], "statusReport"),
        output_format = "html_document"
      )
    }
  }
  if (is.null(statusReport)) {
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

    if (!(is.null(status))) {
      if (length(status) > 1) {
        stop("Can only give one status at a time.")
      }
      if (!(status %in% c("Reviewed", "Approved", "Draft", "Cancelled", "Submitted", "Not Approved", "Recommended"))) {
        stop("status must be either Reviewed, Approved, Draft, Cancelled, Submitted, Not Approved, or Recommended")
      } else {
        if (!(is.null(om))) {
          om <- om[which(om$status == status),]
          if (length(om) == 0) {
            stop("No projects for this subset in this region.")
          }
        } else {
          salary <- salary[which(salary$status == status),]
          if (length(salary) == 0) {
            stop("No projects for this subset in this region.")
          }

        }

      }
    }

  if (!(is.null(region))) {
    regions <- unique(str_extract(om$section_display, "[^-]+"))
    if (any(is.na(regions))) {
    regions <- regions[(-which(is.na(regions)))]
    }
    regions <- str_trim(regions)

    if (!(region %in% regions)) {
      stop("No projects in region = ", region, " . Try ", paste0(regions, collapse=","), " instead.")
    }
    om <- om[which(str_trim(str_extract(om$section_display, "[^-]+")) == region),]
    salary <- salary[which(str_trim(str_extract(salary$section_display, "[^-]+")) == region),]
    if (length(om) == 0 && length(salary) == 0) {
      stop("No projects for this subset in this region.")
    }
  }
  if (!(identical(c("id","overtime_hours","smart_name","duration_weeks",
                   "level_display","funding_source_display","employee_type_display",  "project_year_id",
                   "project_id","fiscal_year", "project_title","median_salary",
                   "salary_per_week","amount_week","amount_overtime", "amount_total", "theme", "activity_type", "functional_group","section_display", "overview",
                   "objectives", "status","lead_staff","deliverables","milestones"), names(salary)))) {
   stop("Must obtain data for salary using getData(type='salary')")
  }

  if (!(class(om) == "data.frame")) {
    stop("om must be a data frame created from getData(type='om')")
  }

  if (!(class(salary) == "data.frame")) {
    stop("salary must be a data frame created from getData(type='salary')")
  }

  if(!(is.null(id)) && (!(is.null(theme))) && (!is.null(functionalGroup)) && (!(is.null(section))) && (!(is.null(division)))) {
    message("id,theme,functionalGroup, section, and division given. theme is used.")
    id <- NULL
    functionalGroup <- NULL
    division <- NULL
    section <- NULL
  }

  # Dealing with om
  if (!(is.null(id))) {
    for (i in seq_along(id)) {
      index <- om[which(om$project_id == id[i]), ]
      # Dealing with salary
      index2 <- salary[which(salary$project_id == id[i]), ]
      if (length(index$project_id) == 0 && length(index2$project_id) == 0) {
        stop("No projects have id = ", id)
      }
      ## Move into Rmd
      rmarkdown::render(
        file.path(Rmdpath, "skeleton.Rmd"),
        output_dir = destdir,
        output_file = id[i],
        output_format = "html_document"
      )
    }
  } else {
    if (!(is.null(theme))) {
    for (i in seq_along(theme)) {
      index <- om[which(om$theme == theme[i]),]

      # Dealing with salary
      index2 <- salary[which(salary$theme == theme[i]), ]

      if (length(index$project_id) == 0 && length(index2$project_id) == 0) {
        stop("No projects have theme = ", theme)
      }

      ## Move into Rmd
      rmarkdown::render(
        file.path(Rmdpath, "skeleton2.Rmd"),
        output_dir = destdir,
        output_file = theme[i],
        output_format = "html_document"
      )
    }
    } else if (!(is.null(functionalGroup))) {
      for (i in seq_along(functionalGroup)) {
        index <- om[which(om$functional_group == functionalGroup[i]),]
        # Dealing with salary
        index2 <- salary[which(salary$functional_group == functionalGroup[i]), ]

        if (length(index$project_id) == 0 && length(index2$project_id) == 0) {
          stop("No projects have functional group = ", functionalGroup)
        }

        ## Move into Rmd
        rmarkdown::render(
          file.path(Rmdpath, "skeleton3.Rmd"),
          output_dir = destdir,
          output_file = functionalGroup[i],
          output_format = "html_document"
        )
      }
    } else if (!(is.null(section))) {
      for (i in seq_along(section)) {
        index <- om[which(gsub(".*- ","",om$section_display) == section[i]),]
        index2 <- salary[which(gsub(".*- ","",salary$section_display) == section[i]),]

        if (length(index$project_id) == 0 && length(index2$project_id) == 0) {
          stop("No projects have section = ", section)
        }

        ## Move into Rmd
        rmarkdown::render(
          file.path(Rmdpath, "skeleton4.Rmd"),
          output_dir = destdir,
          output_file = section[i],
          output_format = "html_document"
        )
      }

    } else if (!(is.null(division))) {
      for (i in seq_along(division)) {
        index <- om[which(unlist(lapply(strsplit(om$section_display, " - ", fixed=TRUE), function(x) x[3])) == division[i]),]
        index2 <- salary[which(unlist(lapply(strsplit(salary$section_display, " - ", fixed=TRUE), function(x) x[3])) == division[i]),]

        if (length(index$project_id) == 0 && length(index2$project_id) == 0) {
          stop("No projects have division = ", division[i])
        }

        ## Move into Rmd
        rmarkdown::render(
          file.path(Rmdpath, "skeleton5.Rmd"),
          output_dir = destdir,
          output_file = division[i],
          output_format = "html_document"
        )
      }
}

  }
  }
}
