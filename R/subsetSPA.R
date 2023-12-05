#' Subset O&M and salary data frames
#'
#' This function either A) return a subset data frame of O&M or salary
#' data frame by region, approved projects, theme,
#' functional group, section, or division or B)
#' returns a list of theme, functional group, section,
#' or division of the setset if either of the above
#' mentioned arguments are set to "return"
#'
#' @param om a data frame likely from `getData(type='om')`
#' @param salary a data frame likely from `getData(type='salary')`
#' @param theme theme classification of projects. This could be set to `return`
#' to see a list of possibilities. See examples below.
#' @param functionalGroup classification of projects. This could be set to `return`
#' to see a list of possibilities. See examples below.
#' @param section classification of projects referring to the sections at DFO.
#' This could be set to `return` to see a list of possibilities. See examples below.
#' @param division classification of projects referring to the divisions at DFO.
#' This could be set to `return` to see a list of possibilities. See examples below.
#' @param region parameter to specify specific region of either `Gulf`, `Maritimes`,
#' `Pacific`, `Quebec`, or `Ontario and Prairie`.
#' @param year parameter to specify a specific fiscal year, e.g. "2023-2024".
#' @param status a character string indicating indicating which project
#' status to include (Approved,Reviewed, Draft, Submitted, Not Approved,
#' Recommended, and Cancelled). If NULL, all projects are included.
#' are included
#' @importFrom stringr str_extract
#' @export
#' @examples
#' \dontrun{
#' # Example 1: Subset for approved projects in the Maritimes Region
#' # for the Coastal Exosystem Science Division
#' library(dataSPA)
#' om <- getData(type="om", cookie=cookie, age=100)
#' d <- subsetSPA(om=om, status="Approved", region="Maritimes",
#' division ="Coastal Ecosystem Science Division")
#' head(d)
#'
#' # Example 2: Get a list of the sections that contain approved projects
#' # in the Maritimes Region
#' d <- subsetSPA(om=om, status="Approved", region="Maritimes",
#' section ="return")
#' }

subsetSPA <- function(om=NULL, salary=NULL, status=NULL, region=NULL, theme=NULL,
                      functionalGroup=NULL, section=NULL, division=NULL, year=NULL) {
  # Can only have om or salary
  if (!(is.null(om)) && (!(is.null(salary)))) {
    stop("Can only do either om or salary. Cannot do them at the same time.")
  }

  # remove all of the amount = 0
  if (!(is.null(om))) {
    if (any(om$amount == 0)) {
      om <- om[-(which(om$amount == 0)),]
    }
  }

  if (!(is.null(salary))) {
    if (any(salary$amount_total == 0)) {
      salary <- salary[-(which(salary$amount_total == 0)),]
    }
  }

  # OM
  if (!(is.null(om))) {
  if (!(identical(
    c("project_id","category_display","project_year_id",
      "amount","funding_source_display",
      "category_type",  "description", "tags",
      "tag_id", "fiscal_year", "project_title",
      "status","overview","objectives",
      "lead_staff","section_display","functional_group",
      "activity_type","theme", "deliverables",
      "milestones", "section_id","division_id",
      "region_id", "funding_id","theme_id",
      "om_id","branch_id"),
    names(om)
  ))) {

    stop("Must obtain data for x using getData(type='om')")
  }
  }


  # SALARY
  if (!(is.null(salary))) {
  if (!(identical(
    c("overtime_hours",         "smart_name",             "duration_weeks"
      ,"level_display",          "funding_source_display", "employee_type_display"
      ,"project_year_id",        "project_id",             "fiscal_year"
      ,"project_title",          "median_salary",          "salary_per_week"
      ,"amount_week",            "amount_overtime",        "amount_total"
      ,"theme",                  "activity_type",          "functional_group"
      ,"section_display",        "overview",               "objectives"
      ,"tag",                    "tag_id",                 "status"
      ,"lead_staff",             "deliverables",           "milestones"
      ,"section_id",             "division_id",            "region_id"
      ,"funding_id",             "theme_id",               "branch_id"
      ,"staff_id"),
    names(salary)
  ))) {
    stop("Must obtain data for x using getData(type='salary')")
  }
  }

  # STATUS
  if (!(is.null(status))) {
    if (length(status) > 1) {
      stop("Can only give one status at a time.")
    }
    if (!(status %in% c("Reviewed", "Approved", "Draft", "Cancelled", "Submitted", "Not Approved", "Recommended"))) {
      stop("status must be either Reviewed, Approved, Draft, Cancelled, Submitted, Not Approved, Recommended")
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


  # REGION
  if (!(is.null(region))) {
    if (!(is.null(om)) && (!(length(om$project_id) == 0))) {
      regions <- unique(str_extract(om$section_display, "[^-]+"))
    } else {
      regions <- unique(str_extract(salary$section_display, "[^-]+"))
    }
    if (any(is.na(regions))) {
      regions <- regions[(-which(is.na(regions)))]
    }
    regions <- str_trim(regions)

    if (region == "return") {
      return(sort(regions))
    }

    if (!(region %in% regions)) {
      stop("No projects in region = ", region, " . Try ", paste0(regions, collapse=","), " instead.")
    }
    if (!(is.null(om))) {
    om <- om[which(str_trim(str_extract(om$section_display, "[^-]+")) == region),]
    if (length(om) == 0 ) {
      stop("No projects for this subset in this region.")
    }
    } else {
    salary <- salary[which(str_trim(str_extract(salary$section_display, "[^-]+")) == region),]
    if (length(salary) == 0 ) {
      stop("No projects for this subset in this region.")
    }
    }
  }



  # YEAR
  if (!(is.null(year))) {
    if (!(is.null(om))) {
      if (!(year %in% unique(om$fiscal_year))) {
        stop("No projects in year = ", year, " . Try ", paste0(unique(om$fiscal_year), collapse=","), " instead.")
      }

      om <- om[which(om$fiscal_year == year),]
      if (length(om) == 0 ) {
        stop("No projects for this subset in this year.")
      }
    } else {
      if (!(year %in% unique(salary$fiscal_year))) {
        stop("No projects in year = ", year, " . Try ", paste0(unique(salary$fiscal_year), collapse=","), " instead.")
      }
      salary <- salary[which(salary$fiscal_year == year),]
      if (length(salary) == 0 ) {
        stop("No projects for this subset in this year.")
      }
    }
  }


  # THEME
  if (!(is.null(theme))) {
  if (length(theme) > 1) {
    stop("Can only provide 1 theme at a time, not ", length(theme))
  }
  if (theme == "return" && (!(is.null(om)))) {
    the <- unique(om$theme)
    if (any(the == "0")) {
      the <- the[-(which(the == "0"))]
    }
      return(sort(the))
  }

  if (theme == "return" && (!(is.null(salary)))) {
      return(sort(unique(salary$theme)))
  }
    if (!(is.null(om))) {
      THEMES <- unique(om$theme)
    } else {
      THEMES <- unique(salary$theme)
    }

  if (!(theme %in% THEMES)) {
    stop("No projects have theme ", theme, " try ", paste0(THEMES, collapse=","), " instead.")
  }


  if (!(is.null(om))) {
  index <- om[which(om$theme == theme),]
  } else {
  index <- salary[which(salary$theme == theme),]
  }
  if (length(index) == 0) {
    stop("No projects have theme ", theme, " try ", ifelse((!(is.null(om))), paste0(unique(om$theme), collapse=","), paste0(unique(salary$theme), collapse=",")), " instead.")
  } else {
    if (!(is.null(om))) {
    om <- index
    } else {
      salary <- index
    }
  }
  # FUNCTIONAL GROUP
} else if (!(is.null(functionalGroup))) {
  if (length(functionalGroup) > 1) {
    stop("Can only provide 1 functionalGroup at a time, not ", length(theme))
  }

  if (!(is.null(om))) {
    FGS <- unique(om$functional_group)
  } else {
    FGS <-  unique(salary$functional_group)
  }
  if (functionalGroup == "return") {
    if (any(FGS == "")) {
      FGS <- FGS[-(which(FGS == ""))]
    }
    return(sort(FGS))
  }

  if (!(functionalGroup %in% FGS)) {
    stop("No projects have functionalGroup ", functionalGroup, " try ", paste0(FGS, collapse=","), " instead.")
  }

  if (!(is.null(om))) {
  index <- om[which(om$functional_group == functionalGroup),]
  } else {
  index <- salary[which(salary$functional_group == functionalGroup),]
  }
  if (length(index) == 0) {
    stop("No projects have functionalGroup ", functionalGroup, " try ", ifelse((!(is.null(om))), paste0(unique(om$functional_group), collapse=","), paste0(unique(salary$functional_group), collapse=",")), " instead.")
  } else {
    if (!(is.null(om))) {
    om <- index
    } else {
      salary <- index
    }
  }
  # SECTION
} else if (!(is.null(section))) {
  if (length(section) > 1) {
    stop("Can only provide 1 section at a time, not ", length(section))
  }

  if (!(is.null(om)) | !(length(om$project_id) == 0)) {
    sec <- unique(gsub(".*- ","",unique(om$section_display)))

  } else {
    sec <- unique(gsub(".*- ","",unique(salary$section_display)))
  }

  if (section == "return") {
    return(sort(unique(sec)))
  }

  if (!(section %in% unique(sec))) {
    stop("No projects have section ",section, " try ", paste0(sec, collapse=","), " instead.")
  }
  if (!(is.null(om))) {
  index <- om[which(gsub(".*- ","",om$section_display) == section),]
  } else {
  index <- salary[which(gsub(".*- ","",salary$section_display) == section),]
  }
  if (length(index) == 0) {
    stop("No projects have section ", section, " try ", paste0(sec, collapse=","), " instead.")
  } else {
    if (!(is.null(om))) {
    om <- index
    } else {
      salary <- index
    }
  }
  # DIVISION
} else if (!(is.null(division))) {
  if (length(division) > 1) {
    stop("Can only provide 1 division at a time, not ", length(division))
  }
  if (!(is.null(om))) {
  div <- NULL
  for (i in seq_along(om$section_display)) {
    div[[i]] <- strsplit(om$section_display[i], " - ", fixed=TRUE)[[1]][3]
  }
  div <- unlist(unique(div))
  if (division == "return") {
    return(sort(unique(div)))
  }
  if (!(division %in% unique(div))) {
    stop("No projects have division ",division, " try ", paste0(div, collapse=","), " instead.")
  }
  index <- om[which(unlist(lapply(strsplit(om$section_display, " - ", fixed=TRUE), function(x) x[3])) == division),]
  } else {
    div <- NULL
    for (i in seq_along(salary$section_display)) {
      div[[i]] <- strsplit(salary$section_display[i], " - ", fixed=TRUE)[[1]][3]
    }
    div <- unlist(unique(div))
    if (division == "return") {
      return(sort(unique(div)))
    }
    if (!(division %in% unique(div))) {
      stop("No projects have division ",division, " try ", paste0(div, collapse=","), " instead.")
    }
    index <- salary[which(unlist(lapply(strsplit(salary$section_display, " - ", fixed=TRUE), function(x) x[3])) == division),]

  }

  if (length(index) == 0) {
    stop("No projects have division ", division, " try ", paste0(div, collapse=","), " instead.")
  } else {
    if (!(is.null(om))) {
    om <- index
    } else {
      salary <- index
    }
  }
}
  if (!(is.null(om))) {
  return(om)
  } else {
    return(salary)
  }
  }

