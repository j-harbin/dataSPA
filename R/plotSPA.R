#' Plot dataSPA information
#'
#' This function plots specific graphs using the data
#' frames returned by [getData()]. Note: by default, only
#' the approved projects are included (this can be changed
#' using the status argument).
#'
#' The various plot types are as follows:
#'
#' * For `which="omBar"`, a bar graph representing amount of money ($)
#  invested in O&M per funding type for project years is plotted. Graph color
#' coded by funding type. If plotting for a specific project, the number of
#' deliverables and milestones per year are shown in blue and red respectively
#'
#' * For `which="omPie"`, a pie chart representing amount of money ($)
#  invested in O&M per funding type for project years is plotted. Graph color
#' coded by funding type
#'
#' * For `which="omAllocation"` Line graphs representing changes in O&M
#' investment for categories over the years. The average rate of change
#' per year for each category, where applicable, displayed on the graph in red
#'
#' * For `which=omAllocationGeneral` a pie chat representing proportion of
#' funding per category type is plotted for year each of the specified project
#'
#' * For `which="salaryBar"` a bar graph representing amount of salary money ($)
# invested per funding type for project years. Salaries are calculated
#' using the median values of a specific job classification. Graph color
#' coded by funding type indicated by the legend.
#'
#' * For `which="salaryAllocation"` Line graphs representing changes in Salary
#' investment for different classifications over the years. The average rate
#' of change per year for each category, where applicable, displayed on the
#' graph in red
#'
#' * For `which="weekAllocation"` a bar graph representing time (weeks)
#' invested per job classification for project years is plotted
#'
#' * For `which="indeterminate"` a bar chart representing percentage of
#' indeterminate vs non-Indeterminate employees for project years.
#' Number of staff shown on the figure in red
#'
#' * For `which="predictSummary"`a line chart showing the trends for
#' changes in different funding scenarios
#'
#' * For `which="predict"` a bar chart showing the trends for changes
#' in different funding scenarios is plotted with the number of stations
#' overlying the figure with a line. A new plot is plotted for each
#' funding scenario. If `endDate` is provided, the bar chart will
#' represent funding as status quo until the end date (See example 3)
#'
#' * For `which="predictSalary` a bar chat showing the trends of salary
#' increasing while funding amount remains the same. Note: This plot does
#' not include amount of overtime as that is hard to predict.
#'
#' * For `which="predictOM` a bar chart showing the trends of om
#' increasing while funding amoutn remains the same. This plot
#' assumes an inflation rate of 2%.
#'
#' * For `which="overviewStatus"` a pie chart is created that shows the
#' number of projects for each of the following status': Approved,
#' Draft, Submitted, and Reviews for the specified parameters.
#'
#' * For `which="overviewInvestment"` a bar chart indicating how much
#' money was spent on A) OM investment (if an om argument is provided),
#' B) Salary investment (if a salary argument is provided), or C)
#' OM and Salary investment (if both om & salary argument given).
#' Note: This includes overtime salary amount.
#'
#' @param om a data frame likely from `getData(type='om')`
#' @param salary a data frame likely from `getData(type='salary')`
#' @param which indicates which plot to plot (See Details).
#' @param id the project_id from the Project Planning Tool
#' @param theme theme classification of projects of either `Mitigation of Ecosystem Stressors`,
#' `Marine Spatial Planning and Conservation`, `Ecosystem Assessment and Climate Change`,
#' `Population Assessment and Recovery`, `Other`, `Technology Development and Application`, and
#'  `Pacific Salmon`
#' @param functionalGroup classification of projects. Too see options do
#' `unique(om$function_group)`, where `om`is the output from `getData(type="om")`
#' @param section classification of projects referring to the sections at DFO. For
#' more details do `unique(om$section_display)`, where `om`is the output from
#' `getData(type="om")`
#' @param division classification of projects referring to the divisions at DFO. For
#' more details do `unique(om$section_display)`, where `om`is the output from
#' `getData(type="om")`
#' @param region parameter to specific specific region of either `Gulf`, `Maritimes`,
#' `Pacific`, `Quebec`, or `Ontario and Prairie`. If no region is given, a summary plot
#' of all regions is given.
#' @param status a character string indicating indicating which project
#' status to include (Approved,Reviewed, Draft, Submitted, Not Approved,
#' Recommended, and Cancelled). If NULL, all projects are included.
#' are included
#' @param funding a variable used when `which='predictSummary'` or
#' `which='predict'` used to indicate which funding source will
#' experience change in the prediction. If `endDate` is provided,
#' only the length of this argument must equal 1.
#' @param fundingChange a percentage used when `which='predictSummary`
#' or `which=predict` used to indicate by how much the funding
#' specified in `funding` will change by. If `endDate` is provided,
#' this argument is ignored.
#' @param dataframe a Boolean indicating if the user wants the data frame
#' of what is being plotted returned. For multi-year projects, a list of
#' data frames for each year is returned. This is mainly for package testing
#' @param year a character indicating which year of the project to
#' plot. This is given in the format "YYYY-YYYY". If `year` is `NULL`
#' all years are plotted by default
#' @param endDate the end date in "YYYY-YYYY" format that indicates
#' when the funding specified by `funding` ends. This is used
#' when `which='predict'` and can only be an interval of one year.
#' @param item if`which='omAllocation'` an argument
#' specifying which category display to plot. If `which=salaryAllocation`,
#' the argument specifys whichs level to plot. By default all are plotted
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @importFrom graphics barplot
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom graphics pie
#' @importFrom graphics title
#' @importFrom graphics lines
#' @importFrom graphics axis
#' @importFrom graphics layout
#' @importFrom graphics points
#' @importFrom graphics abline
#' @importFrom graphics mtext
#' @importFrom graphics text
#' @importFrom stats quantile
#' @importFrom stats lm
#' @importFrom stats coef
#' @importFrom stringr str_count
#' @importFrom stringr str_trim
#' @importFrom grDevices extendrange
#' @examples
#' \dontrun{
#' # Example 1: Plot Bar graph of O&M Allocations
#' library(dataSPA)
#' data <- getData(type="om", cookie=cookie)
#' plotSPA(om=data, which="omBar")
#'
#' # Example 2: Plot a prediction of number of station
#' # for NCP funding decreasing by 25%, 50%, 75%, and 100%
#' for the Snow Crab project
#'
#' library(dataSPA)
#' data(salaries)
#' data <- getData(type='om', cookie=cookie)
#' data2 <- getData(type='salary', cookie=cookie)
#' plotSPA(om=data, salary=data2, which='predictSummary', id=1093,
#' funding= rep("NCP (A-base)",4),
#' fundingChange=c(-25, -5, -75, -100))
#'
#' # Example 3: Bar plot predicting of the change in
#' number of stations impact as a result of a certain funding
#' ending
#' plotSPA(om=data, salary=data2, which='predict', id=1093,
#' funding= "NCP (A-base)", endDate="2026-2027")
#'
#' # Example 4: Pie Chart showing number of projects by status
#' plotSPA(om=data, salary=data2, which="overviewStatus")
#'
#' }
#' @export
#'
#' @return None (invisible NULL).
#'
#' @author Jaimie Harbin

plotSPA <-
  function(om = NULL,
           salary = NULL,
           which = NULL,
           id = NULL,
           theme = NULL,
           functionalGroup = NULL,
           section=NULL,
           division=NULL,
           region=NULL,
           item = NULL,
           funding = NULL,
           fundingChange = NULL,
           status = NULL,
           dataframe = FALSE,
           year = NULL,
           endDate = NULL,
           debug = 0) {
    project_id <-
      category_display <-
      project_year_id <-
      amount <-
      funding_source_display <-
      fiscal_year <- project_year_id <- category_type <- deliverables <- milestones <- salaries <- NULL

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

    if (is.null(which)) {
      stop(
        "must provide a which argument of either 'omBar', 'omPie', 'omAllocation', 'omAllocationGeneral','salaryBar', 'salaryAllocation', 'weekAllocation', 'indeterminate', 'predictSummary','predict', 'predictSalary', 'predictOM', 'overviewStatus', or 'overviewInvestment'"
      )
    }

    if (is.null(om) && is.null(salary)) {
      stop("Must provide either an om or salary argument depending on your plot choice.")
    }
      color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)] # Getting random colors
      set.seed(1)
      if (is.null(salary)) {
        col <- sample(color, length(unique(om$funding_source_display)))
      } else {
        col <- sample(color, length(unique(salary$funding_source_display)))
      }

    if (which == "overviewStatus") {
      if (is.null(om)) {
        stop("must provide om and salary argument.")

      }
      if (is.null(salary)) {
        stop("must provide om and salary argument.")

      }
      if (is.null(om) && is.null(salary)) {
        stop("must provide om and salary argument.")
      }
      om <- subsetSPA(om=om, theme=theme, functionalGroup=functionalGroup, section=section, division=division, year=year, status=status)
      salary <- subsetSPA(salary=salary, theme=theme, functionalGroup=functionalGroup, section=section, division=division, year=year, status=status)

      o <- subset(om, select=c("project_id", "status"))
      s <- subset(salary, select=c("project_id", "status"))

      ss <- s[which(!(s$project_id %in% o$project_id)),]

      df <- rbind(o, ss)
      unique_rows <- !duplicated(df)
      df2 <- subset(df, unique_rows)
      status_counts <- table(df2$status)
      titleNames <- c("theme", "functionalGroup", "section", "division", "year", "status")
      dt <- data.frame(matrix(NA, nrow = 1, ncol = length(titleNames)))
      names(dt) <- titleNames
      if (is.null(theme)) {
        theme <- 0
      }
      if (is.null(functionalGroup)) {
        functionalGroup <- 0
      }

      if (is.null(section)) {
        section <- 0
      }
      if (is.null(division)) {
        division <- 0
      }
      if (is.null(year)) {
        year <- 0
      }
      if (is.null(status)) {
        status <- 0
      }

      titles <- c(theme, functionalGroup, section, division, year, status)
      for (i in seq_along(titles)) {
        if (is.null(titles[i])) {
          titles[i] <- 0
        }
          dt[,i] <- titles[i]
      }
      items <- titleNames[which(!(titles == "0"))]
      items2 <- titles[which(!(titles == "0"))]
      mainTitle <- paste(items, items2, sep=": ")
      PIE <- pie(status_counts, labels = paste0(names(status_counts), " (", status_counts, ")"), main = mainTitle, col=1:length(names(status_counts)))
      return()
      }

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

      if (!(region %in% regions)) {
        stop("No projects in region = ", region, " . Try ", paste0(regions, collapse=","), " instead.")
      }
      om <- om[which(str_trim(str_extract(om$section_display, "[^-]+")) == region),]
      salary <- salary[which(str_trim(str_extract(salary$section_display, "[^-]+")) == region),]
      if (length(om) == 0 && length(salary) == 0) {
        stop("No projects for this subset in this region.")
      }
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

    if (!(
      which %in% c(
        "omBar",
        "omPie",
        "omAllocation",
        "omAllocationGeneral",
        "salaryBar",
        "salaryAllocation",
        "weekAllocation",
        "indeterminate",
        'predictSummary',
        'predict',
        'predictSalary',
        'predictOM',
        'overviewStatus',
        'overviewInvestment'
      )
    )) {
      stop(
        "must provide a which argument of either 'omBar', 'omPie', 'omAllocation', 'omAllocationGeneral', 'salaryBar', 'salaryAllocation', 'weekAllocation', 'indeterminate', 'predictSummary', 'predict','predictSalary', 'predictOM', 'overviewStatus', or 'overviewInvestment'")
    }

    # if (is.null(id) && is.null(theme) && is.null(functionalGroup) && is.null(section) && is.null(division) && (!which == "overviewStatus")) {
    #   stop("Must provide an id,theme, functionalGroup, section, or division argument")
    # }

    if (debug > 0) {
      message("which= ", which, " and id = ", id)
    }
    if (!(is.null(om)) | !(length(om$project_id) == 0)) {
    sec <- unique(gsub(".*- ","",unique(om$section_display)))

    } else {
    sec <- unique(gsub(".*- ","",unique(salary$section_display)))
    }
    # END OF DEALING WITH FORMAT SECTIONS

    if (which %in% c(
      "omBar",
      "omPie",
      "omAllocation",
      "omAllocationGeneral",
      'predictSummary',
      'predict',
      'predictOM',
      'overviewInvestment'
    )) {
      if (debug > 0) {
        message("om has been identified")
      }
      if (is.null(om)) {
        stop("Must provide an om argument.")

      }
      if (!(identical(
        c("project_id","category_display","project_year_id",
          "amount","funding_source_display",
          "category_type",  "description", "tags",
          "tag_id", "fiscal_year", "project_title",
          "status","overview","objectives",
          "section_display","lead_staff","functional_group",
          "activity_type","theme", "deliverables",
          "milestones", "section_id","division_id",
          "region_id", "funding_id","theme_id",
          "om_id",
          "branch_id"),
        names(om)
      ))) {
        stop("Must obtain data for x using getData(type='om')")
      }

        # Dealing with themes

      if (!(is.null(theme))) {
        id <- NULL
        section <- NULL
        functionalGroup <- NULL
        division <- NULL
      }

        if (!(is.null(id))) {
          # JAIM HERE 2
        crab <- om[which(om$project_id == id),]
        } else if (!(is.null(theme))) {
          if (length(theme) > 1) {
            stop("Can only provide 1 theme at a time, not ", length(theme))
          }
          if (!(theme %in% unique(om$theme))) {
            stop("No projects have theme ", theme, " try ", paste0(unique(om$theme), collapse=","), " instead.")
          }
          crab <- om[which(om$theme == theme),]
          if (length(crab) == 0) {
            stop("No projects have theme ", theme, " try ", paste0(unique(om$theme), collapse=","), " instead.")
          }
        } else if (!(is.null(functionalGroup))) {
          if (length(functionalGroup) > 1) {
            stop("Can only provide 1 functionalGroup at a time, not ", length(theme))
          }
          if (!(functionalGroup %in% unique(om$functional_group))) {
            stop("No projects have functionalGroup ", functionalGroup, " try ", paste0(unique(om$functional_group), collapse=","), " instead.")
          }
          crab <- om[which(om$functional_group == functionalGroup),]
          if (length(crab) == 0) {
            stop("No projects have functionalGroup ", functionalGroup, " try ", paste0(unique(om$functional_group), collapse=","), " instead.")
          }
        } else if (!(is.null(section))) {
          if (length(section) > 1) {
            stop("Can only provide 1 section at a time, not ", length(section))
          }
          if (!(section %in% unique(sec))) {
            stop("No projects have section ",section, " try ", paste0(sec, collapse=","), " instead.")
          }

          crab <- om[which(gsub(".*- ","",om$section_display) == section),]
          if (length(crab) == 0) {
            stop("No projects have section ", section, " try ", paste0(sec, collapse=","), " instead.")
          }
        } else if (!(is.null(division))) {
          if (length(division) > 1) {
            stop("Can only provide 1 division at a time, not ", length(division))
          }
          div <- NULL
          for (i in seq_along(om$section_display)) {
            div[[i]] <- strsplit(om$section_display[i], " - ", fixed=TRUE)[[1]][3]
          }
          div <- unlist(unique(div))
          #div <- div[-(which(is.na(div)))]
          if (!(division %in% unique(div))) {
            stop("No projects have division ",division, " try ", paste0(div, collapse=","), " instead.")
          }
          crab <- om[which(unlist(lapply(strsplit(om$section_display, " - ", fixed=TRUE), function(x) x[3])) == division),]
          if (length(crab) == 0) {
            stop("No projects have division ", division, " try ", paste0(div, collapse=","), " instead.")
          }
        } else {
          crab <- om
        }

      if (length(crab$amount) == 0) {
        stop("No O&M Cost was associated with this project.")
      }
      keep <-
        subset(
          crab,
          select = c(
            project_id,
            category_display,
            project_year_id,
            amount,
            funding_source_display,
            fiscal_year,
            category_type,
            deliverables,
            milestones
          )
        )

      if (is.null(year)) {
        years <- unique(keep$fiscal_year)
        years <- years[order(as.numeric(gsub(".*-","",years)))]
      } else {
        years <- year
      }

      m <-
        matrix(0, nrow = length(unique(keep$funding_source_display)), ncol = length(years))
      df <-
        as.data.frame(m, row.names = unique(keep$funding_source_display))
      names(df) <- years

      namesFunding <- unique(keep$funding_source_display)

      if (!(is.null(year)) && !(year %in% unique(keep$fiscal_year))) {
        stop("This project does not have data for ",
             year,
             " try ",
             paste0(unique(keep$fiscal_year), collapse = ","))
      }

      # Fill in corresponding values
      for (i in seq_along(years)) {
        for (j in seq_along(namesFunding)) {
          value <-
            keep[which(keep$fiscal_year == years[i]), ] # Look at one year
          value <-
            sum(value$amount[which(value$funding_source_display == namesFunding[j])])
          #message("value = ", value, " for i = ",i, " and j = ",j)
          df[paste0(namesFunding[j]), paste0(years[i])] <- value
        }
      }

      # omBar
      if (which %in% c("omBar", "predictOM")) {
        if (length(unique(keep$amount)) == 1 && unique(keep$amount == 0) == TRUE) {
          stop("No O&M Cost was associated with this project.")
        }
        ylim <- NULL
        dv <- data.frame(matrix(NA, nrow = 1, ncol = length(years)))
        names(dv) <- years
        ms <- data.frame(matrix(NA, nrow = 1, ncol = length(years)))
        names(ms) <- years

        for (i in seq_along(years)) {
          ylim[[i]] <- sum(subset(
            df, select = c(paste0(years[i]))
          ))
          value <- keep[which(keep$fiscal_year == years[i]),]

          # Deliverables
          if (is.null(theme) && is.null(functionalGroup) && is.null(section) && is.null(division)) {
            if (all(value$deliverables == 0)) {
            dv[[i]] <- 0
          } else {
          dv[[i]] <- length(unlist(strsplit(unique(value$deliverables), "|-----|", fixed=TRUE)))
          }

          # Milestones
          if (all(value$milestones == 0)) {
            ms[[i]] <- 0
          } else {
            ms[[i]] <- length(unlist(strsplit(unique(value$milestones), "|-----|", fixed=TRUE)))
          }
          }
        }
        # Creating place for legend
        if (which == "omBar") {
        par(mar = c(5, 5, 0.6, 8) + 0.3, xpd=TRUE)
        if (is.null(theme) && is.null(functionalGroup) && is.null(section) && is.null(division)) {
          if (length(years) > 1) {
            df <- df[sort(rownames(df)), ]
          } else {
            d <-
              data.frame(matrix(NA, nrow = length(namesFunding), ncol = length(years)))
            names(d) <- years
            rownames(d) <- sort(namesFunding)
            d[, 1] <- df[sort(rownames(df)), ]
            df <- d
          }
        barplot(
          as.matrix(df),
          col = col[which(sort(unique(om$funding_source_display)) %in% sort(namesFunding))],
          ylab = " ",
          ylim = c(0, max(unlist(ylim))*1.5),
          xlab = " ",
          las=2,
          legend.text = TRUE,
          args.legend=list(x="bottomright", legend=wrapText(string=sort(namesFunding)), inset=c(-0.35,0), cex=0.7, bty="n")
        )
        title(ylab = "Amount of O&M Funding ($)", mgp = c(4, 1, 0))
        combine <- c(unlist(unname(ms)), unlist(unname(dv)))
        # Add deliverables and milestones
        par(new=TRUE)
        plot(
          1:length(dv),
          unlist(unname(dv)),
          type = "o",
          pch = 20,
          axes = FALSE,
          xlab = "",
          ylab = "",
          ylim = c(min(combine)-(max(combine)-min(combine))*3, (max(unlist(unname(ms)), unlist(unname(dv))))),
          col = "blue"
        )
        text(
          x = 1:length(dv),
          y = unlist(unname(dv)),
          labels = unlist(unname(dv)),
          pos = 3,
          cex = 0.5,
          col="blue"
        )
        par(new=TRUE)
        plot(
          1:length(ms),
          unlist(unname(ms)),
          type = "o",
          pch = 20,
          axes = FALSE,
          xlab = "",
          ylab = "",
          ylim = c(min(combine)-(max(combine)-min(combine))*3, (max(unlist(unname(ms)), unlist(unname(dv))))),
          col = "red"
        )
        text(
          x = 1:length(ms),
          y = unlist(unname(ms)),
          labels = unlist(unname(ms)),
          pos = 3,
          cex = 0.5,
          col="red"
        )

        if (dataframe == TRUE) {
          return(df)
        }

        } else {
          # Configure margins for theme plot
          if (length(years) > 6) {
          holder <- data.frame(matrix(0, nrow = length(df[,1]), 1))
          names(holder) <- c("  ")
          DF <- cbind(df, holder)
          } else {
            DF <- df
          }
          DF <- DF[sort(rownames(DF)),]
          barplot(
            as.matrix(DF),
            col = col[which(sort(unique(om$funding_source_display)) %in% sort(namesFunding))],
            ylab = " ",
            ylim = c(0, max(unlist(ylim))*1.5),
            xlab = " ",
            las=2,
            legend.text = TRUE,
            args.legend=list(x="topright", inset=c(-0.35,0), cex=0.5)
          )
          title(ylab = "Amount of O&M Funding ($)", mgp = c(4, 1, 0))

          if (dataframe == TRUE) {
            return(DF)
          }
        }

      } else if (which == "predictOM") {
        # Only considering years up until the current fiscal date.
        if (as.numeric(str_extract(max(years), ".+?(?=-)")) > as.numeric(str_extract(Sys.time(), ".+?(?=-)"))) {
          fiscalyear <- as.numeric(str_extract(Sys.time(), ".+?(?=-)"))
          firstyears <- as.numeric(str_extract(names(df), ".+?(?=-)"))
          current <- which(abs(firstyears - fiscalyear) == min(abs(firstyears - fiscalyear)))  # Find closest year
          df <- df[1:current]
          years <- years[1:current]
        }


        ly <-
          as.numeric(gsub("^[^-]*-\\s*([^.]+).*", "\\1", years[length(years)])) # Getting last number of last year (ie. 2023-2024)
        lys <- as.numeric(ly) + 1:2
        lys2 <- c(ly, lys)
        newyear <- paste0(lys2, "-", as.numeric(lys2) + 1)

        DF <- data.frame(matrix(NA, nrow = length(df[,1]), ncol = length(lys2)))
        names(DF) <- newyear

        for (i in seq_along(DF)) {
          m <- 0.02*i
          DF[,i] <- unlist(unname(df[length(years)]))*(1+m)
        }

        dfs <- cbind(df, DF)

        # Add in gap
        gap <- data.frame(matrix(NA, nrow = length(df[,1]), ncol = length(names(dfs))))
        names(gap) <- names(dfs)
        gap[1:length(years)] <- 0

        newGap <- data.frame(matrix(NA, nrow = length(df[,1]), ncol = length(newyear)))
        names(newGap) <- newyear

        for (i in seq_along(newyear)) {
          for (j in seq_along(namesFunding)) {
            old <- unname(unlist(dfs[(length(years))]))[j]
            new <- unname(unlist(dfs[(length(years) + i)]))[j]
            value <- new-old
            newGap[,i][j] <- value
          }
        }
        gap[(length(years)+1):length(names(gap))] <- newGap

        DFs <- rbind(dfs, gap)
        rownames(DFs) <- c(namesFunding, paste0(namesFunding, " GAP"))

        # Restructure gap
        DFs <- DFs[sort(row.names(DFs)),]
        par(mfrow=c(1,1), mar = c(5, 5, 1.5, 4) + 0.1)

        sums <- NULL
        for (i in seq_along(DFs[which(grepl("GAP", row.names(DFs))),][(length(years)+1):length(names(DFs))])) {
          sums[[i]] <- round(sum(as.numeric(unlist(unname(DFs[which(grepl("GAP", row.names(DFs))),][(length(years)+1):length(names(DFs))][i])))),0)
        }
        sums2 <- NULL
        for (i in seq_along(DFs)) {
          sums2[[i]] <- round(sum(as.numeric(unlist(unname(DFs[i])))),0)
        }
        cc <- which(sort(unique(om$funding_source_display)) %in% sort(namesFunding))
        j <- sort(rep(as.character(which(sort(unique(om$funding_source_display)) %in% sort(namesFunding))),2))
        j <- col[as.numeric(j)]
        j[which(!(seq_along(j) %in% seq(from=1, to=length(j), by=2)))] <- "red"
        DFs <- DFs[sort(rownames(DFs)),]
        if (is.null(theme) && is.null(functionalGroup) && is.null(division) && is.null(section)) {
          bp <-
            barplot(
              as.matrix(DFs),
              col = j,
              ylim = c(0, max(unlist(sums2))*2),
              xlab = " ",
              las = 2,
              ylab = " ",
              cex.names=0.8
            )
          legend(
            "topleft",
            wrapText(c(rownames(DFs)[-(which(grepl("GAP", rownames(DFs))))], "Gap in funding")),
            col = c(j[which(!(j == "red"))], "red"),
            pch = rep(20, (length(namesFunding)+1)),
            cex = 0.7
          )
        } else {
          if (length(years) == 1) {
            # 1
            l <- 1
          } else if (length(years) > 1 && length(years) < 5) {
            # 2,3,4
            l <- 2
          } else if (length(years) > 4 && length(years) < 7) {
            # 5, 6
            l <- 3
          } else if (length(years) > 6) {
            # To move further away, make l bigger
            # 7
            l <- 8
          }

          holder <- data.frame(matrix(0, nrow = length(DFs[,1]), l))
          names(holder) <- c("  ")
          DF <- cbind(DFs, holder)
          DF <- DF[sort(rownames(DF)),]
          j <- sort(rep(which(sort(unique(om$funding_source_display)) %in% sort(namesFunding)),2))
          j <- col[as.numeric(j)]
          j[which(!(seq_along(j) %in% seq(from=1, to=length(j), by=2)))] <- "red"
          bp <-
            barplot(
              as.matrix(DF),
              col = j,
              ylim = c(0, max(unlist(sums2))*1.3),
              xlab = " ",
              las = 2,
              ylab = " ",
              cex.names=0.8,
            )
           # Put legend labels on new line
          string <- rownames(DFs)[-(which(grepl("GAP", rownames(DFs))))]
          strings <- wrapText(string=string, nchar=20)

          legend(
            "topright",
            c(strings, "Gap in funding"),
            col = c(j[which(!(j == "red"))], "red"),
            pch = rep(15, (length(namesFunding)+1)),
            cex = 0.55
          )

        }
        title(ylab = "Amount of O&M Funding ($)", mgp = c(4, 1, 0))
        abline(v = mean(bp[length(years):(length(years) + 1)]), col = "red", lty=3)
        m <- max(unlist(sums2)[1:length(years)])
        pr <- max(unlist(sums2[(length(years)+1):(length(sums2))]))
        y <- unname(quantile(c(m,pr), .15))
        #y <- median(c(m,pr))
        if (m-pr < 0) {
          y <- pr+(max(unlist(sums2))/20)
        }
        text(
          x = bp[(length(years)+1):length(names(DFs))],
          y = y,
          labels = paste0("$", sums),
          pos = 3,
          cex = 0.65,
          col="red",
          srt=90
        )

        if (dataframe == TRUE) {
          return(DFs)
        }

      }
      } else if (which == "omPie") {
        df2 <- df # Create storage
        for (i in seq_along(years)) {
          for (j in seq_along(namesFunding)) {
            df2[paste0(namesFunding[j]), paste0(years[i])] <-
              100 * (df[paste0(namesFunding[j]), paste0(years[i])] / sum(df[paste0(years[i])]))
          }
        }

        par(mfrow = c(1, length(years)))
        for (i in seq_along(years)) {
          pie(
            unname(unlist(df2[paste0(years[i])])),
            col = c(1:length(namesFunding)),
            labels = paste0(round(unname(
              unlist(df2[paste0(years[i])])
            ), 2), "%")
          )
          title(paste0(years[i]))
        }
        legend(
          "bottomright",
          c(namesFunding),
          col = c(1:length(namesFunding)),
          pch = rep(20, length(namesFunding)),
          cex = 0.7
        )
        if (dataframe == TRUE) {
          return(df2)
        }

      } else if (which == "omAllocation") {
        category <- unique(keep$category_display)
        years <- unique(keep$fiscal_year)
        years <- years[order(as.numeric(gsub(".*-","",years)))]
        yearsx <- vector(mode="list", length(category))
        amounty <- vector(mode="list", length(category))
        for (i in seq_along(category)) {
          for (j in seq_along(years)) {
            value <- keep[which(keep$category_display == category[i]),] # Look at one category
            value2 <- value[which(value$fiscal_year == years[j]),] # Look at specific year
            if (!(sum(value2$amount, na.rm=TRUE) == 0)) {
            yearsx[[i]][[j]] <- unique(value2$fiscal_year)
            amounty[[i]][[j]] <-sum(value2$amount, na.rm=TRUE)
            } else {
              amounty[[i]][[j]] <- 1 # This is a placeholder
              yearsx[[i]][[j]] <- 1 # This is a placeholder

            }
          }
        }
        # unlisting
        for (i in seq_along(amounty)){
          amounty[[i]] <- unlist(amounty[[i]])
        }

        for (i in seq_along(yearsx)){
          yearsx[[i]] <- unlist(yearsx[[i]])
        }

        names(yearsx) <- category
        names(amounty) <- category
        if (length(unique(keep$amount)) == 1 && unique(keep$amount == 0) == TRUE) {
          stop("No o&m costs have been allocated to this project.")
        }

        bad <- NULL
        for (i in seq_along(amounty)) {
          yearsx[[i]] <- yearsx[[i]][which(!(amounty[[i]] == 1))]
          amounty[[i]] <- amounty[[i]][which(!(amounty[[i]] == 1))]
          if (length(amounty[[i]]) == 0) {
            bad[[i]] <- i
          }
        }
        if (!(is.null(unlist(bad)))) {
        amounty <- amounty[-(unlist(bad))]
        yearsx <- yearsx[-(unlist(bad))]
        category <- category[-(unlist(bad))]
        }

        max <- NULL
        for (i in seq_along(amounty)) {
          if (!(identical(amounty[[i]],numeric(0)))) {
          max[[i]] <- max(amounty[[i]])
          } else {
            max[[i]] <- numeric(0)
          }
        }

        max <- max(unlist(unname(max))) # Find ylim
        for (i in seq_along(yearsx)) {
          for (j in seq_along(yearsx[[i]])) {
            yearsx[[i]][[j]] <- which(years == yearsx[[i]][[j]])
          }
        }

        # Setting layout
        if (is.null(theme) && is.null(functionalGroup) && is.null(section) && is.null(division) && !(is.null(id))) {
        if (length(amounty) == 1) {
          par(mfrow = c(1, 1), mar = c(2, 4, 2, 0.5))
        } else {
          par(mfrow = c(ceiling((length(amounty) / 2)), 2), mar = c(2, 4, 2, 0.5))
        }
        } else {
          par(mfrow=c(1,1))
        }

        if (!(is.null(item))) {
          if (!(any(item %in% category)) | (!(length(unique(item %in% category)) == 1))) {
            stop("Item : ", paste0(item, collapse=","), " is not funded. Try one of the following instead ", paste0(category, collapse=","))
          }
          category <- category[which(category %in% item)]
          amounty <- amounty[which(names(amounty) %in% item)]
          yearsx <- yearsx[which(names(yearsx) %in% item)]
        }

        for (i in seq_along(category)) {
          if (!(identical(amounty[[i]], numeric(0)))) {
          if (length(amounty[[i]]) == 1) {
            ylim <- c((amounty[[i]]-1000), (amounty[[i]]+1000))
          } else {
            ylim <- extendrange(range(amounty[[i]], na.rm=TRUE))
          }
            if (!(dataframe)) {
          plot(seq_along(unique(keep$fiscal_year)), rep(mean(ylim), length(unique(keep$fiscal_year))), col="white", xlab=" ", ylab= "", xaxt="n",ylim=ylim)
          lines(yearsx[[i]], unlist(amounty[[i]]), type="o", pch=20, col="black", xlab=" ", ylab=" ", xaxt="n")
          axis(
            1,
            at = seq_along(years),
            labels = years,
            #las = 2,
            cex.axis = 0.7
          )
          if (!(length(amounty[[i]]) == 1)) {
          m <- lm(unlist(amounty[[i]])~seq_along(yearsx[[i]]))
          mtext(paste0(round(coef(m)[2],0), " $/year"), col="red", line=0, cex=0.5, at=par("usr")[1]+0.9*diff(par("usr")[1:2]))
          }
          if (category[i] == "International travel for meetings, science collaboration and conferences") {
            title("International travel", cex.main=0.85)

          } else {
          title(category[i], cex.main=0.85)
          }
            }
          }
        }
        if (!(dataframe)) {
        mtext(side=2, text="Amount of O&M Funding ($)", line=-1.3, outer=TRUE)
        }

        if (dataframe == TRUE) {
          return(amounty)
        }

      } else if (which == "omAllocationGeneral") {
        par(mfrow = c(1, length(unique(years))))
        DFG <- NULL
        for (i in seq_along(years)) {
          value <-
            keep[which(keep$fiscal_year == years[i]), ] # Look at one year
          mg <- matrix(0, nrow = 1, ncol = length(unique(value$category_type)))
          gdf <- as.data.frame(mg, col.names = unique(value$category_type))
          names(gdf) <- unique(value$category_type)
          for (j in seq_along(unique(value$category_type))) {
            gdf[j] <-
              sum(value$amount[which(value$category_type == unique(value$category_type)[j])], na.rm =
                    TRUE)
          }
          # Fill in values
          sum <- sum(unlist(unname(gdf)), na.rm = TRUE)
          labels <- paste0(round(100 * (unname(
            unlist(gdf)
          ) / sum), 2), " %")
          pie(
            unname(unlist(gdf)),
            labels <-
              labels,
            col = 1:length(unique(value$category_type)),
            radius = 1
          )
          title(paste0(years[i]))
          legend(
            "topleft",
            c(unique(value$category_type)),
            col = c(1:length(unique(
              value$category_type
            ))),
            pch = rep(20, length(unique(
              value$category_type
            ))),
            cex = 0.7
          )
          DFG[[i]] <- gdf
        }
        if (dataframe == TRUE) {
          return(DFG)
        }
      }

    }

    if (which %in% c(
      "salaryBar",
      "salaryAllocation",
      "weekAllocation",
      "indeterminate",
      "predictSummary",
      "predict",
      'predictSalary',
      'overviewInvestment'
    )) {
      if (debug > 0) {
        message("salary has been identified")
      }

      if (is.null(salary)) {
        stop("Must provide a salary argument")
      }
      # This is now salary
      if (debug > 0) {
        message("The names of salary is ", paste0(names(salary), sep = ","))
      }
      if (!(identical(
        c("id", "overtime_hours","smart_name",
          "duration_weeks", "level_display", "funding_source_display",
          "employee_type_display",  "project_year_id","project_id",
          "fiscal_year", "project_title", "median_salary",
          "salary_per_week","amount_week","amount_overtime",
          "amount_total", "theme", "activity_type",
          "functional_group","section_display","overview",
          "objectives","tag", "tag_id",
          "status","lead_staff", "deliverables",
          "milestones","section_id","division_id",
          "region_id", "funding_id","theme_id",
          "staff_id",
          "branch_id"),
        names(salary)
      ))) {
        stop("Must obtain data for x using getData(type='salary')")
      }

      # color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)] # Getting random colors
      # set.seed(1)
      # col <- sample(color, length(unique(salary$funding_source_display)))

      # Theme
      if (!(is.null(theme))) {
        id <- NULL
        functionalGroup <- NULL
        division <- NULL
        section <- NULL
      }

      if (!(is.null(id))) {
        salaryKeep <- salary[which(salary$project_id == id),]
      } else if (!(is.null(theme))) {
        if (!(theme %in% unique(salary$theme))) {
          stop("No projects have theme ", theme, " try ", paste0(unique(salary$theme), collapse=","), " instead.")
        }
        salaryKeep <- salary[which(salary$theme == theme),]
        if (length(salaryKeep) == 0) {
          stop("No projects have theme ", theme, " try ", paste0(unique(salary$theme), collapse=","), " instead.")
        }
      } else if (!(is.null(functionalGroup))) {

        if (!(functionalGroup %in% unique(salary$functional_group))) {
          stop("No projects have functionalGroup ", functionalGroup, " try ", paste0(unique(salary$functional_group), collapse=","), " instead.")
        }
        salaryKeep <- salary[which(salary$functional_group == functionalGroup),]
        if (length(salaryKeep) == 0) {
          stop("No projects have functionalGroup ", functionalGroup, " try ", paste0(unique(salary$functionalGroup), collapse=","), " instead.")
        }
      } else if (!(is.null(section))) {

        if (!(section %in% unique(sec))) {
          stop("No projects have section ", section, " try ", paste0(unique(sec), collapse=","), " instead.")
        }
        salaryKeep <- salary[which(gsub(".*- ","",salary$section_display) == section),]

        if (length(salaryKeep) == 0) {
          stop("No projects have section ", section, " try ", paste0(unique(sec), collapse=","), " instead.")
        }

      } else if (!(is.null(division))) {
        if (length(division) > 1) {
          stop("Can only provide 1 division at a time, not ", length(division))
        }
        div <- NULL
        for (i in seq_along(salary$section_display)) {
          div[[i]] <- strsplit(salary$section_display[i], " - ", fixed=TRUE)[[1]][3]
        }
        div <- unlist(unique(div))
        #div <- div[-(which(is.na(div)))]
        if (!(division %in% unique(div))) {
          stop("No projects have division ",division, " try ", paste0(div, collapse=","), " instead.")
        }
        salaryKeep <- salary[which(unlist(lapply(strsplit(salary$section_display, " - ", fixed=TRUE), function(x) x[3])) == division),]
        if (length(salaryKeep) == 0) {
          stop("No projects have division ", division, " try ", paste0(div, collapse=","), " instead.")
        }

      } else {
        salaryKeep <- salary
      }

      if (is.null(year)) {
        salyears <- unique(salaryKeep$fiscal_year)
        salyears <- salyears[order(as.numeric(gsub(".*-","",salyears)))]

      } else {
        salyears <- year
      }

      if (!(is.null(year)) &&
          !(year %in% unique(salaryKeep$fiscal_year))) {
        stop("This project does not have data for ",
             year,
             " try ",
             paste0(unique(salaryKeep$fiscal_year), collapse = ","))
      }

      if (length(salaryKeep$amount_total) == 0) {
        stop("No salary cost was associated with this project.")
      }

      salm <-
        matrix(0, nrow = length(unique(salaryKeep$funding_source_display)), ncol =
                 length(salyears))
      saldf <-
        as.data.frame(salm, row.names = unique(salaryKeep$funding_source_display))
      names(saldf) <- salyears

      salnamesFunding <- unique(salaryKeep$funding_source_display)

      # Fill in corresponding values
      # NOTE: Sometimes overtime is 1.5 or 2 times. This does not account for that.
      for (i in seq_along(salyears)) {
        for (j in seq_along(salnamesFunding)) {
          value <-
            salaryKeep[which(salaryKeep$fiscal_year == salyears[i]), ] # Look at one year
          value2 <-
            value[which(value$funding_source_display == salnamesFunding[j]), ]
          totalSum <-
            sum(value2$amount_total[which(is.finite(value2$amount_total))], na.rm =
                  TRUE)
          saldf[paste0(salnamesFunding[j]), paste0(salyears[i])] <- totalSum
        }
      }

      if (which == "salaryBar") {
        par(mfrow = c(1, 1))
        par(mar = c(5, 5, 0.6, 8) + 0.3, xpd=TRUE)
        ylim <- NULL
        for (i in seq_along(salyears)) {
          ylim[[i]] <- sum(subset(
            saldf, select = c(paste0(salyears[i]))
          ))
        }
        if (length(unique(unlist(unname(saldf)))) == 1) {
          if (unique(unlist(unname(saldf))) == 0) {
            stop("No time spent on this project")
          }
        }
        if (is.null(theme) && is.null(functionalGroup) && is.null(section) && is.null(division)) {
        barplot(
          as.matrix(saldf),
          col = col[which(sort(unique(salary$funding_source_display)) %in% sort(salnamesFunding))],
          #col = col,
          ylab = " ",
          ylim = c(0, max(unlist(ylim))*2),
          xlab = " ",
          las=2,
          legend.text = TRUE,
          args.legend=list(x="bottomright", legend=wrapText(sort(salnamesFunding)), inset=c(-0.35,0), cex=0.7),bty="n")
        } else {
          if (length(salyears) > 6) {
          holder <- data.frame(matrix(0, nrow = length(saldf[,1]), 1))
          names(holder) <- c("  ")
          DF <- cbind(saldf, holder)
          } else {
            DF <- saldf
          }

          if (length(salyears) > 1) {
            DF <- DF[sort(rownames(DF)), ]
          } else {
            d <-
              data.frame(matrix(NA, nrow = length(salnamesFunding), ncol = length(salyears)))
            names(d) <- salyears
            rownames(d) <- sort(salnamesFunding)
            d[, 1] <- DF[sort(rownames(DF)), ][1]
            DF <- d
          }
          barplot(
            as.matrix(DF),
            col = col[which(sort(unique(salary$funding_source_display)) %in% sort(salnamesFunding))],
            ylab = " ",
            ylim = c(0, max(unlist(ylim))*2),
            legend.text = TRUE,
            xlab = " ",
            args.legend=list(x="topright", legend=wrapText(sort(salnamesFunding)),inset=c(-0.35,0), cex=0.5,bty="n"),
            las=2
          )

        }
        title(ylab = "Amount of Salary Funding ($)", mgp = c(4, 1, 0))
        # legend(
        #   "topleft",
        #   c(salnamesFunding),
        #   col = c(1:length(salnamesFunding)),
        #   pch = rep(20, length(salnamesFunding)),
        #   cex = 0.7
        # )
        if (dataframe == TRUE) {
          return(saldf)
        }
      } else if (which %in% "salaryAllocation") {
        category <- unique(salaryKeep$level_display)
        salyears <- salyears[order(as.numeric(gsub(".*-","",salyears)))]
        yearsx <- vector(mode="list", length(category))
        amounty <- vector(mode="list", length(category))
        for (i in seq_along(category)) {
          for (j in seq_along(salyears)) {
            value <- salaryKeep[which(salaryKeep$level_display == category[i]),] # Look at one category
            value2 <- value[which(value$fiscal_year == salyears[j]),] # Look at specific year
            if (!(sum(value2$amount_total, na.rm=TRUE) == 0)) {
              yearsx[[i]][[j]] <- unique(value2$fiscal_year)
              amounty[[i]][[j]] <-sum(value2$amount_total, na.rm=TRUE)
            } else {
              amounty[[i]][[j]] <- 1 # This is a placeholder
              yearsx[[i]][[j]] <- 1 # This is a placeholder

            }
          }
        }
        # unlisting
        for (i in seq_along(amounty)){
          amounty[[i]] <- unlist(amounty[[i]])
        }

        for (i in seq_along(yearsx)){
          yearsx[[i]] <- unlist(yearsx[[i]])
        }
        names(yearsx) <- category
        names(amounty) <- category
        bad <- NULL
        for (i in seq_along(amounty)) {
          yearsx[[i]] <- yearsx[[i]][which(!(amounty[[i]] == 1))]
          amounty[[i]] <- amounty[[i]][which(!(amounty[[i]] == 1))]
          if (length(amounty[[i]]) == 0) {
            bad[[i]] <- i
          }
        }
        if (!(is.null(unlist(bad)))) {
          amounty <- amounty[-(unlist(bad))]
          yearsx <- yearsx[-(unlist(bad))]
          category <- category[-(unlist(bad))]
        }
        if (length(amounty) == 0) {
          stop("No time spent on this project.")
        }

        max <- NULL
        for (i in seq_along(amounty)) {
          if (!(identical(amounty[[i]],numeric(0)))) {
            max[[i]] <- max(amounty[[i]])
          } else {
            max[[i]] <- numeric(0)
          }
        }

        max <- max(unlist(unname(max))) # Find ylim
        for (i in seq_along(yearsx)) {
          for (j in seq_along(yearsx[[i]])) {
            yearsx[[i]][[j]] <- which(salyears == yearsx[[i]][[j]])
          }
        }

        par(mar = c(2, 4, 2, 0.5))
        # Setting layout
        if (is.null(theme) && is.null(functionalGroup) && is.null(section) && is.null(division) && !(is.null(id))) {
        if (length(amounty) == 1) {
          par(mfrow = c(1, 1))
        } else {
          par(mfrow = c(ceiling((length(amounty) / 2)), 2))
        }
        } else {
          par(mfrow=c(1,1))
        }


        if (!(is.null(item))) {
          if (!(any(item %in% category)) | (!(length(unique(item %in% category)) == 1))) {
            stop("Item : ", paste0(item, collapse=","), " is not funded. Try one of the following instead ", paste0(category, collapse=","))
          }
          category <- category[which(category %in% item)]
          amounty <- amounty[which(names(amounty) %in% item)]
          yearsx <- yearsx[which(names(yearsx) %in% item)]
        }


        for (i in seq_along(category)) {
          if (!(identical(amounty[[i]], numeric(0)))) {
            if (length(amounty[[i]]) == 1) {
              ylim <- c((amounty[[i]]-1000), (amounty[[i]]+1000))
            } else {
              ylim <- extendrange(range(amounty[[i]], na.rm=TRUE))
            }
            if (!(dataframe)) {
            plot(seq_along(unique(salaryKeep$fiscal_year)), rep(mean(ylim), length(unique(salaryKeep$fiscal_year))), col="white", xlab=" ", ylab= "", xaxt="n",ylim=ylim)
            lines(yearsx[[i]], unlist(amounty[[i]]), type="o", pch=20, col="black", xlab=" ", ylab=" ", xaxt="n")
            axis(
              1,
              at = seq_along(salyears),
              labels = salyears,
              #las = 2,
              cex.axis = 0.7
            )
            if (!(length(amounty[[i]]) == 1)) {
              m <- lm(unlist(amounty[[i]])~seq_along(yearsx[[i]]))
              mtext(paste0(round(coef(m)[2],0), " $/year"), col="red", line=0, cex=0.5, at=par("usr")[1]+0.9*diff(par("usr")[1:2]))
            }
            if (category[i] == "International travel for meetings, science collaboration and conferences") {
              title("International travel", cex.main=0.85)

            } else {
              title(category[i], cex.main=0.85)
            }
            }
          }
        }
        if (!(dataframe)) {
        mtext(side=2, text="Amount of Salary Funding ($)", line=-1.3, outer=TRUE)
        }

        if (dataframe == TRUE) {
          return(amounty)
        }

      } else if (which %in% c("weekAllocation", "predictSalary")) {
        # STEP ONE (PREDICTSALARY): LOOP ALL YEARS TO OBTAIN WEEKS FOR LEVEL CLASSIFICATION
        par(mfrow = c(1, length(salyears)))
        DFL2 <- NULL
        for (i in seq_along(salyears)) {
          value <-
            salaryKeep[which(salaryKeep$fiscal_year == salyears[i]), ] # Look at one year
          ml2 <- matrix(0, nrow = 1, ncol = length(unique(value$level_display)))
          dfl2 <- as.data.frame(ml2, col.names = unique(value$level_display))
          names(dfl2) <- unique(value$level_display)
          for (j in seq_along(unique(value$level_display))) {
            dfl2[j] <-
              sum(value$duration_weeks[which(value$level_display == unique(value$level_display)[j])], na.rm =
                    TRUE)
          }
          DFL2[[i]] <- dfl2
          # Fill in values
          if (which == "weekAllocation") {
            barplot(
              as.matrix(dfl2),
              col = 1,
              ylab = "Time (weeks)",
              xlab = "Job Classification"
            )
            title(paste0(salyears[i]))
          }

        }

        if (dataframe == TRUE && which == "weekAllocation") {
          return(DFL2)
        }

      } else if (which == "indeterminate") {
        par(mfrow = c(1, length(salyears)))
        DFI <- NULL

        bad <- ifelse(length(unique(salaryKeep$duration_weeks)) == 1 && unique(salaryKeep$duration_weeks) == 0, TRUE, FALSE)
        for (i in seq_along(salyears)) {
          value <-
            salaryKeep[which(salaryKeep$fiscal_year == salyears[i]), ] # Look at one year
          mi <- matrix(0, nrow = 1, ncol = 2)
          dfi <-
            as.data.frame(mi, col.names = c("Indeterminate", "Non-indeterminate"))
          names(dfi) <- c("Indeterminate", "Non-indeterminate")
          dfi[1] <-
            sum(value$amount_total[which(grepl("Indeterminate", value$employee_type_display))], na.rm =
                  TRUE)
          dfi[2] <-
            sum(value$amount_total[which(!(grepl(
              "Indeterminate", value$employee_type_display
            )))], na.rm = TRUE)

          # Fill in values
          int <-
            round(unlist(unname(dfi[1])) / sum(value$amount_total, na.rm = TRUE) *
                    100,
                  2)
          non <-
            round(unlist(unname(dfi[2])) / sum(value$amount_total, na.rm = TRUE) *
                    100,
                  2)

          INT <-
            length(value$id[which(grepl("Indeterminate", value$employee_type_display))]) # For # of employees
          NON <-
            length(value$id[which(!(grepl(
              "Indeterminate", value$employee_type_display
            )))])
          dfi[1] <- int
          dfi[2] <- non
          par(mar = c(12, 4, 4, 2) + 0.1)
          if (!(bad)) {
          if (i == 1) {
          b <- barplot(as.matrix(dfi), las=2, ylim=c(0,120), ylab= "Percent (%)", cex.names=ifelse(length(salyears) > 4, 0.8,1))
          text(x= b, y=1:2,pos = 3, label = c(INT,NON), cex = 1, col = "red")
          } else {
          b <- barplot(as.matrix(dfi), las=2, ylim=c(0,120), ylab= " ", cex.names=ifelse(length(salyears) > 4, 0.8,1))
          text(x= b, y=c(1,1),pos = 3, label = c(INT,NON), cex = 1, col = "red")
          }
          title(paste0(salyears[i]))
          DFI[[i]] <- dfi
          } else {
            dfi[1,] <- c(INT,NON)
            if (i == 1) {
              b <- barplot(as.matrix(dfi), las=2, ylim=c(0,(max(INT,NON)+3)), ylab= "Number of Employees", cex.names=ifelse(length(salyears) > 4, 0.8,1))
              text(x= b, y=c(1,1),pos = 3, label = c(INT,NON), cex = 1, col = "red")
            } else {
              b <- barplot(as.matrix(dfi), las=2, ylim=c(0,(max(INT,NON)+3)), ylab= " ", cex.names=ifelse(length(salyears) > 4, 0.8,1))
              text(x= b, y=c(1,1),pos = 3, label = c(INT,NON), cex = 1, col = "red")
            }
            title(paste0(salyears[i]))

          }
        }
        if (dataframe == TRUE) {
          return(DFI)
        }
      }

    }

    if (which %in% c("predictSummary", "predict")) {
      if (!(id == 1093)) {
        stop("This code is just being started and only works for snow crab project (i.e. id=1093)")
      }

      if (is.null(funding)) {
        stop(
          'must specify funding argument. Choose one of the following: ',
          paste0(unique(keep$funding_source_display), collapse = ",")
        )
      }

      if (is.null(fundingChange) && is.null(endDate)) {
        stop("must specify fundingChange")
      }

      if (!(length(funding) == length(fundingChange)) &&
          is.null(endDate)) {
        stop("funding and fundingChange must be the same length")
      }

      for (i in seq_along(funding)) {
        if (!(funding[i] %in% unique(keep$funding_source_display))) {
          stop(
            "funding ",
            funding[i],
            " did not fund stations in this project. Try : ",
            paste0(unique(keep$funding_source_display), collapse = " or ")
          )
        }
      }

      # Cost allocated to station (Vessels, Boats)
      stationFund <-
        unique(keep$funding_source_display[which(keep$category_display == "Vessels, Boats")])
      cost <-
        data.frame(matrix(NA, nrow = length(stationFund), ncol = length(years)))
      names(cost) <- years
      rownames(cost) <- stationFund
      for (i in seq_along(years)) {
        for (j in seq_along(stationFund)) {
          value <-
            keep[which(keep$fiscal_year == years[i]), ] # Look at one year
          value2 <-
            value[which(value$category_display == "Vessels, Boats"), ] # Look at one year
          cost[paste0(years[i])][, 1][j] <-
            sum(value2$amount[which(value2$funding_source_display == stationFund[j])], na.rm =
                  TRUE)
        }
      }
      # identifying the new years to add
      ly <-
        gsub("^[^-]*-\\s*([^.]+).*", "\\1", years[length(years)]) # Getting last number of last year (ie. 2023-2024)
      newyear <- paste0(ly, "-", as.numeric(ly) + 1)
      if (!(is.null(endDate))) {
        fundingChange <- -100
      }
      columnnames <- paste0(newyear, seq_along(funding))
      # Addressing changes in funding based on user input
      for (i in seq_along(funding)) {
        for (j in seq_along(columnnames)) {
          cc <-
            cost[years[length(years)]][funding[i], ] #isolating cost from last year of specified funding
          change <- cc * (fundingChange[j] / 100)
          changeAmount <- cc + change
          cost[columnnames[j]] <- NA
          cost[columnnames[j]][funding[i], ] <- changeAmount
        }
      }
      # Filling in value from previous year if user didn't specify a change
      for (j in seq_along(columnnames)) {
        same <- which(is.na(unname(unlist(cost[columnnames[j]]))))
        #message("For j = ", j, " same = ", same)
        cost[columnnames[j]][same, ] <- cost[years[length(years)]][same, ]
      }
      # right of figure
      station <- cost # Setting dimensions for station

      station[1, ] <- rep(390)
      station[2, ] <- rep(24)

      costPerStation <- cost[1:length(years)] / station[1:length(years)]

      # Now take the number of cost in the most recent year
      recent <- costPerStation[length(years)]
      for (i in seq_along(columnnames)) {
        for (j in seq_along(stationFund)) {
          station[columnnames[i]][j, ] <-
            cost[columnnames[i]][j, ] / recent[[1]][j]
        }
      }

      totalnames <- c(years, columnnames)

      # if we wanted color coded by funding this is where we would do it
      totalstations <-
        data.frame(matrix(NA, nrow = 1, ncol = length(totalnames)))
      names(totalstations) <- totalnames
      for (i in seq_along(totalnames)) {
        totalstations[totalnames[i]] <- sum(station[totalnames[i]])
      }
      if (which == "predictSummary") {
        # left of figure
        layout(matrix(c(1, 2), 1, 2, byrow = TRUE))
        bp <-
          barplot(
            as.matrix(cost),
            col = c(1:length(namesFunding)),
            ylim = c(0, sum(subset(
              df, select = c(paste0(years[1]))
            )) + 109000),
            las = 2,
            cex.names = 0.7,
            ylab = ""
          )
        abline(v = mean(bp[length(years):(length(years) + 1)]), col = "red", lty=3)
        title(ylab = "Amount of O&M Funding ($)", mgp = c(3.3, 1, 0))
        tot <- rep(40000, length(names(cost)))
        points(bp,
               tot,
               xpd = TRUE,
               col = c(rep(1, length(years)), 1 + (1:length(funding))),
               pch = 20)
        legend(
          "topright",
          c(namesFunding),
          col = c(1:length(namesFunding)),
          pch = rep(20, length(namesFunding)),
          cex = 0.6
        )
        p <- totalstations[1:length(years)]
        yearPoint <- unlist(unname(p))
        x <- 1:(length(years) + 1)
        stationLabels <- NULL
        for (i in seq_along(columnnames)) {
          y <- c(yearPoint, unname(unlist(totalstations[length(years) + i])))
          stationLabels[[i]] <- y[length(y)]
          if (i == 1) {
            xlim <- length(x) + 1
            plot(
              x,
              y,
              type = "o",
              ylab = "Number of Stations",
              pch = 20,
              col = i + 1,
              ylim = c(min(unlist(
                unname(totalstations)
              )) - 10, max(unlist(
                unname(totalstations)
              )) + 10),
              xaxt = "n",
              xlab = " ",
              xlim = c(0, xlim)
            )
            axis(
              1,
              at = seq_along(y),
              labels = c(years, newyear),
              las = 2,
              cex.axis = 0.7
            )
          } else {
            lines(x,
                  y,
                  col = i + 1,
                  type = "o",
                  pch = 20)
          }
        }
        abline(v = length(years), col = "red")
        labels <- paste0(funding, ",", fundingChange, "%")
        labels <- c("Status Quo", labels)
        y <- c(yearPoint, yearPoint[length(yearPoint)])
        lines(x,
              y,
              col = 1,
              type = "o",
              pch = 20)
        text(
          x = x[1:length(years)],
          y = y[1:length(years)],
          labels = y[1:length(years)],
          pos = 3,
          cex = 0.5
        )
        text(
          x = rep((length(years) + 1), length(columnnames) + 1),
          y = c(y[length(years) + 1], unlist(stationLabels)),
          labels = round(c(y[length(years) + 1], unlist(
            stationLabels
          )), 0),
          pos = 4,
          cex = 0.5
        )
        legend(
          "bottomleft",
          labels,
          col = 1:(length(x) + 1),
          pch = 20,
          cex = 0.7
        )

        if (dataframe == TRUE) {
          return(totalstations)
        }
      } else if (which == "predict") {
        if (is.null(endDate)) {
          par(mar = c(5, 5, 4, 4) + 0.3)
          ny2 <- as.numeric(gsub("^[^-]*-\\s*([^.]+).*", "\\1", newyear))
          newyear2 <- paste0(ny2, "-", ny2 + 1)
          ny3 <- as.numeric(gsub("^[^-]*-\\s*([^.]+).*", "\\1", newyear2))
          newyear3 <- paste0(ny3, "-", ny3 + 1)
          labels <- c(years, newyear, newyear2, newyear3)

          for (i in seq_along(fundingChange)) {
            # Each scenario gets a separate plot
            mat <-
              cbind(cost[1:length(years)], cost[length(years) + i], cost[length(years) +
                                                                           i], cost[length(years) + i])
            names(mat) <- labels
            bp <-
              barplot(
                as.matrix(mat),
                col = c(1:length(namesFunding)),
                ylim = c(0, sum(subset(
                  df, select = c(paste0(years[1]))
                )) + 109000),
                las = 2,
                cex.names = 0.7,
                ylab = ""
              )
            abline(v = mean(bp[length(years):(length(years) + 1)]), col = "red", lty=3)
            par(new = TRUE)
            ts <-
              cbind(totalstations[1:length(years)],
                    totalstations[length(years) + i],
                    totalstations[length(years) + i],
                    totalstations[length(years) + i])
            plot(
              1:length(ts),
              unlist(unname(ts)),
              type = "o",
              pch = 20,
              axes = FALSE,
              xlab = "",
              ylab = "",
              ylim = c(0, (max(
                unlist(unname(ts))
              ) * 1.15)),
              col = "blue"
            )
            text(
              x = 1:length(ts),
              y = unlist(unname(ts)),
              labels = unlist(unname(ts)),
              pos = 3,
              cex = 0.5
            )
            axis(
              side = 4,
              at = pretty(range(0, max(
                unlist(unname(ts))
              ))),
              col = "blue",
              col.ticks = "blue"
            )
            mtext(
              "Number of Stations",
              side = 4,
              line = 3,
              col = "blue"
            )
            title(ylab = "Amount of O&M Funding ($)", mgp = c(4, 1, 0))
          }
        } else {
          if (!(length(funding) == 1)) {
            stop(
              "Can only predict the ending of one funding at a time. Your funding argument has length = ",
              length(funding)
            )
          }

          # Determine how many years until ending
          ny2 <- as.numeric(gsub("^[^-]*-\\s*([^.]+).*", "\\1", newyear))
          endYears <- NULL
          for (i in seq_along(0:20)) {
            endYears[[i]] <- paste0(ny2 + i, "-", ny2 + (i + 1))
          }

          year1 <- as.numeric(gsub("(.+?)(\\-.*)", "\\1", endDate))
          year2 <- as.numeric(gsub("^[^-]*-\\s*([^.]+).*", "\\1", endDate))
          if (!(year2 - year1 == 1)) {
            stop("endDate must only be one year interval not ",
                 year2 - year1)
          }

          if (length(which(endYears %in% endDate)) == 0) {
            stop(
              "It's not possible to predict for endDate ",
              endDate,
              " Make
   sure to have it in YYYY-YYYY format. You also may on predict for
   up to 20 years in advance"
            )
          }
          labels <- c(years, newyear, endYears[1:which(endYears %in% endDate)])
          ey <- data.frame(matrix(NA, nrow = 2, ncol = length(labels)))
          names(ey) <- labels
          ey[1:length(years)] <- cost[1:length(years)]
          ey[(length(years) + 1):(length(labels) - 1)] <- cost[length(years)]
          rownames(ey) <- stationFund
          ey[length(labels)][paste0(funding), ] <- 0
          ey[length(labels)][which(!(stationFund %in% funding)), ] <-
            ey[length(labels) - 1][which(!(stationFund %in% funding)), ] # copying over non impacting funding


          # TEST NOW
          par(mar = c(5, 5, 4, 4) + 0.3)
          bp <-
            barplot(
              as.matrix(ey),
              col = c(1:length(namesFunding)),
              ylim = c(0, sum(subset(
                df, select = c(paste0(years[1]))
              )) + 109000),
              las = 2,
              cex.names = 0.7,
              ylab = ""
            )
          abline(v = mean(bp[length(years):(length(years) + 1)]), col = "red")

          # Creating number of stations

          ts <- data.frame(matrix(NA, nrow = 1, ncol = length(labels)))
          names(ts) <- labels
          ts[1:length(years)] <- totalstations[1:length(years)]
          ts[(length(years) + 1):(length(labels) - 1)] <-
            totalstations[length(years)]
          gone <- station[length(years)][which(stationFund %in% funding), ]
          ts[length(labels)] <-
            unlist(unname(totalstations[length(years)])) - gone
          par(new = TRUE)
          plot(
            1:length(ts),
            unlist(unname(ts)),
            type = "o",
            pch = 20,
            axes = FALSE,
            xlab = "",
            ylab = "",
            ylim = c(0, (max(
              unlist(unname(ts))
            ) * 1.15)),
            col = "blue"
          )
          text(
            x = 1:length(ts),
            y = unlist(unname(ts)),
            labels = unlist(unname(ts)),
            pos = 3,
            cex = 0.5
          )
          axis(
            side = 4,
            at = pretty(range(0, max(
              unlist(unname(ts))
            ))),
            col = "blue",
            col.ticks = "blue"
          )
          mtext(
            "Number of Stations",
            side = 4,
            line = 3,
            col = "blue"
          )
          title(ylab = "Amount of O&M Funding ($)", mgp = c(4, 1, 0))
          legend(
            "bottomleft",
            c(salnamesFunding),
            col = c(1:length(salnamesFunding)),
            pch = rep(20, length(salnamesFunding)),
            cex = 0.7
          )

        }
      }
    } else if (which == "predictSalary") {
      if (as.numeric(str_extract(max(salyears), ".+?(?=-)")) > as.numeric(str_extract(Sys.time(), ".+?(?=-)"))) {
        fiscalyear <- as.numeric(str_extract(Sys.time(), ".+?(?=-)"))
        firstyears <- as.numeric(str_extract(names(saldf), ".+?(?=-)"))
        current <- which(abs(firstyears - fiscalyear) == min(abs(firstyears - fiscalyear)))  # Find closest year
        saldf <- saldf[1:current]
        salyears <- salyears[1:current]
      }

      ncol <- length(salyears) + 3 # For three years
      DFSAL <-
        data.frame(matrix(NA, nrow = length(salnamesFunding), ncol = ncol))
      # Creating new year labels
      ly <-
        gsub("^[^-]*-\\s*([^.]+).*", "\\1", salyears[length(salyears)]) # Getting last number of last year (ie. 2023-2024)
      newyear <- paste0(ly, "-", as.numeric(ly) + 1)
      ny2 <- as.numeric(gsub("^[^-]*-\\s*([^.]+).*", "\\1", newyear))
      newyear2 <- paste0(ny2, "-", ny2 + 1)
      ny3 <- as.numeric(gsub("^[^-]*-\\s*([^.]+).*", "\\1", newyear2))
      newyear3 <- paste0(ny3, "-", ny3 + 1)
      labels <- c(salyears, newyear, newyear2, newyear3)
      names(DFSAL) <- labels
      DFSAL[1:length(salyears)] <- saldf

      # STEP 2: DETERMINE MEDIAN SALARIES FOR EACH YEAR
      fundingLevel <- salary$level_display
      fundingLevel[which(fundingLevel == "IT--03")] <- "CS--03"
      soi <- vector(mode = "list", length(unique(salary$level_display)))
      load(file.path(system.file(package="dataSPA"),"data", "salaries.rda"))
      for (i in seq_along(unique(fundingLevel))) {
        s <-
          salaries[which(grepl(unique(fundingLevel)[[i]], salaries$`Level and Step`)), ] # Find which salary spreadsheet to take median
        eyears <-
          as.numeric(sub('.* ', '', s$`Level and Step`)) # Extract everything after space to get year
        EY <- sort(unique(as.numeric(eyears)))
        for (j in seq_along(EY)) {
          soi[[i]][j] <-
            median(s[which(eyears %in% EY[j]), ]$`Annual Salary`, na.rm = TRUE)
        }
      }
      names(soi) <- unique(fundingLevel) # This is the medians over the years
      # STEP 3: DETERMINE RATE OF INCREASE
      roi <- NULL
      for (i in seq_along(soi)) {
        x <- 1:length(soi[[i]])
        y <- soi[[i]]
        m <- lm(y ~ x)
        roi[[i]] <- unname(m$coefficients[2]) # Approx 1500 every year
      }
      names(roi) <- unique(fundingLevel) # This is the rate of increase/year
      dfROI <-
        data.frame(matrix(
          " ",
          nrow = length(salnamesFunding),
          ncol = length(salyears) + 3
        ))
      # STEP 4: REMOVE OVERTIME HOURS (THEY'RE HARD TO PREDICT)
      # Find total amount spent on the project per funding type (need this for bar chart)
      for (i in seq_along(salyears)) {
        for (j in seq_along(salnamesFunding)) {
          value <-
            salaryKeep[which(salaryKeep$fiscal_year == salyears[i]), ] # Look at one year
          value2 <-
            value[which(value$funding_source_display == salnamesFunding[j]), ] # specific funding source
          totalSum <-
            sum(value2$amount_week[which(is.finite(value2$amount_week))], na.rm =
                  TRUE)
          saldf[paste0(salnamesFunding[j]), paste0(salyears[i])] <- totalSum
        }
      }
      dfROI[1:length(salyears)] <- saldf
      names(dfROI) <- labels
      row.names(dfROI) <- salnamesFunding
      # Fill in next values
      value <-
        salaryKeep[which(salaryKeep$fiscal_year == salyears[length(salyears)]), ] # Look at last
      new <- value
      # STEP 5: CHANGE MEDIAN SALARIES BY ADDING ROI
      extraYears <- length(salyears)+1:3
      for (j in seq_along(extraYears)) {
        for (k in seq_along(unique(value$funding_source_display))) {
        for (i in seq_along(value$level_display)) {
            newAmount <-
              unname(unlist(roi[which(names(roi) == new$level_display[i])])) # Find ROI for level
            new$median_salary[i] <-
              value$median_salary[i] + (j*newAmount) # Update calculations
            new$salary_per_week[i] <- new$median_salary[i] / 52
            new$amount_week[i] <- new$salary_per_week[i] * new$duration_weeks[i]
            new$amount_overtime[i] <- 0
        }

            # Ignoring overtime
            totalSum <-
              sum(new$amount_week[which(new$funding_source_display == unique(new$funding_source_display)[k])], na.rm =
                    TRUE)
            dfROI[extraYears[j]][which(rownames(dfROI) == unique(value$funding_source_display)[k]),] <- round(totalSum, 2)
            new <- new # Reset new

        }

      }
      keep <- (length(salyears) + 1):length(labels)
      for (i in seq_along(keep)) {
        dfROI[keep][i][which(dfROI[keep][[i]] == " "), ] <- 0
      }

# STEP 6: DETERMINE THE GAP
dfROI2 <- data.frame(matrix(0, ncol=length(labels), nrow=length(salnamesFunding)+length(salnamesFunding)))
names(dfROI2) <- labels
rownames(dfROI2) <- c(salnamesFunding, paste0(salnamesFunding, "GAP"))
dfROI2[1:length(salnamesFunding),] <- dfROI
old <- dfROI[length(salyears)]
N <- dfROI[(length(salyears)+1):length(labels)]
# determine the gap from the last year for each funding type
gap <- NULL
g <- NULL
for (i in seq_along(N)) {
gap[[i]] <- old
g[[i]] <- as.numeric(unlist(unname(N[i])))-unlist(unname(old[1]))
for (j in seq_along(salnamesFunding)) {
  gap[[i]][j,] <- g[[i]][j]
}
}

# Fill in the gap values for the new data frame
# Subtract missing values from prediction
for (i in seq_along(keep)) {
  for (k in seq_along(salnamesFunding)) {
    dfROI2[keep][i][(length(salnamesFunding) + k), ] <- gap[[i]][k, ]
    re <- as.numeric(dfROI2[keep][i][k,])-gap[[i]][k,]
    dfROI2[keep][i][k,] <- as.numeric(re)
      }

}

# Structuring data frame so reds aren't together
dfROI2 <- dfROI2[sort(row.names(dfROI2)),]
# Show gap + number
cc <- which(sort(unique(salary$funding_source_display)) %in% sort(salnamesFunding))
j <- sort(rep(cc,2))
j <- col[as.numeric(j)]
j[which(!(seq_along(j) %in% seq(from=1, to=length(j), by=2)))] <- "red"
par(mfrow=c(1,1), mar = c(5, 5, 1.5, 4) + 0.1)
#Print amount of money

sums <- NULL
for (i in seq_along(dfROI2[which(grepl("GAP", row.names(dfROI2))),][(length(salyears)+1):length(labels)])) {
  sums[[i]] <- round(sum(as.numeric(unlist(unname(dfROI2[which(grepl("GAP", row.names(dfROI2))),][(length(salyears)+1):length(labels)][i])))),0)
}
sums2 <- NULL
for (i in seq_along(dfROI)) {
  sums2[[i]] <- round(sum(as.numeric(unlist(unname(dfROI[i])))),0)
}

if (length(unique(unlist(unname(dfROI2)))) == 1) {
  if (as.numeric(unique(unlist(unname(dfROI2))) == 0)) {
    stop("No time spent on this project.")
  }

}

if (is.null(theme) && is.null(functionalGroup) && is.null(section) && is.null(division)) {
bp <-
  barplot(
    as.matrix(dfROI2),
    col = j,
    ylim = c(0, max(unlist(sums2))*2),
    xlab = " ",
    las = 2,
    ylab = " ",
    cex.names=0.8
  )
legend(
  "topleft",
  wrapText(c(rownames(dfROI2)[-(which(grepl("GAP", rownames(dfROI2))))], "Gap in funding")),
  col = c(j[which(!(j == "red"))], "red"),
  pch = rep(20, (length(salnamesFunding)+1)),
  cex = 0.7,
  bty="n"
)
} else {

  if (length(salyears) == 1) {
    # 1
    l <- 1
  } else if (length(salyears) > 1 && length(salyears) < 5) {
    # 2,3,4
    l <- 2
  } else if (length(salyears) > 4 && length(salyears) < 7) {
    # 5, 6
    l <- 3
  } else if (length(salyears) > 6) {
    # To move further away, make l bigger
    # 7
    l <- 8
  }

  holder <- data.frame(matrix(0, nrow = length(dfROI2[,1]), l))
  names(holder) <- c("  ")
  DF <- cbind(dfROI2, holder)

  for (i in seq_along(DF[keep])) {
    DF[keep][,i] <- as.numeric(DF[keep][,i])
  }
  bp <-
    barplot(
      as.matrix(DF),
      col = j,
      ylim = c(0, max(unlist(sums2))*1.3),
      xlab = " ",
      las = 2,
      ylab = " ",
      cex.names=0.8,
    )

  string <- rownames(dfROI2)[-(which(grepl("GAP", rownames(dfROI2))))]
  strings <- wrapText(string=string, nchar=20)
  legend(
    "bottomright",
    c(strings, "Gap in funding"),
    col = c(j[which(!(j == "red"))], "red"),
    pch = rep(15, (length(salnamesFunding)+1)),
    cex = 0.55
  )

}
title(ylab = "Amount of Salary Funding ($)", mgp = c(4, 1, 0))
abline(v = mean(bp[length(salyears):(length(salyears) + 1)]), col = "red", lty=3)
m <- max(unlist(sums2)[1:length(salyears)])
pr <- max(unlist(sums2[(length(salyears)+1):(length(sums2))]))
y <- unname(quantile(c(m,pr), .15))
#y <- median(c(m,pr))
if (m-pr < 0) {
  y <- pr+(max(unlist(sums2))/20)
}
text(
  x = bp[(length(salyears)+1):length(labels)],
  y = y,
  labels = paste0("$", sums),
  pos = 3,
  cex = 0.65,
  col="red",
  srt=90
)

if (dataframe == TRUE) {
  return(dfROI2)
}
    } else if (which == "overviewInvestment") {
      if (which == "overviewInvestment") {
        if (!(is.null(om)) && (!(is.null(salary)))) {
          om <- crab
          salary <- salaryKeep
          # Both om and salary are given
          statuses <- sort(unique(c(unique(om$status), unique(salary$status))))
        } else if (!(is.null(om))) {
          om <- crab
          statuses <- sort(unique(om$status))
        } else if (!(is.null(salary))) {
          salary <- salaryKeep
          # salary is given
          statuses <- sort(unique(salary$status))
        }
        sdf <- data.frame(matrix(NA, nrow = 1, ncol = length(statuses)))
        names(sdf) <- statuses

        for (i in seq_along(statuses)) {
          if (!(is.null(om))) {
            value <- om[which(om$status == statuses[i]),] # Isolating specific status
            if (is.null(salary)) {
              sdf[,i] <- sum(value$amount)
            }
          }
          if (!(is.null(salary))) {
            value2 <- salary[which(salary$status == statuses[i]),] # Isolating specific status
            if (is.null(om)) {
              sdf[,i] <- sum(value2$amount_total)
            }
          }
          if (!(is.null(om)) && !(is.null(salary))) {
            sdf[,i] <- sum(value2$amount_total) + sum(value$amount)
          }
        }
        if (!(is.null(om)) && is.null(salary)) {
          ylab <- "Amount of O&M Investment ($)"
        } else if (!(is.null(salary)) && is.null(om)) {
          ylab <- "Amount of Salary Investment ($)"
        } else if (!(is.null(om)) && !(is.null(salary))) {
          ylab <- "Amount of O&M & Salary Investment ($)"
        }
        barplot(
          unlist(unname(sdf)),
          col = 1:(length(statuses)),
          names.arg = statuses,
          ylab = ylab,
          ylim = c(0, max(unlist(sdf))*1.5),
          xlab = " ",
          las=2
        )
      }



    }
}


