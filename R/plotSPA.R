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
#' * For `which="omAllocation"` Line graphs representing changes in O&M
#' investment for categories over the years. The average rate of change
#' per year for each category, where applicable, displayed on the graph in red
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
#' * For `which="indeterminate"` a bar chart representing percentage of
#' indeterminate vs non-Indeterminate employees for project years.
#' Number of staff shown on the figure in red
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
#' @param dataframe a Boolean indicating if the user wants the data frame
#' of what is being plotted returned. For multi-year projects, a list of
#' data frames for each year is returned. This is mainly for package testing
#' @param year a character indicating which year of the project to
#' plot. This is given in the format "YYYY-YYYY". If `year` is `NULL`
#' all years are plotted by default
#' @param item if`which='omAllocation'` an argument
#' specifying which category display to plot. If `which=salaryAllocation`,
#' the argument specifys whichs level to plot. By default all are plotted
#' @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.
#' @importFrom graphics barplot legend par pie title lines axis layout points
#' @importFrom graphics abline mtext text
#' @importFrom stats quantile
#' @importFrom stats lm coef
#' @importFrom stringr str_count str_trim
#' @importFrom grDevices extendrange
#' @importFrom tidytext bind_tf_idf
#' @importFrom dplyr count desc inner_join select summarize mutate arrange anti_join
#' @importFrom tidytext get_stopwords
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 expansion
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 theme
#' @importFrom tidyr pivot_longer
#' @importFrom scales label_dollar
#' @importFrom plotly ggplotly
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr across everything
#' @examples
#' \dontrun{
#' # Example 1: Plot Bar graph of O&M Allocations
#' library(dataSPA)
#' data <- getData(type="om", cookie=cookie)
#' plotSPA(om=data, which="omBar")
#'
#' # Example 2: Pie Chart showing number of projects by status
#' plotSPA(om=data, salary=data2, which="overviewStatus")
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
           status = NULL,
           dataframe = FALSE,
           year = NULL,
           debug = 0) {

    project_id <-
      category_display <-
      project_year_id <-
      amount <-
      funding_source_display <-
      fiscal_year <- project_year_id <- category_type <- deliverables <- milestones <- salaries <-
      stop_words <- words <- overview <- subpoint <- tf_idf_sum <- tf_idf <- salaries <- deliverable <-
      milestone <- . <-  Year <- Amount <- fundingPalette <- dmoney <- mmoney <- FundingSource <-
      GAP <- NULL

    load(file.path(system.file(package="dataSPA"),"data", "fundingPalette.rda"))

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
        "must provide a which argument of either 'omBar', 'omAllocation', 'salaryBar', 'salaryAllocation', 'indeterminate', 'predictSalary', 'predictOM', 'overviewStatus', 'overviewInvestment', or 'ebfm"
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
      #mainTitle <- paste(items, items2, sep=": ")
      border <- rep("black", length(names(status_counts)))
      border[which(names(status_counts) == "Approved")] <- "red"
      par(lwd = 2)
      PIE <- pie(status_counts, labels = paste0(names(status_counts), " (", status_counts, ")"), main = " ", border=border, col=1:length(names(status_counts)), edges=200)
      on.exit(par(lwd = 1))
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
        "omAllocation",
        "salaryBar",
        "salaryAllocation",
        "indeterminate",
        'predictSalary',
        'predictOM',
        'overviewStatus',
        'overviewInvestment',
        'ebfm'
      )
    )) {
      stop(
        "must provide a which argument of either 'omBar', 'omAllocation', 'salaryBar', 'salaryAllocation', 'indeterminate', 'predictSalary', 'predictOM', 'overviewStatus', 'overviewInvestment', or 'ebfm'")
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
      "omAllocation",
      'predictOM',
      'overviewInvestment',
      'ebfm'
    )) {
      if (debug > 0) {
        message("om has been identified")
      }
      if (is.null(om)) {
        stop("Must provide an om argument.")

      }
      # if (!(identical(
      #   c("project_id","category_display","project_year_id",
      #     "amount","funding_source_display",
      #     "category_type",  "description", "tags",
      #     "tag_id", "fiscal_year", "project_title",
      #     "status","overview","objectives",
      #     "lead_staff","section_display","functional_group",
      #     "activity_type","theme", "deliverables",
      #     "milestones", "section_id","division_id",
      #     "region_id", "funding_id","theme_id",
      #     "branch_id","om_id"),
      #   names(om)
      # ))) {
      #   stop("Must obtain data for x using getData(type='om')")
      # }

        # Dealing with themes

      if (!(is.null(theme))) {
        id <- NULL
        section <- NULL
        functionalGroup <- NULL
        division <- NULL
      }

        if (!(is.null(id))) {
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
          #if (is.null(theme) && is.null(functionalGroup) && is.null(section) && is.null(division)) {
            if (all(value$deliverables == "0")) {
            dv[[i]] <- 0
          } else {
          dv[[i]] <- length(unlist(strsplit(unique(value$deliverables), "|-----|", fixed=TRUE)))
          }

          # Milestones
          if (all(value$milestones == "0")) {
            ms[[i]] <- 0
          } else {
            ms[[i]] <- length(unlist(strsplit(unique(value$milestones), "|-----|", fixed=TRUE)))
          }
          #}
        }
        # Creating place for legend
        if (which == "omBar") {
        if (dataframe == TRUE) {
          return(df)
        }
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
          milesanddels <- data.frame(Year=names(dv),
                                     deliverable=unlist(dv),
                                     milestone=unlist(ms)) %>%
            mutate(dmoney=max(colSums(df))*1.05+deliverable*max(colSums(df)/5),
                   mmoney=max(colSums(df))*1.05+milestone*max(colSums(df))/5)

          p <- ggplot(df %>%
                        mutate(`Funding Source`=row.names(.)) %>%
                        pivot_longer(cols=-`Funding Source`,names_to="Year",values_to="Amount"))+
            geom_col(aes(x=Year,y=Amount,fill=`Funding Source`))+
            labs(y="Amount of O&M Funding ($)")+
            scale_fill_manual(values=fundingPalette,name="Funding Source")+
            theme_classic()+
            geom_line(data=milesanddels,
                      aes(x=Year, y=dmoney, group=1), col="blue")+
            geom_line(data=milesanddels,
                      aes(x=Year, y=mmoney, group=1), col="red")+
            geom_point(data=milesanddels,
                       aes(x=Year, y=dmoney, group=1), col="blue")+
            geom_point(data=milesanddels,
                       aes(x=Year, y=mmoney, group=1), col="red")+
            geom_text(data=milesanddels,
                      aes(x=Year, y=dmoney*1.05,label=deliverable, group=1), col="blue")+
            geom_text(data=milesanddels,
                      aes(x=Year, y=mmoney*1.05, label=milestone, group=1), col="red")+
            scale_y_continuous(labels = label_dollar(),
                               expand = expansion(mult=c(0,0.2)))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels vertically

          return(ggplotly(p))

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
          if (i == 1) {
          m <- 0.02
          DF[,i] <- unlist(unname(df[length(years)]))*(1+m)
          } else {
            DF[,i]<- unlist(unname(DF[i-1]))*(1+0.02)
          }
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
        #browser() #tuesday

        # FIXING DOUBLE GAPPING
        for (i in seq_along(dfs[,newyear])) {
          dfs[,newyear][i] <- df[,length(df)]
        }

        DFs <- rbind(dfs, gap)

        rownames(DFs) <- c(namesFunding, paste0(namesFunding, " GAP"))

        # Restructure gap
        DFs <- DFs[sort(rownames(DFs)),]
          # Reshape data to long format
          df_long <- DFs %>%
            rownames_to_column("FundingSource") %>%
            pivot_longer(-FundingSource, names_to = "Year", values_to = "Amount")

          # Separate GAP rows
          df_long <- df_long %>%
            mutate(GAP = grepl("GAP", FundingSource),
                   FundingSource = gsub(" GAP", "", FundingSource))

          # Create stacked bar chart
          p <- ggplot(df_long, aes(x = Year, y = Amount, fill = ifelse(GAP, "GAP", FundingSource))) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = c(fundingPalette, "GAP" = "red")) +
            guides(fill = guide_legend(override.aes = list(color = NA), title = "Funding Source")) +
            theme_minimal()+
            labs(y = "Amount of O&M Funding", x = "Year") +  # Update y-axis label
            theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels vertically
          return(ggplotly(p))

          # end tuesday

        if (dataframe == TRUE) {
          return(DFs)
        }

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

      }
    }

    if (which %in% c(
      "salaryBar",
      "salaryAllocation",
      "indeterminate",
      'predictSalary',
      'overviewInvestment',
      'ebfm'
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
        if (dataframe == TRUE) {
          return(saldf)
        }
        p <- ggplot(saldf %>%
                      mutate(`Funding Source`=row.names(.)) %>%
                      pivot_longer(cols=-`Funding Source`,names_to="Year",values_to="Amount"))+
          geom_col(aes(x=Year,y=Amount,fill=`Funding Source`))+
          labs(y="Amount of Salary Funding ($)")+
          scale_fill_manual(values=fundingPalette,name="Funding Source")+
          theme_classic()+
          scale_y_continuous(labels = label_dollar(),
                             expand = expansion(mult=c(0,0.2)))+
          theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels vertically

        return(ggplotly(p))

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

      } else if (which %in% c("predictSalary")) {
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
            length(value$staff_id[which(grepl("Indeterminate", value$employee_type_display))]) # For # of employees
          NON <-
            length(value$staff_id[which(!(grepl(
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

    if (which == "predictSalary") {
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

# FIXING DOUBLE GAPPING (HERE JAIM)
for (i in keep) {
  dfROI[,i] <- dfROI[,length(salyears)]
}


# Structuring data frame so reds aren't together
dfROI2 <- dfROI2[sort(row.names(dfROI2)),]

dfROI2 <- dfROI2 %>%
  mutate(across(everything(), as.numeric))

if (length(unique(unlist(unname(dfROI2)))) == 1) {
  if (as.numeric(unique(unlist(unname(dfROI2))) == 0)) {
    stop("No time spent on this project.")
  }

}

df_long <- dfROI2 %>%
  rownames_to_column("FundingSource") %>%
  pivot_longer(-FundingSource, names_to = "Year", values_to = "Amount")

# Separate GAP rows
df_long <- df_long %>%
  mutate(GAP = grepl("GAP", FundingSource),
         FundingSource = gsub(" GAP", "", FundingSource))

# Create stacked bar chart
p <- ggplot(df_long, aes(x = Year, y = Amount, fill = ifelse(GAP, "GAP", FundingSource))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(fundingPalette, "GAP" = "red")) +
  guides(fill = guide_legend(override.aes = list(color = NA), title = "Funding Source")) +
  theme_minimal()+
  labs(y = "Amount of Salary Funding", x = "Year") +  # Update y-axis label
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels vertically
return(ggplotly(p))









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

    } else if (which == "ebfm") {
      EBFMFILE <- "//dcnsbiona01a/BIODataSvc/IN/MSP/PowerBI-Projects/dataSPA/inputs/Ecological Pillar Objectives 20 April 2023.docx"
      if (!(file.exists(EBFMFILE))) {
        stop("Must have access to the EBFM frame work pillar file for this plot type")
      } else {
        if (exists("crab")) {
          om <- crab
        }
        if (exists("salaryKeep")) {
          salary <- salaryKeep
        }
        docx <-
          (read_docx(EBFMFILE) |>
             docx_summary())[-c(1:4, 18), ]
        pillars <- data.frame()

        for (i in seq_along(docx$text)) {
          if (is.na(docx$level[i])) {
            pillar <- docx$text[i]
          } else if (docx$level[i] == 1) {
            subpillar <- docx$text[i]
          } else if (docx$level[i] == 2) {
            point <- docx$text[i]
            if (docx$level[i] >= docx$level[i + 1]) {
              pillars <- rbind(
                pillars,
                data.frame(
                  pillar = pillar,
                  subpillar = subpillar,
                  point = point,
                  subpoint = "",
                  level = docx$level[i]
                )
              )
            }
          } else if (docx$level[i] > 2) {
            pillars <- rbind(
              pillars,
              data.frame(
                pillar = pillar,
                subpillar = subpillar,
                point = point,
                subpoint = docx$text[i],
                level = docx$level[i]
              )
            )
          }

        }

        pillars <- pillars |>
          separate_wider_delim(pillar,":",names=c("pillar","description"))|>
          mutate(pillar=gsub(".. ","",pillar))

        subpillars <- pillars |>
          mutate(subpillar=paste0(pillar,": ",subpillar)) |>
          group_by(pillar,description,subpillar) |>
          summarise(words=paste(point,subpoint,collapse=" "),
                    .groups = 'drop') |>
          group_by(pillar,description,subpillar) |>
          unnest_tokens(word,words) |>
          dplyr::anti_join(get_stopwords(),by="word") |>
          group_by(pillar,description,subpillar,word) |>
          summarise(n=n(),
                    .groups = 'drop') |>
          ungroup()
        subpillars_tf_idf <- subpillars |>
          bind_tf_idf(word,subpillar,n) |>
          arrange(desc(tf_idf))
        tidy_om <- om |>
          select(project_id,overview) |>
          unique() |>
          unnest_tokens("word",overview) |>
          suppressMessages(dplyr::anti_join(stop_words)) |>
          count(word, project_id)
        scores <- tidy_om |>
          inner_join(subpillars_tf_idf,by="word",relationship =
                       "many-to-many") |>
          group_by(project_id,subpillar) |>
          summarize(tf_idf_sum=sum(tf_idf),
                    .groups = 'drop') |>
          ungroup() |>
          group_by(project_id) |>
          mutate(relative_weight=tf_idf_sum/sum(tf_idf_sum)) |>
          ungroup()
        pal <- c(lighten("#1b9e77",0.4),
                 lighten("#1b9e77",0.1),
                 darken("#1b9e77",0.1),
                 darken("#1b9e77",0.4),
                 lighten("#7570b3",0.4),
                 "#7570b3",
                 darken("#7570b3",0.4),
                 lighten("#d95f02",0.2),
                 darken("#d95f02",0.2))
        sp <- unique(scores$subpillar)
        noIDS <- NULL
        for (i in seq_along(sp)) {
          noIDS[[i]] <- length(unique(scores$project_id[which(scores$subpillar == sp[i])]))
        }
        scores2 <- data.frame(matrix(NA, nrow = length(sp), ncol = 2))
        if (dataframe) {
          names(scores2) <- c("subpillar", "ids")
          scores2$subpillar <- sp
          kk <- NULL
          for (i in seq_along(sp)){
            k <- unique(scores$project_id[which(scores$subpillar == sp[i])])
            kk[[i]] <- paste0(k, collapse=",")
          }
          scores2$ids <- unlist(kk)
          return(scores2)
        }
        names(scores2) <- c("noIDS", "subpillar")
        scores2$noIDS <- unlist(noIDS)
        scores2$subpillar <- sp
        par(mar = c(1, 3, 1, 3), mfrow=c(1,2), xpd=TRUE)  # c(bottom, left, top, right)
        pie(scores2$noIDS, labels=scores2$noIDS, col=pal)
        plot(0, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1), axes=FALSE)
        legend("left", legend=unique(scores2$subpillar), col=pal, pch=20, cex=0.5, pt.cex=2, bty="n",inset = c(-0.29, 0))
      }

    }
}
