#' Plot an argoFloats Object
#'
#' This function plots specific graphs (determined by the
#' `which` argument) using the data frames returned by
#' [getData()].
#'
#' The various plot types are as follows:
#'
#' * For `which="omBar"`, a bar graph representing amount of money ($)
#  invested in O&M per funding type for project years is plotted. Graph color
#' coded by funding type.
#'
#' * For `which="omPie"`, a pie chart representing amount of money ($)
#  invested in O&M per funding type for project years is plotted. Graph color
#' coded by funding type
#'
#' * For `which="omAllocation"` a bar graph representing amount of money ($)
#' invested per O&M category for project years is plotted
#'
#' * For `which=omAllocationGeneral` a pie chat representing proportion of
#' funding per category type is plotted for year each of the specified project
#'
#' * For `which="salaryBar"` a bar graph representing amount of salary money ($)
# invested per funding type for project years. Salaries are calculated
#' using the median values of a specific job classification. Graph color
#' coded by funding type indicated by the legend.
#'
#' * For `which="salaryAllocation"` a bar graph representing amount of salary money ($)
# invested per job classification for project years. Salaries are calculated
#' using the median values of a specific job classification.
#'
#' * For `which="weekAllocation"` a bar graph representing time (weeks)
# invested per job classification for project years is plotted

#' * For `which="indeterminate"` a pie chart representing proportion of indeterminate vs
#' non-Indeterminate employees for project years is plotted
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
#' increasing while funding amount remains the same
#'
#' @param om a data frame likely from `getData(type='om')`
#' @param salary a data frame likely from `getData(type='salary')`
#' @param which indicates which plot to plot (See Details). The
#' options to use include: `omBar`, `omPie`, `omAllocation`,
#' `omAllocationGeneral`, `salaryBar`, `salaryAllocation`,
#' `weekAllocation`,`indeterminate`, `predictSummary`, or `predict`
#' @param id the project_id from the Project Planning Tool
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
#' @param salaries a built in data frame obtained from the `dataSPA`
#' package that contains information about pay levels for
#' different DFO positions
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
#' @importFrom stats lm
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
#' data <- getData(type='om', cookie=cook
#' data2 <- getData(type='salary', cookie=cookie)
#' plotSPA(om=data, salary=data2, which='predictSummary', id=1093,
#' funding= rep("NCP (A-base)",4),
#' fundingChange=c(-25, -5, -75, -100), salaries=salaries)
#'
#' # Example 3: Bar plot predicting of the change in
#' number of stations impact as a result of a certain funding
#' ending
#'
#' plotSPA(om=data, salary=data2, which='predict', id=1093,
#' funding= "NCP (A-base)", endDate="2026-2027", salaries=salaries)
#'
#' }
#' @export
#'
#' @return None (invisible NULL).
#'
#' @author Jaimie Harbin
#'

plotSPA <-
  function(om = NULL,
           salary = NULL,
           which = NULL,
           id = NULL,
           funding = NULL,
           fundingChange = NULL,
           dataframe = FALSE,
           year = NULL,
           endDate = NULL,
           salaries = NULL,
           debug = 0) {
    project_id <-
      category_display <-
      project_year_id <-
      amount <-
      funding_source_display <-
      fiscal_year <- project_year_id <- category_type <- NULL

    if (is.null(which)) {
      stop(
        "must provide a which argument of either 'omBar', 'omPie', 'omAllocation', 'omAllocationGeneral','salaryBar', 'salaryAllocation', 'weekAllocation', 'indeterminate', 'predictSummary",'predict','predictSalary'
      )
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
        'predictSalary'
      )
    )) {
      stop(
        "must provide a which argument of either 'omBar', 'omPie', 'omAllocation', 'omAllocationGeneral', 'salaryBar', 'salaryAllocation', 'weekAllocation', 'indeterminate'"
      )
    }

    if (is.null(id)) {
      stop("As of 2023/05/10, user must provide an id to plot specific projects")
    }

    if (debug > 0) {
      message("which= ", which, " and id = ", id)
    }

    if (which %in% c(
      "omBar",
      "omPie",
      "omAllocation",
      "omAllocationGeneral",
      'predictSummary',
      'predict'
    )) {
      if (debug > 0) {
        message("om has been identified")
      }
      if (is.null(om)) {
        stop("Must provide an om argument.")

      }
      if (!(identical(
        c(
          "project_id",
          "category_display",
          "project_year_id",
          "amount",
          "funding_source_display",
          "id",
          "category_type",
          "description",
          "fiscal_year",
          "project_title",
          "status",
          "overview",
          "objectives",
          "deliverables",
          "lead_staff"
        ),
        names(om)
      ))) {
        stop("Must obtain data for x using getData(type='om')")
      }


      crab <- om[which(om$project_id == id), ]
      keep <-
        subset(
          crab,
          select = c(
            project_id,
            category_display,
            project_year_id,
            amount,
            funding_source_display,
            id,
            fiscal_year,
            category_type
          )
        )

      if (is.null(year)) {
        years <- unique(keep$fiscal_year)
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
      if (which == "omBar") {
        barplot(
          as.matrix(df),
          col = c(1:length(namesFunding)),
          ylab = "Amount of O&M Funding ($)",
          ylim = c(0, sum(subset(
            df, select = c(paste0(years[1]))
          )) + 109000),
          xlab = "Year"
        )
        legend(
          "bottomright",
          c(namesFunding),
          col = c(1:length(namesFunding)),
          pch = rep(20, length(namesFunding)),
          cex = 0.7
        )

        if (dataframe == TRUE) {
          return(df)
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
        par(mar = c(12, 4, 4, 2) + 0.1)
        par(mfrow = c(1, length(years)))
        DF <- NULL
        for (i in seq_along(years)) {
          value <-
            keep[which(keep$fiscal_year == years[i]), ] # Look at one year
          mo <- matrix(0, nrow = 1, ncol = length(unique(value$category_display)))
          mdf <- as.data.frame(mo, col.names = unique(value$category_display))
          names(mdf) <- unique(value$category_display)
          for (j in seq_along(unique(value$category_display))) {
            mdf[j] <-
              sum(value$amount[which(value$category_display == unique(value$category_display)[j])], na.rm =
                    TRUE)
          }
          barplot(
            as.matrix(mdf),
            col = 1,
            las = 2,
            ylab = "Cost ($)",
            xlab = NULL,
            cex.axis = 0.7
          )
          title(paste0(years[i]))
          DF[[i]] <- mdf
        }
        if (dataframe == TRUE) {
          return(DF)
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
      'predictSalary'
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
        c(
          "id",
          "overtime_hours",
          "smart_name",
          "duration_weeks",
          "level_display",
          "funding_source_display",
          "employee_type_display",
          "project_year_id",
          "project_id",
          "fiscal_year",
          "project_title",
          "median_salary",
          "salary_per_week",
          "amount_week",
          "amount_overtime",
          "amount_total"
        ),
        names(salary)
      ))) {
        stop("Must obtain data for x using getData(type='salary')")
      }

      salaryKeep <- salary[which(salary$project_id == id), ]

      if (is.null(year)) {
        salyears <- unique(salaryKeep$fiscal_year)
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
        barplot(
          as.matrix(saldf),
          col = c(1:length(salnamesFunding)),
          ylab = "Amount of Salary Funding ($)",
          ylim = c(0, sum(subset(
            saldf, select = c(paste0(salyears[1]))
          )) + 109000),
          xlab = "Year"
        )
        legend(
          "topright",
          c(salnamesFunding),
          col = c(1:length(salnamesFunding)),
          pch = rep(20, length(salnamesFunding)),
          cex = 0.7
        )
        if (dataframe == TRUE) {
          return(saldf)
        }
      } else if (which %in% "salaryAllocation") {
        par(mfrow = c(1, length(salyears)))
        DFL <- NULL
        for (i in seq_along(salyears)) {
          value <-
            salaryKeep[which(salaryKeep$fiscal_year == salyears[i]), ] # Look at one year
          ml <- matrix(0, nrow = 1, ncol = length(unique(value$level_display)))
          dfl <- as.data.frame(ml, col.names = unique(value$level_display))
          names(dfl) <- unique(value$level_display)

          for (j in seq_along(unique(value$level_display))) {
            dfl[j] <-
              sum(value$amount_total[which(value$level_display == unique(value$level_display)[j])], na.rm =
                    TRUE)
          }
          # Fill in values
          barplot(
            as.matrix(dfl),
            col = 1,
            ylab = "Salary Cost ($)",
            xlab = "Job Classification"
          )
          title(paste0(salyears[i]))
          DFL[[i]] <- dfl
        }
        if (dataframe == TRUE) {
          return(DFL)
        }

      } else if (which %in% c("weekAllocation", "predictSalary")) {
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
        if (dataframe == TRUE) {
          return(DFL2)
        }

      } else if (which == "indeterminate") {
        par(mfrow = c(1, length(salyears)))
        DFI <- NULL
        for (i in seq_along(salyears)) {
          value <-
            salaryKeep[which(salaryKeep$fiscal_year == salyears[i]), ] # Look at one year
          mi <- matrix(0, nrow = 1, ncol = 2)
          dfi <-
            as.data.frame(mi, col.names = c("indeterminate", "Non-Indeterminate"))
          names(dfi) <- c("indeterminate", "Non-Indeterminate")
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

          pie(unname(unlist(dfi)),
              col = c(1:2),
              labels <- paste0(c(int, non), "% ; ", c(INT, NON), " staff"))
          title(paste0(salyears[i]))
          DFI[[i]] <- dfi
        }
        legend(
          "bottomright",
          c("Indeterminate", "Non-indeterminate"),
          col = c(1:2),
          pch = rep(20, 2),
          cex = 0.7
        )
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
        abline(v = mean(bp[length(years):(length(years) + 1)]), col = "red")
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
          #fundingChange <- -100
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
            abline(v = mean(bp[length(years):(length(years) + 1)]), col = "red")
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
      ncol <- length(salyears) + 3 # For three years
      DFSAL <-
        data.frame(matrix(NA, nrow = length(salnamesFunding), ncol = ncol))

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

      # Need to determine rate of increase
      # ERASE JAIM

      fundingLevel <- salary$level_display

      soi <- vector(mode = "list", length(unique(salary$level_display)))
      for (i in seq_along(unique(fundingLevel))) {
        if (is.null(salaries)) {
          stop(
            "must load salaries data frame by using data(salaries) and set salaries=salaries in plotSPA() argument"
          )
        }
        s <-
          salaries[which(grepl(unique(fundingLevel)[[i]], salaries$`Level and Step`)), ]
        eyears <-
          as.numeric(sub('.* ', '', s$`Level and Step`)) # Extract everything after space to get year
        EY <- sort(unique(as.numeric(eyears)))
        for (j in seq_along(EY)) {
          #message("i = ",i, " and j =", j)
          soi[[i]][j] <-
            mean(s[which(eyears %in% EY[j]), ]$`Annual Salary`, na.rm = TRUE)
        }
      }
      names(soi) <- unique(fundingLevel)
      roi <- NULL
      for (i in seq_along(soi)) {
        x <- 1:length(soi[[i]])
        y <- soi[[i]]
        m <- lm(y ~ x)
        roi[[i]] <- unname(m$coefficients[2]) # Approx 1500 every year
      }
      names(roi) <- unique(fundingLevel) # This is the rate of increase

      # FIX GL-MAN-04, GL-MAN-05, GL-MAN-07

      dfROI <-
        data.frame(matrix(
          " ",
          nrow = length(salnamesFunding),
          ncol = length(salyears) + 3
        ))
      dfROI[1:length(salyears)] <- saldf
      names(dfROI) <- labels
      row.names(dfROI) <- salnamesFunding

      # Fill in next values
      #browser()
      value <-
        salaryKeep[which(salaryKeep$fiscal_year == salyears[length(salyears)]), ] # Look at last
      new <- value
      for (j in length(salyears) + 1:3) {
        # Assigning new
        if (!(j == length(salyears) + 1)) {
          value <- new
        }

        for (i in seq_along(value$level_display)) {
          for (k in seq_along(unique(value$funding_source_display))) {
            newAmount <-
              unname(unlist(roi[which(names(roi) == new$level_display[i])])) # Find ROI for level
            new$median_salary[i] <-
              value$median_salary[i] + newAmount # Update calculations
            new$salary_per_week[i] <- new$median_salary[i] / 52
            new$amount_week[i] <- new$salary_per_week[i] * new$duration_weeks[i]
            new$amount_overtime[i] <-
              (new$salary_per_week[i] / 37.5) * new$overtime_hours[i]
            new$amount_total[i] <-
              ifelse(
                value$overtime_hours[i] == 0,
                new$amount_week[i],
                (new$amount_week[i] + new$amount_overtime[i])
              )
            totalSum <-
              sum(new$amount_total[which(new$funding_source_display == unique(new$funding_source_display[k]))], na.rm =
                    TRUE)
            dfROI[j][k, ] <- round(totalSum, 2)
            new <- new # Reset new

          }

        }

      }
#browser()
par(mfrow = c(1, 1))
bp <-
  barplot(
    as.matrix(dfROI),
    col = c(1:length(salnamesFunding)),
    ylim = c(0, sum(subset(
      saldf, select = c(paste0(salyears[1]))
    )) + 109000),
    xlab = "Year",
    las = 2,
    ylab = "Amount of Salary Funding ($)"
  )
#title(ylab = "Amount of Salary Funding ($)", mgp = c(2, 1, 0))
abline(v = mean(bp[length(salyears):(length(salyears) + 1)]), col = "red")
legend(
  "topright",
  c(salnamesFunding),
  col = c(1:length(salnamesFunding)),
  pch = rep(20, length(salnamesFunding)),
  cex = 0.7
)

}
}


