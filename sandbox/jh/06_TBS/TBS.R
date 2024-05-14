library(dataSPA)
library(TBSpayRates)
library(stringr)
#groups <- c("AI", "AO", "AV", "CS", "CX", "EC", "EL", "FB", "FI", "FS", "LP", "NR", "PA", "PR", "RE", "RO", "SO", "SP", "TC", "TR", "UT")
groups <- "FI"
letters <- FALSE
final <- NULL
for (g in seq_along(groups)) { # 1. Cycle through each lead group
g <- 1
group <- groups[[g]]
sal <-  get_salaries(groups = group)

# First check that the - is a number (this is for group=AV, classification= CO-DEV/PER)

classification <- substr(sal$Classification, 1, 2)

for (c in seq_along(unique(classification))) { # 2. Cycle through classifications
salary <- as.data.frame(sal[which(classification == unique(classification)[c]),])

# Test that there is 01 rather than 1
test <- unlist(lapply(strsplit(salary$Classification, "-"), function(x) trimws(x[2], "left")))
if(!(all(grepl("^0", test)))) {
  k <- which(!(grepl("^0", test)))
  if (any(is.na(suppressWarnings(tryCatch(as.numeric(test[k]), error = function(e) NA))))) {
    letters <- TRUE
  }
  if (!(length(k[which(!(is.na(suppressWarnings(tryCatch(as.numeric(test[k]), error = function(e) NA)))))])) == 0) { # This means there is ones without leading 0s but they are all Letters (e.g. CO-DEV/PER)
  salary$Classification[k[which(!(is.na(suppressWarnings(tryCatch(as.numeric(test[k]), error = function(e) NA)))))]] <- paste0(trimws(unlist(lapply(strsplit(salary$Classification[k], "-"), function(x) x[1])), "right"), "-0",trimws(unlist(lapply(strsplit(salary$Classification[k], "-"), function(x) x[2])), "left"))
  }
}


# See steps: https://github.com/dfo-mar-odis/TBSpayRates/issues/8
if (any(grepl("restructure", salary$Effective.Date, ignore.case=TRUE))) {
  good <- which(grepl("restructure", salary$Effective.Date, ignore.case=TRUE))
  BAD <- NULL
  BAD2 <- NULL
  for (go in seq_along(good)) {
    bad <- good[go]-1
    if (grepl("adjustment", salary$Effective.Date[bad], ignore.case=TRUE)) {
      BAD[[go]] <- bad # This is getting ready to remove adjustment before restructure
      salary$date[good[go]] <- salary$date[bad] # filling in the date from above because restructure has no date
    } else {
      # Adjustment is not at the top (e.g. CO-01 (https://github.com/dfo-mar-odis/TBSpayRates/issues/10))
      salary$Effective.Date[bad] <- paste0(salary$Effective.Date[bad], "adjustment jaim2")
      BAD[[go]] <- bad # This is getting ready to remove adjustment before restructure
      salary$date[good[go]] <- salary$date[bad] # filling in the date from above because restructure has no date
    }

    # Now checking if the dateS before the "bad" is the same as the "bad". If so, it will also be removed
    checkDate <- gsub("-.*", "", salary$date[bad])
    checkDates <- gsub("-.*", "", salary$date[1:(bad-1)])
    if (any(checkDates %in% checkDate)) {
      # Other lines are to be removed
      b <- which(checkDates %in% checkDate) # which from above have the same dates
      b2 <- which(salary$Classification == salary$Classification[good[go]]) # which from above have the same classification
      BAD2[[go]] <- intersect(b,b2)
    }
  }
  salary <- as.data.frame(salary[-(unique(c(unlist(BAD),unlist(BAD2)))),])
}

adjust <- c("W)", "X)", "Y)")

for (a in seq_along(adjust)) {
  if (any(grepl(adjust[a], salary$Effective.Date, ignore.case = TRUE))) {
    good <- which(grepl(adjust[a], salary$Effective.Date, ignore.case=TRUE))
    good2 <- which(grepl("adjustment", salary$Effective.Date, ignore.case=TRUE))

    if (!(all(good %in% good2))) {
      # This means there is a W) with no "adjustment" (like FO-02 22)
      fix <- good[which(!(good %in% good2))]
      salary$Effective.Date[fix]
      salary$Effective.Date[fix] <- paste0(salary$Effective.Date[fix], "adjustment jaim")
    }
  }

}

if (any(grepl("adjustment", salary$Effective.Date, ignore.case=TRUE))) {
  # There is no restructure but there could still be adjustment
  # There could so still be adjustment after restructure leaves

  # Now we need to see if Adjustment is put before or after (see group=AI vs group=SP (AC))

  good <- which(grepl("adjustment", salary$Effective.Date, ignore.case=TRUE))

  check <- good[1]-1
  if (salary$date[check] == salary$date[good[1]]) { # if the date above is the same
    below <- TRUE # Adjustment is below everything
    above <- FALSE
  } else {
    below <- FALSE
    above <- TRUE
    good <- good+1
  }

  nextBAD <- NULL
  for (go in seq_along(good)) {
    bad <- good[go]-1
    if (salary$date[bad] == salary$date[good[go]]) {
      nextBAD[[go]] <- bad # This is getting ready to remove adjustment before restructure
    }
  }
  # FIX ME: There may need to be further checks here
salary <- as.data.frame(salary[-(unlist(nextBAD)),])
}

# Find each Classification

year <- regmatches(salary$date, regexpr("\\d{4}", salary$date))
year <- substr(year, 3, 4)
step <- max(as.numeric(regmatches(names(salary)[which(grepl("step", names(salary), ignore.case = TRUE))], regexpr("(?<=\\.)\\d+", names(salary)[which(grepl("step", names(salary), ignore.case = TRUE))], perl = TRUE))))
salary$Classification <- gsub(" ", "", salary$Classification) # removing spaces
#class <- unique(str_extract(salary$Classification, "(?<=-)[0-9]{2}"))
class <- unique(sub(".+\\-", "", salary$Classification)) # This allows letters to be obtained as well

initial_vector <- paste0(paste0(gsub("-", "--", unique(salary$Classification))[1], "-"), 1:step)

list1 <- NULL
for (y in seq_along(unique(year))) {
list1[[y]] <- paste0(initial_vector," ", unique(year)[y])
}

final_vector <- unlist(list1)

levels <- unique(class)

listy <- NULL
for (f in seq_along(levels)) {
  listy[[f]] <- gsub(levels[1], levels[f], final_vector)
}

LevelAndStep <- unlist(listy)
# Do a test for instances like SG-SRE-01
# AC--01-1 21" (levelandstep for regular) and Classification is "AC"
# "SG--SRE--01-1 21" and Class: "SG"

Classification <- rep(strsplit(LevelAndStep, "-")[[1]][1], length(LevelAndStep))
df <- data.frame(matrix(NA, nrow = length(LevelAndStep), ncol = 3))
names(df) <- c("Classification", "Level and Step", "Annual Salary")
df$Classification <- Classification
df$`Level and Step` <- LevelAndStep

for (r in seq_along(1:nrow(df))) { # 3. Go through df to assign salary steps
  message("r = ", r, " and c = ", c)
  MED <- FALSE
  if (length(strsplit(df$`Level and Step`[r], "-")[[1]]) == 6) {
    # Three letter
    k1 <- which(unlist(lapply(strsplit(salary$Classification, "-"), function(x) x[3])) == sub(".*?(\\d+).*", "\\1", df$`Level and Step`[r])) # Condition 1: Check the "01"
    if (!(length(unique(salary$Classification[k1]))) == 1) {
      # This means we have a situation like SG-SRE-01 and SG-PAT-01
      k1 <- which(unlist(lapply(strsplit(salary$Classification[k1], "-"), function(x) x[[2]])) == strsplit(df$`Level and Step`[r], "-")[[1]][3])
    }

  } else {
    if (!(letters)) {
    k1 <- which(unlist(lapply(strsplit(salary$Classification, "-"), function(x) x[2])) == sub(".*?(\\d+).*", "\\1", df$`Level and Step`[r])) # Condition 1: Check the "01"
    } else {
    k1 <- which(unlist(lapply(strsplit(salary$Classification, "-"), function(x) x[2])) == sub(".*--(.*?)-.*", "\\1", df$`Level and Step`[r])) # Condition 1: Check the "01"
    }
  }

  k2 <- which(year == sub("^[^ ]+ ", "", df$`Level and Step`[r])) # Condition 2: Making sure the year is the same
  keep <- salary[intersect(k1, k2),]
  k3 <- unlist(lapply(strsplit(names(keep), "\\."), function(x) x[2]) == trimws(regmatches(df$`Level and Step`[r], regexpr("\\d+\\s", df$`Level and Step`[r])), "right")) # Condition 3. Determine which step

  # Doing a check if there is a range instead
  if (!(length(keep[,which(k3)]) == 1)) {
browser()
  }
  if (is.na(keep[,which(k3)])) {
    # See if there is any ranges
    if (any(sub(".*\\.", "", names(keep)[grepl("Range", names(keep))]) == trimws(regmatches(df$`Level and Step`[r], regexpr("\\d+\\s", df$`Level and Step`[r])), "right"))) {
      MED <- TRUE
      k <- which(grepl("Range", names(keep),ignore.case = TRUE))
      kk <- which(sub(".*\\.", "", names(keep)) == trimws(regmatches(df$`Level and Step`[r], regexpr("\\d+\\s", df$`Level and Step`[r])), "right"))
      k3 <- intersect(k, kk)
    }
  }

  if (!(MED)) {
  df$`Annual Salary`[r] <- keep[,which(k3)]
  } else {
    if (!(length(k3) == 0)) { # This means there is a range, but not for that step
    parts <- strsplit(keep[,k3], " to ")
    # Extract values
    num1 <- as.numeric(gsub(",", "", parts[[1]][1]))
    num2 <- as.numeric(gsub(",", "", parts[[1]][2]))
    df$`Annual Salary`[r] <- median(c(num1,num2))
    }
  }
}
final[[c]] <- df

} # end classification
}

FINAL <- do.call(rbind, final)

if (any(is.na(FINAL$`Annual Salary`))) {
df <- FINAL[-(which(is.na(FINAL$`Annual Salary`))),]
} else {
  df <- FINAL
}
