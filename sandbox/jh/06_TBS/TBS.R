library(dataSPA)
library(TBSpayRates)
library(stringr)
#groups <- c("AI", "AO", "AV", "CS", "CX", "EC", "EL", "FB", "FI", "FS", "LP", "NR", "PA", "PR", "RE", "RO", "SO", "SP", "TC", "TR", "UT")
groups <- "RE"
letters <- FALSE
final <- NULL
for (g in seq_along(groups)) { # 1. Cycle through each lead group
g <- 1
group <- groups[[g]]
sal <-  get_salaries(groups = group)

# Create condition for AS--Development compared to AS-02 and PM-- Development
if (any(grepl("development", sal$Classification, ignore.case = TRUE))) {
  d1 <- which(grepl("development", sal$Classification, ignore.case=TRUE))
  if (any(grepl("--", sal$Classification, ignore.case=TRUE))) {
    d2 <- which(grepl("--", sal$Classification, ignore.case=TRUE))
    d3 <- intersect(d1,d2)
    sal$Classification[d3] <- gsub("--", "-", sal$Classification[d3])
  }

}


# Check if there is - at the end of sal$Classification (group=PA)
hyphen <- grepl("-$", sal$Classification)
sal$Classification[which(hyphen)] <- sub("-$", "", sal$Classification[which(hyphen)])


#TEST
 if (any(grepl("special", sal$Classification, ignore.case=TRUE))) {
 s1 <- which(grepl("special", sal$Classification, ignore.case=TRUE))


 find_sequence_end <- function(vec) {
   for (i in 2:length(vec)) {
     if (vec[i] != vec[i - 1] + 1) {
       return(i - 1)
     }
   }
   return(length(vec))
 }


 # Use lapply to apply the function to each element of the vector
 breaks <- unlist(lapply(list(s1), find_sequence_end))  # This is the location that the sequence ends
 breaks <- c(1,breaks) # Adding the first special

 for (b in seq_along(breaks)) {
   if (!(b == max(seq_along(breaks)))) {
     range1 <- b
     range2 <- breaks[b+1]
   } else {
     range1 <- breaks[b]+1
     range2 <- length(s1)
   }

   specialRange <- s1[range1:range2]
   #DA-CON-SpecialLevelC
   sal$Classification[specialRange] <- paste0(sub("^(.*)-.*$", "\\1", sal$Classification[s1[range1]-1]), "-", gsub("-", "", gsub(" ", "", sal$Classification[specialRange]))) # 1. Get DA-CON 2. Remove - 3. remove spaces
 }
 }




# First check that the - is a number (this is for group=AV, classification= CO-DEV/PER)
classification <- substr(sal$Classification, 1, 2)




# END TEST

for (c in seq_along(unique(classification))) { # 2. Cycle through classifications
salary <- as.data.frame(sal[which(classification == unique(classification)[c]),])

# Test that there is 01 rather than 1
test <- trimws(sub("^.+-(.*)$", "\\1", salary$Classification), "left") # Keep everything after the last -
#test <- unlist(lapply(strsplit(salary$Classification, "-"), function(x) trimws(x[2], "left")))
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

listy <- unlist(listy)



listx <- NULL
for (cl in seq_along(unique(sub("^(.*)-.*$", "\\1", unique(salary$Classification))))) { # Added for SE-RES/SE-REM (https://github.com/dfo-mar-odis/TBSpayRates/issues/18)
starting <- gsub("-", "--", unique(sub("^(.*)-.*$", "\\1", unique(salary$Classification)))[cl])
replacing <-sub("\\s\\d{2}$", "", listy) # Remove last two letters at the end "SE--RES--01-1 21" to "SE--RES--01-1"
replacing <- sub("-[^-]*$", "", replacing) # Removing everything after last - "SE--RES--01-1" to "SE--RES--01"
replacing <- sub("--[^-]*$", "", replacing) # Keep everything before last -- "SE--RES--01-1 21" to "SE--RES"
listx[[cl]] <- gsub(replacing[1],starting,listy)
}

listy <- unlist(listx)

LevelAndStep <- unlist(listy)

# Do a test for instances like SG-SRE-01
# AC--01-1 21" (levelandstep for regular) and Classification is "AC"
# "SG--SRE--01-1 21" and Class: "SG"

Classification <- rep(strsplit(LevelAndStep, "-")[[1]][1], length(LevelAndStep))
df <- data.frame(matrix(NA, nrow = length(LevelAndStep), ncol = 3))
names(df) <- c("Classification", "Level and Step", "Annual Salary")
df$Classification <- Classification
df$`Level and Step` <- LevelAndStep
# if (c == 8) {
#   browser()
# }
for (r in seq_along(1:nrow(df))) { # 3. Go through df to assign salary steps
  message("r = ", r, " and c = ", c)
  MED <- FALSE
  if (length(strsplit(df$`Level and Step`[r], "-")[[1]]) == 6) {
    # Three letter
    #k1 <- which(unlist(lapply(strsplit(salary$Classification, "-"), function(x) x[3])) == sub(".*?(\\d+).*", "\\1", df$`Level and Step`[r])) # Condition 1: Check the "01"
    k1 <- which(unlist(lapply(strsplit(salary$Classification, "-"), function(x) x[3])) == sub(".*--([^\\-]*)-.*", "\\1", df$`Level and Step`[r])) # Condition 1: Check the "01"

    if (length(strsplit(salary$Classification[k1], "-")[[1]]) == 3) {
      # This means we have a situation like SG-SRE-01 and SG-PAT-01
      # If there is a nextK this means we have a case such as SE-REM-02 and SE-RES-02
      nextK <- which(sub(".*-(.*?)-.*", "\\1", salary$Classification) == sub(".*--(.*?)-.*", "\\1", df$`Level and Step`[r]))
      k1 <- intersect(k1, nextK)  # This is like OE-BEO-03-1 20 (there is no 03) problem r=121
    }
  } else {
    k1 <- which(unlist(lapply(strsplit(salary$Classification, "-"), function(x) x[2])) == sub(".*--(.*?)-.*", "\\1", df$`Level and Step`[r])) # Condition 1: Check the "01"
  }
  k2 <- which(year == sub("^[^ ]+ ", "", df$`Level and Step`[r])) # Condition 2: Making sure the year is the same
  keep <- salary[intersect(k1, k2),]
  k3 <- unlist(lapply(strsplit(names(keep), "\\."), function(x) x[2]) == trimws(regmatches(df$`Level and Step`[r], regexpr("\\d+\\s", df$`Level and Step`[r])), "right")) # Condition 3. Determine which step

  if (exists("nextK") && length(k1) == 0) {
    # This is a test for OE-BEO-03-1 20 (there is no 03), r=121 in group="PA"
    names <- names(keep)
   keep <- data.frame(matrix(NA, ncol = ncol(keep), nrow = 1))
   names(keep) <- names
  }

  # Doing a check if there is a range instead
  if (!(length(keep[,which(k3)]) == 1)) {
browser()
  }
  if (is.na(keep[,which(k3)]) && !(exists("nextK"))) {
    if (exists("nextK")) {
    rm(nextK)
    }
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
