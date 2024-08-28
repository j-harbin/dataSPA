om <- getData(type="om", cookie="cookie", age=10000)

TAGS <- NULL
for (i in seq_along(unique(om$tags))) {
  TAGS[[i]] <- strsplit(unique(om$tags)[i], ",\\s*")[[1]]
}
TAGS <- sort(unique(unlist(TAGS)))

tags <- c("biomass metrics", "structure and function", "threats to productivity", "genetic diversity", "species diversity", "functional diversity", "representativity", "key fish habitat", "connectivity", "uniqueness", "threats to habitat")

keep <- NULL

for (i in seq_along(tags)) {
  keep[[i]] <- TAGS[which(grepl(tags[i], TAGS, ignore.case=TRUE))]
}

