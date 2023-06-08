library(dataSPA)
# om <- getData(type="om", cookie=cookie)
ind <- om[which(om$project_id == 1093),]
category <- unique(ind$category_display)
years <- unique(ind$fiscal_year)
yearsx <- vector(mode="list", length(category))
amounty <- vector(mode="list", length(category))
for (i in seq_along(category)) {
  for (j in seq_along(years)) {
  value <- ind[which(ind$category_display == category[i]),] # Look at one category
  value2 <- value[which(value$fiscal_year == years[j]),] # Look at specific year
  yearsx[[i]][[j]] <- unique(value2$fiscal_year)
  amounty[[i]][[j]] <-sum(value2$amount, na.rm=TRUE)
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

max <- lapply(amounty, max)
max <- max(unlist(unname(max))) # Find ylim
plot(1:(length(years)+3), 1:(length(years)+3), type="o", pch=20, ylim=c(0,max*1.1), ylab="O&M Cost ($)", xlab=" ",xaxt = "n", col="white")
for (i in seq_along(category)) {
  lines(seq_along(yearsx[[i]]), unlist(amounty[[i]]), type="o", pch=20, col=i)
}
axis(
  1,
  at = seq_along(years),
  labels = years,
  las = 2,
  cex.axis = 0.7
)
legend("topright", category, col=1:length(category), pch=rep(20, length(category)), cex=0.55)
