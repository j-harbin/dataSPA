library(dataSPA)
# om <- getData(type="om", cookie=cookie)
ind <- om[which(om$project_id == 1093),]
category <- unique(ind$category_display)
years <- unique(ind$fiscal_year)
data <- matrix(NA,ncol=length(category), nrow=length(years))
colnames(data) <- category
row.names(data) <- years

amounty <- vector(mode="list", length(category))
for (i in seq_along(category)) {
  for (j in seq_along(years)) {
    value <- ind[which(ind$category_display == category[i]),] # Look at one category
    value2 <- value[which(value$fiscal_year == years[j]),] # Look at specific year
    amounty[[i]][[j]] <-sum(value2$amount, na.rm=TRUE)
  }
}

# Fill in data from amounty
for (i in seq_along(category)) {
data[,i] <- unlist(unname(amounty[i]))
}

# Grouped
#par(mar = c(12, 4, 4, 2) + 0.1)
barplot(data, col=1:length(years), beside=T, las=2, ylim=c(0,max(unname(unlist(data)))*1.5), cex.names=0.7)
legend("topleft", years, col=1:length(years), pch=rep(20, length(years)), cex=0.8)

# Stacked by year
data2 <- matrix(NA,ncol=length(years), nrow=length(category))
colnames(data2) <- years
row.names(data2) <- category

for (i in seq_along(category)) {
  data2[i,] <- unlist(unname(amounty[i]))
}

barplot(data2, beside=FALSE, col=1:length(category), las=2, ylim=c(0,max(unname(unlist(data)))*2.5), cex.names=0.7)
legend("topleft", category, col=1:length(category), pch=rep(20, length(category)), cex=0.5)
