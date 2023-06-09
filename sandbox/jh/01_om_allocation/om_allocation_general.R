library(dataSPA)
# om <- getData(type="om", cookie=cookie)
ind <- om[which(om$project_id == 1093),]
category <- unique(ind$category_type)
years <- unique(ind$fiscal_year)
yearsx <- vector(mode="list", length(category))
amounty <- vector(mode="list", length(category))
for (i in seq_along(category)) {
  for (j in seq_along(years)) {
    value <- ind[which(ind$category_type == category[i]),] # Look at one category
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

# FIGURE 1 COMBINED LINES
plot(1:(length(years)+1), 1:(length(years)+1), type="o", pch=20, ylim=c(0,max*1.1), ylab="O&M Cost ($)", xlab=" ",xaxt = "n", col="white")
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


# FIGURE 2 INDEPENDANT Y-AXIS
par(mfrow=c(2,2), mar=c(2,4,2,0.5))
for (i in seq_along(category)) {
  plot(seq_along(yearsx[[i]]), unlist(amounty[[i]]), type="o", pch=20, col="black", xlab=" ", ylab=" ", xaxt="n")
  axis(
    1,
    at = seq_along(years),
    labels = years,
    #las = 2,
    cex.axis = 0.7
  )
  m <- lm(unlist(amounty[[i]])~seq_along(yearsx[[i]]))
  mtext(paste0(round(coef(m)[2],0), " $/year"), col="red", line=-1, cex=0.5)
  title(category[i])
}

# FIGURE 3 RELATIVE INDEPENDANT LINE CHART

par(mfrow=c(2,2), mar=c(2,4,2,0.5))
for (j in seq_along(category)) {
  plot(1:(length(years)), 1:(length(years)), type="o", pch=20, ylim=c(0,max*1.1), ylab="O&M Cost ($)", xlab=" ",xaxt = "n", col="white")
  for (i in seq_along(category)) {
    lines(seq_along(yearsx[[i]]), unlist(amounty[[i]]), type="o", pch=20, col="gray", cex=1.4)
  }
  lines(seq_along(yearsx[[j]]), unlist(amounty[[j]]), type="o", pch=20, col="red", cex=1.4)
  m <- lm(unlist(amounty[[j]])~seq_along(yearsx[[j]]))
  mtext(paste0(round(coef(m)[2],0), " $/year"), col="red", line=-1, cex=0.5)
  title(category[j])
}

# Figure 4 GROUPED BAR CHART

data <- matrix(NA,ncol=length(category), nrow=length(years))
colnames(data) <- category
row.names(data) <- years

amounty <- vector(mode="list", length(category))
for (i in seq_along(category)) {
  for (j in seq_along(years)) {
    value <- ind[which(ind$category_type == category[i]),] # Look at one category
    value2 <- value[which(value$fiscal_year == years[j]),] # Look at specific year
    amounty[[i]][[j]] <-sum(value2$amount, na.rm=TRUE)
  }
}

# Fill in data from amounty
for (i in seq_along(category)) {
  data[,i] <- unlist(unname(amounty[i]))
}
par(mfrow=c(1,1),mar = c(9, 4, 4, 2) + 0.1)
barplot(data, col=1:length(years), beside=T, las=2, ylim=c(0,max(unname(unlist(data)))*1.5), cex.names=0.7)
legend("topleft", years, col=1:length(years), pch=rep(20, length(years)), cex=0.8)

# Figure 5 INDEPENDANT BAR CHART
par(mfrow=c(4,2),mar=c(2,4,2,0.5))
for (i in seq_along(category)) {
  barplot(data[,i], cex.names=0.7, ylim=c(0,max(data[,i])*1.3))
  m <- lm(data[,i]~seq_along(yearsx[[i]]))
  mtext(paste0(round(coef(m)[2],0), " $/year"), col="red", line=-1, cex=0.5)
  title(category[i])
}
