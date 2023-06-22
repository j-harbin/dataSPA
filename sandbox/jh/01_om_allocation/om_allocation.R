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
# FIGURE 1 (COMBINED LINES)
plot(1:(length(years)+2), 1:(length(years)+2), type="o", pch=20, ylim=c(0,max*1.1), ylab="O&M Cost ($)", xlab=" ",xaxt = "n", col="white")
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
par(mfrow=c(4,2), mar=c(2,4,2,0.5))
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
# FIGURE 3 GRAYED OUT LINES
par(mfrow=c(4,2), mar=c(2,4,2,0.5))
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

# FIGURE 4 GROUPED BAR CHART
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

par(mfrow=c(1,1),mar = c(12, 4, 4, 2) + 0.1)
barplot(data, col=1:length(years), beside=T, las=2, ylim=c(0,max(unname(unlist(data)))*1.5), cex.names=0.7)
legend("topleft", years, col=1:length(years), pch=rep(20, length(years)), cex=0.8)

# FIGURE 5 STACKED BAR CHART
data2 <- matrix(NA,ncol=length(years), nrow=length(category))
colnames(data2) <- years
row.names(data2) <- category

for (i in seq_along(category)) {
  data2[i,] <- unlist(unname(amounty[i]))
}

barplot(data2, beside=FALSE, col=1:length(category), las=2, ylim=c(0,max(unname(unlist(data)))*2.5), cex.names=0.7)
legend("topleft", category, col=1:length(category), pch=rep(20, length(category)), cex=0.5)

# FIGURE 6 INDEPENDANT BAR CHART
par(mfrow=c(4,2),mar=c(2,4,2,0.5))
for (i in seq_along(category)) {
  barplot(data[,i], cex.names=0.7, ylim=c(0,max(data[,i])*1.3))
  m <- lm(data[,i]~seq_along(yearsx[[i]]))
  mtext(paste0(round(coef(m)[2],0), " $/year"), col="red", line=-1, cex=0.5)
  title(category[i])
}

# FIGURE 7 SCALE LINES
for (i in seq_along(amounty)){
  amounty[[i]] <- unlist(amounty[[i]])
}
graphs <- NULL
for (i in seq_along(amounty)) {
  means <- mean(amounty[[i]], na.rm=TRUE)
  if (means < 1001) {
    graphs[[i]] <- "A"
  } else if (means > 1000 & means < 100001) {
    graphs[[i]] <- "B"
  } else if (means > 100001) {
    graphs[[i]] <- "C"
  }
}

par(mfrow=c(3,1), mar=c(1.5,5,3,2) + 0.1)
plot(1:(length(years)+1), 1:(length(years)+1), type="o", pch=20, ylim=c(0,2000), ylab="O&M Cost ($)", xlab=" ",xaxt = "n", col="white")
for (i in seq_along(category[which(graphs == "A")])) {
  lines(seq_along(yearsx[which(graphs == "A")][[i]]), unlist(amounty[which(graphs == "A")][[i]]), type="o", pch=20, col=which(graphs == "A")[[i]])
}
legend("topright", category[which(graphs == "A")], col=c(which(graphs == "A")), pch=rep(20, length(category[which(graphs == "A")])), cex=0.55)
axis(
  1,
  at = seq_along(years),
  labels = years,
  cex.axis = 1
)

plot(1:(length(years)+1), 1:(length(years)+1), type="o", pch=20, ylim=c(1000,100000), ylab="O&M Cost ($)", xlab=" ",xaxt = "n", col="white")
for (i in seq_along(category[which(graphs == "B")])) {
  lines(seq_along(yearsx[which(graphs == "B")][[i]]), unlist(amounty[which(graphs == "B")][[i]]), type="o", pch=20, col=which(graphs == "B")[[i]])
}
legend("topright", category[which(graphs == "B")], col=c(which(graphs == "B")), pch=rep(20, length(category[which(graphs == "B")])), cex=0.55)
axis(
  1,
  at = seq_along(years),
  labels = years,
  cex.axis = 1
)

plot(1:(length(years)+1), 1:(length(years)+1), type="o", pch=20, ylim=c(100000,max(unlist(amounty[which(graphs == "C")]))*1.3), ylab="O&M Cost ($)", xlab=" ",xaxt = "n", col="white")
for (i in seq_along(category[which(graphs == "C")])) {
  lines(seq_along(yearsx[which(graphs == "C")][[i]]), unlist(amounty[which(graphs == "C")][[i]]), type="o", pch=20, col=which(graphs == "C")[[i]])
}
legend("topright", category[which(graphs == "C")], col=c(which(graphs == "C")), pch=rep(20, length(category[which(graphs == "C")])), cex=0.55)
axis(
  1,
  at = seq_along(years),
  labels = years,
  cex.axis = 1
)

# FIGURE 8 SCALE GROUP BAR CHART

par(mfrow=c(3,1), mar=c(1.5,5,3,2) + 0.1)
for (i in c("A", "B", "C")) {
barplot(data[,which(graphs == i)], col=1:length(years), beside=T, ylim=c(0,max(unname(unlist(data[,which(graphs == i)])))*2.5), cex.names=0.7)
if (i == "A") {
legend("topleft", years, col=1:length(years), pch=rep(20, length(years)), cex=0.8)
}
}


