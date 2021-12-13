library(ggplot2)
library(lessR)
library(viridis)
# get the data from a file included with lessR
d <- rd("Employee")

# --------------------------------------------------------
# pie (doughnut) chart from the data for a single variable
# --------------------------------------------------------

# basic pie chart, actually a doughnut or ring chart
# with default hcl colors (except for themes "gray" and "white")
PieChart(Dept)
# short name
#pc(Dept)

library(lessR)
#Joint measures
count<-c(16,63)
names(count) <- c("Degraded rigid joint","Unbound joint")
mycolors <- getColors("viridis",  n=5)
co1<-pc(count, fill=mycolors, main = "")

count2<-c(29,40,10)
names(count2) <- c("Sandstone setts","Concrete slabs", "Others")
#count2<-as.data.frame(count2)
co2<-pc(count2,fill= mycolors, main = "")

count3<-c(11,53,8,3,4)
names(count3) <- c("Front of house","Sidewalk", "Internal yard", "Road","Others")
co3<-pc(count3, fill=mycolors, main = "")

