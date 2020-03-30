###### Coursework: 4248337 - Tal Rozenman #######
###### G54DMA - Plant Dataset #####

# Set work directory
setwd("C:/Users/thoma/OneDrive/Documents/R/7111PartA/Project_A")

# Install packages
#install.packages("ggplot2")
#install.packages("xtable")
#install.packages("stargazer")
#install.packages("fpc")
#install.packages("cluster")
#install.packages("fossil")

# load package
library(stargazer)
library(ggplot2)
library(xtable)
#library(fpc)
library(cluster)
library(fossil)
library(tidyr)

# Load Dataset
library(readxl)
Data_2019 <- read_excel("data 2019 tom.xlsx")
Data_2019.numeric <- Data_2019[,sapply(Data_2019, is.numeric)]

i1 <- sapply(Data_2019, is.numeric)
y1 <- "Througput" #change it to actual column name
x1 <- setdiff(names(Data_2019)[i1], y1)
cor(Data_2019[x1], Data_2019[[y1]], use = "complete.obs")
cor(Data_2019$Througput, Data_2019$Feed_Density, use = "complete.obs")
data.correlation <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,2:31], use = "complete.obs")
data.correlation2 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,32], use = "complete.obs")
data.correlation3 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,33:53], use = "complete.obs")
data.correlation4 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,54], use = "complete.obs")
data.correlation5 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,55], use = "complete.obs")
data.correlation6 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,56], use = "complete.obs")
data.correlation7 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,57], use = "complete.obs")
data.correlation8 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,58], use = "complete.obs")
data.correlation9 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,59:81], use = "complete.obs")
data.correlation10 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,82], use = "complete.obs") #83 unavailable
data.correlation11 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,84], use = "complete.obs")
data.correlation12 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,85:97], use = "complete.obs") #98 unavailable
data.correlation13 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,100:103], use = "complete.obs") #99 is Set point
data.correlation14 <-  cor(Data_2019.numeric$Througput, Data_2019.numeric[,104], use = "complete.obs")
data.correlation15 <-  cor(Data_2019.numeric$Througput, Data_2019.numeric[,105], use = "complete.obs")
data.correlation16 <-  cor(Data_2019.numeric$Througput, Data_2019.numeric[,106], use = "complete.obs")
data.correlation17 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,107], use = "complete.obs")
data.correlation18 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,108], use = "complete.obs")
data.correlation19 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,109:111], use = "complete.obs")
data.correlation20 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,113:121], use = "complete.obs")#122 is set point
data.correlation21 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,123:127], use = "complete.obs")
data.correlation22 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,128], use = "complete.obs")
data.correlation23 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,129], use = "complete.obs")
data.correlation24 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,130], use = "complete.obs")
data.correlation25 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,131:139], use = "complete.obs")#140 is set point
data.correlation26 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,141:146], use = "complete.obs")
data.correlation27 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,147], use = "complete.obs")
data.correlation28 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,148], use = "complete.obs")
data.correlation29 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,149], use = "complete.obs")
data.correlation30 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,150:152], use = "complete.obs")
data.correlation31 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,153], use = "complete.obs")
data.correlation32 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,154], use = "complete.obs")
data.correlation33 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,155], use = "complete.obs")
data.correlation34 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,156], use = "complete.obs")
data.correlation35 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,157], use = "complete.obs")
data.correlation36 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,158], use = "complete.obs")
data.correlation37 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,159], use = "complete.obs")
data.correlation38 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,160], use = "complete.obs")
data.correlation39 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,161], use = "complete.obs")
data.correlation40 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,162:166], use = "complete.obs")
data.correlation41 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,168:171], use = "complete.obs") #167 is Set point
data.correlation42 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,173], use = "complete.obs")#172 is Set point
data.correlation43 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,174], use = "complete.obs")
data.correlation44 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,175], use = "complete.obs")
data.correlation45 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,176:177], use = "complete.obs")
data.correlation46 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,179], use = "complete.obs")
data.correlation47 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,180], use = "complete.obs")
data.correlation48 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,181], use = "complete.obs")
data.correlation49 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,182:187], use = "complete.obs")#188 is Set point
data.correlation50 <- cor(Data_2019.numeric$Througput, Data_2019.numeric[,189:190], use = "complete.obs")

data.correlation.tot <- Reduce(function(x, y) merge(x, y, all=TRUE), list(data.correlation, data.correlation2, data.correlation3, data.correlation4, data.correlation5, 
                                                                          data.correlation6, data.correlation7, data.correlation8, data.correlation9, data.correlation10,
                                                                          data.correlation11, data.correlation12, data.correlation13, data.correlation14, data.correlation15,
                                                                          data.correlation16, data.correlation17, data.correlation18, data.correlation19, data.correlation20,
                                                                          data.correlation21, data.correlation22, data.correlation23, data.correlation24, data.correlation25,
                                                                          data.correlation26, data.correlation27, data.correlation28, data.correlation29, data.correlation30,
                                                                          data.correlation31, data.correlation32, data.correlation33, data.correlation34, data.correlation35,
                                                                          data.correlation36, data.correlation37, data.correlation38, data.correlation39, data.correlation40,
                                                                          data.correlation41, data.correlation42, data.correlation43, data.correlation44, data.correlation45,
                                                                          data.correlation46, data.correlation47, data.correlation48, data.correlation49, data.correlation50))


numbers <- c(2:35)

summary(data.correlation)

plot(numbers, data.correlation)
class(data.correlation)


plot1 <- plot(Data_2019.numeric[,2:10], Data_2019.numeric$Througput)

plot2 <- plot(Data_2019.numeric[,11:20], Data_2019.numeric$Througput)

pairs(Data_2019.numeric)