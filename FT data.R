# Libraries
library(ggplot2)
library(viridis)
library(tidyverse)
library(cowplot)
library(plot3D)

#Multidimensional approach
fname = "DatawithCTRL.csv"         
dat = read.csv("DatawithCTRL.csv",stringsAsFactors=FALSE)

# Find the max value between numeric string
dat2 <- dat %>%
  select("Ch2", "Ch3", "Ch4")
dat2$colMax <- apply(dat2, 1, function(x) max(x))

#Merge two dataframes
data = merge(dat2, dat)

# Make fibre type classification
data$Type <- ifelse(data$colMax == data$Ch3, 'Myh2+',
                    ifelse(data$colMax == data$Ch4, 'Myh1+', 'Myh7+'))

# Rename channels
x <- Myh2 <- data$Ch3
y <- Myh1 <- data$Ch4
z <- Myh7 <- data$Ch2

#Do 3D plot with all cohort
scatter3D(x, y, z, alpha = 0.4, pch = 16, cex = 0.7,
          data=data, colvar = NULL, theta =135, phi = 30, bty ="g",
          xlab = "Myh2+", ylab ="Myh1+", zlab = "Myh7+", 
          ticktype = "simple",
          col.var = as.integer(data$Type), col = c("blue", "green", "red"))
#P3 pre exercise
P3pre <- dat %>%
  filter(Filename == "P3") %>%
  filter(Training == " Pre")
P3pre

x <- Myh2 <- P3pre$Ch3
y <- Myh1 <- P3pre$Ch4
z <- Myh7 <- P3pre$Ch2

scatter3D(x, y, z, alpha = 0.4, pch = 16, cex = 0.7,
          data=P3pre, colvar = NULL, theta =135, phi = 30, bty ="g",
          xlab = "Myh2+", ylab ="Myh1+", zlab = "Myh7+", 
          ticktype = "simple",
          col.var = as.integer(P3pre$Type), col = c("blue", "green", "red"))
#P3 post exercise
P3post <- dat %>%
  filter(Filename == "P3") %>%
  filter(Training == "Post")
P3post

x <- Myh2 <- P3post$Ch3
y <- Myh1 <- P3post$Ch4
z <- Myh7 <- P3post$Ch2

scatter3D(x, y, z, alpha = 0.4, pch = 16, cex = 0.7,
          data=P3post, colvar = NULL, theta =135, phi = 30, bty ="g",
          xlab = "Myh2+", ylab ="Myh1+", zlab = "Myh7+", 
          ticktype = "simple",
          col.var = as.integer(P3post$Type), col = c("blue", "green", "red"))