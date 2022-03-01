library(tidyverse)
library(dplyr)
library(viridis)
library(ggplot2)
library(cowplot)
library(plot3D)
library(plotly)


#3D plots for each samples
fname = "DatawithCTRL.csv"
data = read.delim(fname, sep=",", stringsAsFactors=FALSE)
unique(data$Filename)


#Cut off
X <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "JG1" |
           Filename == "JG4" |
           Filename == "DS1" |
           Filename == "DS4" |
           Filename == "AB1" |
           Filename == "AB4" |
           Filename == "PL1" |
           Filename == "PL4" |
           Filename == "PJ1" |
           Filename == "PJ4")

write.csv(X, "Cutoffdata.csv")

I <- ggplot(X, aes(x=Myh7)) +
  geom_density(fill="grey50", color="grey50", alpha=0.6) +
  geom_vline(xintercept=250, linetype='dashed', color='black', size=1) +
  xlab("Myh7 intensity value")+
  ylim(0,0.008) +
  xlim(0,4000)
I

IIa <- ggplot(X, aes(x=Myh2)) +
  geom_density(fill="grey50", color="grey50", alpha=0.6) +
  geom_vline(xintercept=500, linetype='dashed', color='black', size=1) +
  xlab("Myh2 intensity value")+
  ylim(0,0.008)+
  xlim(0,4000)
IIa 

IIx <- ggplot(X, aes(x=Myh1)) +
  geom_density(fill="grey50", color="grey50", alpha=0.6) +
  geom_vline(xintercept=400, linetype='dashed', color='black', size=1) +
  xlab("Myh1 intensity value")+
  ylim(0,0.008)+
  xlim(0,4000)
IIx

fname = "Cutoff_data.csv"
data = read.delim(fname, sep=",", stringsAsFactors=FALSE)
unique(data$Type)

all <- ggplot(data, aes(x=Value, group=Type, fill=Type, colour=Type)) +
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c("purple", "firebrick2", "darkgreen")) +
  scale_colour_manual(values=c("purple", "firebrick2", "darkgreen")) +
  xlab("Intensity channels value") +
  ylim(0,0.008)+
  xlim(0,4000)+
  theme ( legend.position = "none")
all 


#ALL samples including controls and patients
D <- data %>%
  select(Myh2, Myh1, Myh7, FibreType) %>%
  filter(FibreType == "Myh2+" |
           FibreType == "Myh1+" |
           FibreType == "Myh7+")

D$FibreType[which(D$FibreType == 'Myh2')] <- 'Myh2+'
D$FibreType[which(D$FibreType == 'Myh1')] <- 'Myh1+'
D$FibreType[which(D$FibreType == 'Myh7')] <- 'Myh7+'
D$FibreType <- as.factor(D$FibreType)

ALL <- plot_ly(D, x = ~Myh2, y = ~Myh1, z = ~Myh7, color = ~FibreType,
               colors = c('purple', 'firebrick2', 'darkgreen'), size = 3)
ALL <- ALL %>% add_markers()
ALL <- ALL %>% layout(scene = list(xaxis = list(title = 'Myh2'),
                                   yaxis = list(title = 'Myh1'),
                                   zaxis = list(title = 'Myh7')))
ALL

#Controls only
C <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "M1189-16" |
           Filename == "M1180-16" |
           Filename == "M1212-16" |
           Filename == "M1217-16")
Controls <- C %>%
  select(Myh2, Myh1, Myh7, FibreType)

Controls$FibreType[which(Controls$FibreType == 'Myh2')] <- 'Myh2+'
Controls$FibreType[which(Controls$FibreType == 'Myh1')] <- 'Myh1+'
Controls$FibreType[which(Controls$FibreType == 'Myh7')] <- 'Myh7+'
Controls$FibreType <- as.factor(Controls$FibreType)

CTRL <- plot_ly(Controls, x = ~Myh2, y = ~Myh1, z = ~Myh7, color = ~FibreType,
               colors = c('purple', 'firebrick2', 'darkgreen'), size = 3)
CTRL <- CTRL %>% add_markers()
CTRL <- CTRL %>% layout(scene = list(xaxis = list(title = 'Myh2'),
                                   yaxis = list(title = 'Myh1'),
                                   zaxis = list(title = 'Myh7')))
CTRL

#Pre-exercise
A <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename, Exercise) %>%
  filter(Filename == "AB1" |
           Filename == "DS1" |
           Filename == "JG1" |
           Filename == "PL1" |
           Filename == "PJ1")
Pre <- A %>%
  select(Myh2, Myh1, Myh7, FibreType)

Pre$FibreType[which(Pre$FibreType == 'Myh2')] <- 'Myh2+'
Pre$FibreType[which(Pre$FibreType == 'Myh1')] <- 'Myh1+'
Pre$FibreType[which(Pre$FibreType == 'Myh7')] <- 'Myh7+'
Pre$FibreType <- as.factor(Pre$FibreType)

PRE <- plot_ly(Pre, x = ~Myh2, y = ~Myh1, z = ~Myh7, color = ~FibreType,
                colors = c('purple', 'firebrick2', 'darkgreen'), size = 3)
PRE <- PRE %>% add_markers()
PRE <- PRE %>% layout(scene = list(xaxis = list(title = 'Myh2'),
                                     yaxis = list(title = 'Myh1'),
                                     zaxis = list(title = 'Myh7')))

PRE

#Post-exercise
B <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename, Exercise) %>%
  filter(Filename == "AB4" |
           Filename == "DS4" |
           Filename == "JG4" |
           Filename == "PL4" |
           Filename == "PJ4")
Post <- B %>%
  select(Myh2, Myh1, Myh7, FibreType)

Post$FibreType[which(Post$FibreType == 'Myh2')] <- 'Myh2+'
Post$FibreType[which(Post$FibreType == 'Myh1')] <- 'Myh1+'
Post$FibreType[which(Post$FibreType == 'Myh7')] <- 'Myh7+'
Post$FibreType <- as.factor(Post$FibreType)

POST <- plot_ly(Post, x = ~Myh2, y = ~Myh1, z = ~Myh7, color = ~FibreType,
               colors = c('purple', 'firebrick2', 'darkgreen'), size = 3)
POST <- POST %>% add_markers()
POST <- POST %>% layout(scene = list(xaxis = list(title = 'Myh2'),
                                   yaxis = list(title = 'Myh1'),
                                   zaxis = list(title = 'Myh7')))

POST

#Disease cohort
#Post-exercise
J <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename, Exercise) %>%
  filter(Filename == "JG1" |
           Filename == "JG4" |
           Filename == "DS1" |
           Filename == "DS4" |
           Filename == "AB1" |
           Filename == "AB4" |
           Filename == "PL1" |
           Filename == "PL4" |
           Filename == "PJ1" |
           Filename == "PJ4")

Dis <- J %>%
  select(Myh2, Myh1, Myh7, FibreType)

Dis$FibreType[which(Dis$FibreType == 'Myh2')] <- 'Myh2+'
Dis$FibreType[which(Dis$FibreType == 'Myh1')] <- 'Myh1+'
Dis$FibreType[which(Dis$FibreType == 'Myh7')] <- 'Myh7+'
Dis$FibreType <- as.factor(Dis$FibreType)

DIS <- plot_ly(Dis, x = ~Myh2, y = ~Myh1, z = ~Myh7, color = ~FibreType,
                colors = c('purple', 'firebrick2', 'darkgreen'), size = 3)
DIS <- DIS %>% add_markers()
DIS <- DIS %>% layout(scene = list(xaxis = list(title = 'Myh2'),
                                     yaxis = list(title = 'Myh1'),
                                     zaxis = list(title = 'Myh7')))

DIS

#Single sample
P11 <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "JG1" |
         Filename == "FAKE")
X <- P11 %>%
  select(Myh2, Myh1, Myh7, FibreType)

P14 <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "JG4"|
           Filename == "FAKE")
X <- P14 %>%
  select(Myh2, Myh1, Myh7, FibreType)

P21 <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "DS1"|
           Filename == "FAKE")
X <- P21 %>%
  select(Myh2, Myh1, Myh7, FibreType)

P24 <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "DS4"|
           Filename == "FAKE")
X <- P24 %>%
  select(Myh2, Myh1, Myh7, FibreType)

P31 <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "AB1"|
           Filename == "FAKE")
X <- P31 %>%
  select(Myh2, Myh1, Myh7, FibreType)

P34 <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "AB4"|
           Filename == "FAKE")
X <- P34 %>%
  select(Myh2, Myh1, Myh7, FibreType)

P41 <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "PL1"|
           Filename == "FAKE")
X <- P41 %>%
  select(Myh2, Myh1, Myh7, FibreType)

P44 <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "PL4"|
           Filename == "FAKE")
X <- P44 %>%
  select(Myh2, Myh1, Myh7, FibreType)

P51 <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "PJ1"|
           Filename == "FAKE")
X <- P51 %>%
  select(Myh2, Myh1, Myh7, FibreType)

P54 <- data %>%
  select(Myh2, Myh1, Myh7, FibreType, Filename) %>%
  filter(Filename == "PJ4" |
           Filename == "FAKE")
X <- P54 %>%
  select(Myh2, Myh1, Myh7, FibreType)

X$FibreType[which(X$FibreType == 'Myh2')] <- 'Myh2+'
X$FibreType[which(X$FibreType == 'Myh1')] <- 'Myh1+'
X$FibreType[which(X$FibreType == 'Myh7')] <- 'Myh7+'
X$FibreType <- as.factor(X$FibreType)

Fig <- plot_ly(X, x = ~Myh2, y = ~Myh1, z = ~Myh7, 
               color = ~FibreType, colors = c('purple', 'firebrick2', 'darkgreen'),
               size = 3)
Fig <- Fig %>% add_markers()
Fig <- Fig %>% layout(scene = list(xaxis = list(title = 'Myh2'),
                                     yaxis = list(title = 'Myh1'),
                                     zaxis = list(title = 'Myh7')))

Fig
