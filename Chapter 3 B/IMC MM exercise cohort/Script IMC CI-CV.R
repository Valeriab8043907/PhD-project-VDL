library(tidyverse)
library(dplyr)
library(ggplot2)
library(viridis)
library(cowplot)
library(plot3D)
library(RColorBrewer)
library(plotly)

#Read the files ande merge them

fname0 = "Controls.csv"
dat0 = read.delim(fname0, sep=",", stringsAsFactors=FALSE)

fname1 = "P1_1_A.csv"
dat1 = read.delim(fname1, sep=",", stringsAsFactors=FALSE)
unique(dat1$Folder.1)
name = c("P1_1_A")
names(name) = c("P1-1")
dat1$Folder.1 = name[dat1$Folder.1]

fname2 = "P1_1_B.csv"
dat2 = read.delim(fname2, sep=",", stringsAsFactors=FALSE)
unique(dat2$Folder.1)
name = c("P1_1_B")
names(name) = c("P1-1")
dat2$Folder.1 = name[dat2$Folder.1]

fname3 = "P1_4_A.csv"
dat3 = read.delim(fname3, sep=",", stringsAsFactors=FALSE)
unique(dat3$Folder.1)
name = c("P1_4_A")
names(name) = c("P1-4")
dat3$Folder.1 = name[dat3$Folder.1]

fname4 = "P1_4_B.csv"
dat4 = read.delim(fname4, sep=",", stringsAsFactors=FALSE)
unique(dat4$Folder.1)
name = c("P1_4_B")
names(name) = c("P1-4")
dat4$Folder.1 = name[dat4$Folder.1]

fname5 = "P2_1_A.csv"
dat5 = read.delim(fname5, sep=",", stringsAsFactors=FALSE)
unique(dat5$Folder.1)
name = c("P2_1_A")
names(name) = c("P2-1")
dat5$Folder.1 = name[dat5$Folder.1]

fname6 = "P2_1_B.csv"
dat6 = read.delim(fname6, sep=",", stringsAsFactors=FALSE)
unique(dat6$Folder.1)
name = c("P2_1_B")
names(name) = c("P2-1")
dat6$Folder.1 = name[dat6$Folder.1]

fname7 = "P2_4_A.csv"
dat7 = read.delim(fname7, sep=",", stringsAsFactors=FALSE)
unique(dat7$Folder.1)
name = c("P2_4_A")
names(name) = c("P2-4")
dat7$Folder.1 = name[dat7$Folder.1]

fname8 = "P2_4_B.csv"
dat8 = read.delim(fname8, sep=",", stringsAsFactors=FALSE)
unique(dat8$Folder.1)
name = c("P2_4_B")
names(name) = c("P2-4")
dat8$Folder.1 = name[dat8$Folder.1]

fname9 = "P3_1_A.csv"
dat9 = read.delim(fname9, sep=",", stringsAsFactors=FALSE)
unique(dat9$Folder.1)
name = c("P3_1_A")
names(name) = c("P3-1")
dat9$Folder.1 = name[dat9$Folder.1]

fname10 = "P3_1_B.csv"
dat10 = read.delim(fname10, sep=",", stringsAsFactors=FALSE)
unique(dat10$Folder.1)
name = c("P3_1_B")
names(name) = c("P3-1")
dat10$Folder.1 = name[dat10$Folder.1]

fname11 = "P3_4_A.csv"
dat11 = read.delim(fname11, sep=",", stringsAsFactors=FALSE)
unique(dat11$Folder.1)
name = c("P3_4_A")
names(name) = c("P3-4")
dat11$Folder.1 = name[dat11$Folder.1]

fname12 = "P3_4_B.csv"
dat12 = read.delim(fname12, sep=",", stringsAsFactors=FALSE)
unique(dat12$Folder.1)
name = c("P3_4_B")
names(name) = c("P3-4")
dat12$Folder.1 = name[dat12$Folder.1]

fname13 = "P4_1_A.csv"
dat13 = read.delim(fname13, sep=",", stringsAsFactors=FALSE)
unique(dat13$Folder.1)
name = c("P4_1_A")
names(name) = c("P4-1")
dat13$Folder.1 = name[dat13$Folder.1]

fname14 = "P4_1_B.csv"
dat14 = read.delim(fname14, sep=",", stringsAsFactors=FALSE)
unique(dat14$Folder.1)
name = c("P4_1_B")
names(name) = c("P4-1")
dat14$Folder.1 = name[dat14$Folder.1]

fname15 = "P4_4_A.csv"
dat15 = read.delim(fname15, sep=",", stringsAsFactors=FALSE)
unique(dat15$Folder.1)
name = c("P4_4_A")
names(name) = c("P4-4")
dat15$Folder.1 = name[dat15$Folder.1]

fname16 = "P4_4_B.csv"
dat16 = read.delim(fname16, sep=",", stringsAsFactors=FALSE)
unique(dat16$Folder.1)
name = c("P4_4_B")
names(name) = c("P4-4")
dat16$Folder.1 = name[dat16$Folder.1]

fname17 = "P5_1_A.csv"
dat17 = read.delim(fname17, sep=",", stringsAsFactors=FALSE)
unique(dat17$Folder.1)
name = c("P5_1_A")
names(name) = c("P5-1")
dat17$Folder.1 = name[dat17$Folder.1]

fname18 = "P5_1_B.csv"
dat18 = read.delim(fname18, sep=",", stringsAsFactors=FALSE)
unique(dat18$Folder.1)
name = c("P5_1_B")
names(name) = c("P5-1")
dat18$Folder.1 = name[dat18$Folder.1]

fname19 = "P5_4_A.csv"
dat19 = read.delim(fname19, sep=",", stringsAsFactors=FALSE)
unique(dat19$Folder.1)
name = c("P5_4_A")
names(name) = c("P5-4")
dat19$Folder.1 = name[dat19$Folder.1]

fname20 = "P5_4_B.csv"
dat20 = read.delim(fname20, sep=",", stringsAsFactors=FALSE)
unique(dat20$Folder.1)
name = c("P5_4_B")
names(name) = c("P5-4")
dat20$Folder.1 = name[dat20$Folder.1]

merged=rbind(dat0, dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8, dat9, dat10, dat11, dat12, dat13, dat14, dat15, dat16, dat17, dat18, dat19, dat20)
write.csv(merged, "All_merged.csv")


#Start from here
fname30 = "All_merged.csv"
data = read.delim(fname30, sep=",", stringsAsFactors=FALSE)

#Rename ROIs
unique(data$Filename)
ROI = c("01", "02", "03", "04", "05", "06", "07")
names(ROI) = c("ROI001_ROI_001", "ROI002_ROI_002", "ROI003_ROI_003",
               "ROI004_ROI_004", "ROI005_ROI_005", "ROI006_ROI_006", "ROI007_ROI_007")
data = data[data$Filename %in% names(ROI),]
data$Filename = ROI[data$Filename]
unique(data$Filename)


# Rename channels
unique(data$Channel)
chans = c("CYB", "LOG_CYB",
          "SDHA", "LOG_SDHA",
          "TFAM", "LOG_TFAM",
          "NDUFB8", "LOG_NDUFB8",
          "ATPB", "LOG_ATPB",
          "ND4", "LOG_ND4",
          "VDAC", "LOG_VDAC",
          "COX4", "LOG_COX4",
          "MTCO1", "LOG_MTCO1",
          "Dystrophin", "LOG_Dystrophin", 
          "DNA", "LOG_DNA",
          "Mean","Edges","Area","AspectRatio","Perimeter","Circularity","xCoord","yCoord")
names(chans) = c("115In_CYTB", "LOG_115In_CYTB",
                 "153Eu_SDHA", "LOG_153Eu_SDHA",
                 "156Gd_TFAM", "LOG_156Gd_TFAM",
                 "160Gd_NDUFB8", "LOG_160Gd_NDUFB8",
                 "161Dy_ATP8", "LOG_161Dy_ATP8",
                 "164Dy_ND4", "LOG_164Dy_ND4",
                 "166Er_VDAC1", "LOG_166Er_VDAC1",
                 "168Er_COX4_4L2", "LOG_168Er_COX4_4L2",
                 "172Yb_MTCO1", "LOG_172Yb_MTCO1",
                 "176Yb_Dystrophin", "LOG_176Yb_Dystrophin",
                 "191Ir_DNA1", "LOG_191Ir_DNA1",
                 "Mean","Edges","Area","AspectRatio","Perimeter","Circularity","xCoord","yCoord")

# Discard data where channel name not listed above
data = data[data$Channel %in% names(chans),]
data$Channel = chans[data$Channel]
unique(data$Channel)

# Rename the variable of training
unique(data$Folder.1)
new <- data %>%
  select(ID, Filename, Value, Channel, Folder.1)
new = rename(new, Training = Folder.1)
unique(new$Training)

pats = c("Controls","P1_PRE_A","P1_PRE_B","P1_POST_A","P1_POST_B",
         "P2_PRE_A","P2_PRE_B","P2_POST_A","P2_POST_B",
         "P3_PRE_A","P3_PRE_B","P3_POST_A","P3_POST_B",
         "P4_PRE_A","P4_PRE_B","P4_POST_A","P4_POST_B",
         "P5_PRE_A","P5_PRE_B","P5_POST_A","P5_POST_B")
names(pats) = c("Controls","P1_1_A","P1_1_B","P1_4_A","P1_4_B",
                "P2_1_A","P2_1_B","P2_4_A","P2_4_B",
                "P3_1_A","P3_1_B","P3_4_A","P3_4_B",
                "P4_1_A","P4_1_B","P4_4_A","P4_4_B",
                "P5_1_A","P5_1_B","P5_4_A","P5_4_B")
new$patient_id = pats[new$Training]
unique(new$patient_id)

new$ID_ROI <- paste(new$ID, new$Filename, sep = "_")


data <- spread(new, Channel, Value)

###Generate control df
datac = subset(data, patient_id%in%c("Controls"))

#Correlation (open the other script)


#95% PREDICTIVE INTERVAL MODEL
dat <- data %>%
  select("Training", "patient_id", "ID_ROI",
         "VDAC")

write.csv(dat, "VDAC1.csv")

# Axis limits and colours
getRange = function(x){
  vals = log(x) ###Confused where vals came from
  vals = vals[!is.na(vals)&is.finite(vals)]
  return(range(vals))
}

##Conor's code to work on generating plots editing in progress  
mitochan = "VDAC"
mitorange = getRange(dat[,mitochan]) ####struggling with mitorange since I am unsure how to convert for mine

storeDefect = list()

pdf("Report191021",width=10,height=7)
# Do everything in a loop: always avoid copy-pasting code where you can: minimises mistakes and inconsistencies
for(pat in pats){
  datapat = subset(dat, patient_id==pat)
  allchans = gsub("LOG_","",colnames(dat)[grepl("LOG",colnames(dat))])
  chans = allchans
  chanvec = chans[chans%in%colnames(datapat)]
  chanvec = chanvec[chanvec!=mitochan]
  chandefect = rep(-999,length(chanvec))
  names(chandefect) = chanvec
  op = par(mfrow=c(3,3), mar=c(5.5, 4.5, 4.5, 2.1))
  for(chan in chanvec){
    dt = subset(dat, patient_id==pat)
    chanrange = getRange(dt[,chan])
    # Linear regression for controls and predictive interval
    xctrl = log(datac[,mitochan])
    yctrl = log(datac[,chan])
    xpat = log(dt[,mitochan])
    ypat = log(dt[,chan])
    xsyn = seq(mitorange[1],mitorange[2],length.out=50)
    mod = lm(yctrl~xctrl)
    
    pred_syn = predict(mod,newdata = data.frame(xctrl=xsyn), se.fit=TRUE,  interval = "prediction",na.action=na.omit)$fit
    mid_syn = pred_syn[,1]
    up_syn = pred_syn[,3]
    low_syn = pred_syn[,2]
    
    pred = predict(mod,newdata = data.frame(xctrl=xpat), se.fit=TRUE,  interval = "prediction",na.action=na.omit)$fit
    up = pred[,3]
    low = pred[,2]	 
    dt[[paste(chan,"up_pred",sep="_")]]=up
    dt[[paste(chan,"low_pred",sep="_")]]=low
    dt[[paste(chan,"under_exp",sep="_")]] = log(dt[[chan]]) < dt[[paste(chan,"low_pred",sep="_")]]
    dt[[paste(chan,"over_exp",sep="_")]] = log(dt[[chan]]) > dt[[paste(chan,"up_pred",sep="_")]]
    
    ###Paste to dat
    datapat[[paste(chan,"up_pred",sep="_")]]=up
    datapat[[paste(chan,"low_pred",sep="_")]]=low
    datapat[[paste(chan,"under_exp",sep="_")]] = log(dt[[chan]]) < dt[[paste(chan,"low_pred",sep="_")]]
    datapat[[paste(chan,"over_exp",sep="_")]] = log(dt[[chan]]) > dt[[paste(chan,"up_pred",sep="_")]]
    
    #psd = (up - low)/(2*1.96)
    #upz = mid+3*psd
    #lowz = mid-3*psd 
    
    N = length(dt$ID)
    fracUP = sum(dt[[paste(chan,"over_exp",sep="_")]],na.rm=TRUE)/N
    fracDOWN = sum(dt[[paste(chan,"under_exp",sep="_")]],na.rm=TRUE)/N
    chandefect[chan] = fracDOWN
    
    mlab = paste(pat,"below:",signif(100*fracDOWN,3),"% above:",signif(100*fracUP,3),"%")
    
    plot(xpat,ypat,xlab=paste("log(",mitochan,")",sep=""),ylab=paste("log(",chan,")",sep=""),xlim=mitorange,ylim=chanrange,col=rgb(0,0,1,0.2),pch=16,main=mlab,cex.lab=2,cex.axis=1.75)
    points(xsyn,mid_syn,type="l",lty=1,lwd=2,col="black")
    points(xsyn,up_syn,type="l",lty=2,lwd=2,col="black")
    points(xsyn,low_syn,type="l",lty=2,lwd=2,col="black")
    
  }
  storeDefect[[pat]] = chandefect
  
  par(op)
  mtext(paste("N =",N), outer=TRUE,  cex=1, line=-1.3)
  write.csv(datapat, file=paste(pat,"_95PI_sig_data.csv",sep=""),row.names=FALSE)
}
dev.off()

#defectdf = do.call("rbind",storeDefect)
#print(storeDefect)
#write.csv(fracDOWN, "Deficiency.csv")


#INTRA-MUSCLE VARIABILITY? 
fname0 = "P1_PRE_A_95PI_sig_data.csv"
dat0 = read.delim(fname0, sep=",", stringsAsFactors=FALSE)
fname1 = "P1_PRE_B_95PI_sig_data.csv"
dat1 = read.delim(fname1, sep=",", stringsAsFactors=FALSE)
fname2 = "P2_PRE_A_95PI_sig_data.csv"
dat2 = read.delim(fname2, sep=",", stringsAsFactors=FALSE)
fname3 = "P2_PRE_B_95PI_sig_data.csv"
dat3 = read.delim(fname3, sep=",", stringsAsFactors=FALSE)
fname4 = "P3_PRE_A_95PI_sig_data.csv"
dat4 = read.delim(fname4, sep=",", stringsAsFactors=FALSE)
fname5 = "P3_PRE_B_95PI_sig_data.csv"
dat5 = read.delim(fname5, sep=",", stringsAsFactors=FALSE)
fname6 = "P4_PRE_A_95PI_sig_data.csv"
dat6 = read.delim(fname6, sep=",", stringsAsFactors=FALSE)
fname7 = "P4_PRE_B_95PI_sig_data.csv"
dat7 = read.delim(fname7, sep=",", stringsAsFactors=FALSE)
fname8 = "P5_PRE_A_95PI_sig_data.csv"
dat8 = read.delim(fname8, sep=",", stringsAsFactors=FALSE)
fname9 = "P5_PRE_B_95PI_sig_data.csv"
dat9 = read.delim(fname9, sep=",", stringsAsFactors=FALSE)

PRE=rbind(dat0,dat1,dat2,dat3,dat4,dat5,dat6,dat7,dat8,dat9)

PRE$Biopsy = "A"
PRE$Biopsy[grep("_B",PRE$Sample)]="B"
print(unique(PRE$Biopsy))

# Rename the variable of sample
unique(PRE$Sample)
new = c("P01_PRE_A","P01_PRE_B",
         "P02_PRE_A","P02_PRE_B",
         "P03_PRE_A","P03_PRE_B",
         "P04_PRE_A","P04_PRE_B",
         "P05_PRE_A","P05_PRE_B")
names(new) = c("P1_PRE_A","P1_PRE_B",
                "P2_PRE_A","P2_PRE_B",
                "P3_PRE_A","P3_PRE_B",
                "P4_PRE_A","P4_PRE_B",
                "P5_PRE_A","P5_PRE_B")
PRE$Sample = new[PRE$Sample]
unique(PRE$Sample)

MTCO1 <- PRE%>%
  select(Sample, Biopsy, MTCO1_low_pred, MTCO1_under_exp)
write.csv(MTCO1, "MTCO1.csv")

fname0 = "MTCO1.csv"
dat0 = read.delim(fname0, sep=",", stringsAsFactors=FALSE)

MTCO1g <- ggplot(dat0, aes(x=Sample, y=MTCO1_low_pred))+
  geom_boxplot()+
  geom_point(aes(x=Sample, y=MTCO1_low_pred, col=Biopsy, alpha=0.01),
             position=position_jitterdodge(dodge.width=0.6)) +
  scale_color_manual(values=c("blue", "blue")) +
  ggtitle("MTCO1") +
  ylab("Proportion below controls") +
  ylim(0,10) +
  xlab("")+
  theme(axis.text.x = element_text(angle = 90)) +
  geom_vline(xintercept=c(2.5,4.5,6.5,8.5,10.5),linetype='dashed',color='black',size=0.3) +
  theme(panel.background = element_rect(fill="grey100", color="white"))
MTCO1g

#T TEST TWO SIDED BECAUSE I WANT TO SEE WHETHER BIOPSIES A AND B ARE UNIVERSALLY DIFFERENT

#ATPB up
for (fn in sort(unique(PRE$Sample))){
  A = PRE$ATPB_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$ATPB_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}
#ATPB low
for (fn in sort(unique(PRE$Sample))){
  A = PRE$ATPB_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$ATPB_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}

#COX4 up
for (fn in sort(unique(PRE$Sample))){
  A = PRE$COX4_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$COX4_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}
#COX4 low
for (fn in sort(unique(PRE$Sample))){
  A = PRE$COX4_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$COX4_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}

#CYB up
for (fn in sort(unique(PRE$Sample))){
  A = PRE$CYB_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$CYB_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}
#CYB low
for (fn in sort(unique(PRE$Sample))){
  A = PRE$CYB_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$CYB_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}

#MTCO1 up
for (fn in sort(unique(PRE$Sample))){
  A = PRE$MTCO1_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$MTCO1_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}
#MTCO1 low
for (fn in sort(unique(PRE$Sample))){
  A = PRE$MTCO1_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$MTCO1_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}

#ND4 up
for (fn in sort(unique(PRE$Sample))){
  A = PRE$ND4_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$ND4_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}
#ND4 low
for (fn in sort(unique(PRE$Sample))){
  A = PRE$ND4_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$ND4_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}

#NDUFB8 up
for (fn in sort(unique(PRE$Sample))){
  A = PRE$NDUFB8_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$NDUFB8_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}
#NDUFB8 low
for (fn in sort(unique(PRE$Sample))){
  A = PRE$NDUFB8_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$NDUFB8_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}

#SDHA up
for (fn in sort(unique(PRE$Sample))){
  A = PRE$SDHA_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$SDHA_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}
#SDHA low
for (fn in sort(unique(PRE$Sample))){
  A = PRE$SDHA_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$SDHA_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}

#TFAM up
for (fn in sort(unique(PRE$Sample))){
  A = PRE$TFAM_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$TFAM_up_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}
#TFAM low
for (fn in sort(unique(PRE$Sample))){
  A = PRE$TFAM_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="A")]
  B = PRE$TFAM_low_pred[(PRE$Sample==fn)&(PRE$Biopsy=="B")]
  print(fn)
  print(t.test(A,B,
               alternative = c("two.sided", "less", "greater"),
               paired = FALSE, var.equal = FALSE))
}

#Graph for showing summary deficiency
name = "Prop.csv"
data = read.delim(name, sep=",", stringsAsFactors=FALSE)
#P1
P1B <- data%>%
  select(Sample, Biopsy, Variable, Study, Target, Value)%>%
  filter(Sample == "Controls" |
           Sample == "P1_1A" |
           Sample == "P1_1B" |
           Sample == "P1_4A" |
           Sample == "P1_4B" ) %>%
  filter(Variable == "Below")%>%
  mutate(Target = factor(Target, levels=c("ND4","NDUFB8","SDHA","CYB","COX4","MTCO1","ATPB","TFAM","DNA")))
P1g <- ggplot(P1B, aes(x=Target, y=Value, col=Study)) +
  geom_point(aes(x=Target, y=Value, col=Study, shape=Biopsy), position=position_jitterdodge(dodge.width=0.6)) +
  scale_color_manual(values=c("seagreen4", "grey30", "black")) +
  ggtitle("P1 BELOW") +
  ylab("Proportion below controls (%)") +
  ylim(0,100) +
  xlab("")+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5),linetype='dashed',color='black',size=0.3) +
  theme(panel.background = element_rect(fill="grey100", color="white"))
P1g

P1A <- data%>%
  select(Sample, Biopsy, Variable, Study, Target, Value)%>%
  filter(Sample == "Controls" |
           Sample == "P1_1A" |
           Sample == "P1_1B" |
           Sample == "P1_4A" |
           Sample == "P1_4B" ) %>%
  filter(Variable == "Above")%>%
  mutate(Target = factor(Target, levels=c("ND4","NDUFB8","SDHA","CYB","COX4","MTCO1","ATPB","TFAM","DNA")))
P1g <- ggplot(P1A, aes(x=Target, y=Value, col=Study)) +
  geom_point(aes(x=Target, y=Value, col=Study, shape=Biopsy), position=position_jitterdodge(dodge.width=0.6)) +
  scale_color_manual(values=c("seagreen4", "grey30", "black")) +
  ggtitle("P1 ABOVE") +
  ylab("Proportion above controls (%)") +
  ylim(0,100) +
  xlab("")+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5),linetype='dashed',color='black',size=0.3) +
  theme(panel.background = element_rect(fill="grey100", color="white"))
P1g


#P2
P2B <- data%>%
  select(Sample, Biopsy, Variable, Study, Target, Value)%>%
  filter(Sample == "Controls" |
           Sample == "P2_1A" |
           Sample == "P2_1B" |
           Sample == "P2_4A" |
           Sample == "P2_4B" ) %>%
  filter(Variable == "Below")%>%
  mutate(Target = factor(Target, levels=c("ND4","NDUFB8","SDHA","CYB","COX4","MTCO1","ATPB","TFAM","DNA")))
P2g <- ggplot(P2B, aes(x=Target, y=Value, col=Study)) +
  geom_point(aes(x=Target, y=Value, col=Study, shape=Biopsy), position=position_jitterdodge(dodge.width=0.6)) +
  scale_color_manual(values=c("mediumblue", "grey30", "black")) +
  ggtitle("P2 BELOW") +
  ylab("Proportion below controls (%)") +
  ylim(0,100) +
  xlab("")+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5),linetype='dashed',color='black',size=0.3) +
  theme(panel.background = element_rect(fill="grey100", color="white"))
P2g

P2A <- data%>%
  select(Sample, Biopsy, Variable, Study, Target, Value)%>%
  filter(Sample == "Controls" |
           Sample == "P2_1A" |
           Sample == "P2_1B" |
           Sample == "P2_4A" |
           Sample == "P2_4B" ) %>%
  filter(Variable == "Above")%>%
  mutate(Target = factor(Target, levels=c("ND4","NDUFB8","SDHA","CYB","COX4","MTCO1","ATPB","TFAM","DNA")))
P2g <- ggplot(P2A, aes(x=Target, y=Value, col=Study)) +
  geom_point(aes(x=Target, y=Value, col=Study, shape=Biopsy), position=position_jitterdodge(dodge.width=0.6)) +
  scale_color_manual(values=c("mediumblue", "grey30", "black")) +
  ggtitle("P2 ABOVE") +
  ylab("Proportion above controls (%)") +
  ylim(0,100) +
  xlab("")+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5),linetype='dashed',color='black',size=0.3) +
  theme(panel.background = element_rect(fill="grey100", color="white"))
P2g

#P3
P3B <- data%>%
  select(Sample, Biopsy, Variable, Study, Target, Value)%>%
  filter(Sample == "Controls" |
           Sample == "P3_1A" |
           Sample == "P3_1B" |
           Sample == "P3_4A" |
           Sample == "P3_4B" ) %>%
  filter(Variable == "Below")%>%
  mutate(Target = factor(Target, levels=c("ND4","NDUFB8","SDHA","CYB","COX4","MTCO1","ATPB","TFAM","DNA")))
P3g <- ggplot(P3B, aes(x=Target, y=Value, col=Study)) +
  geom_point(aes(x=Target, y=Value, col=Study, shape=Biopsy), position=position_jitterdodge(dodge.width=0.6)) +
  scale_color_manual(values=c("deeppink2", "grey30", "black")) +
  ggtitle("P3 BELOW") +
  ylab("Proportion below controls (%)") +
  ylim(0,100) +
  xlab("")+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5),linetype='dashed',color='black',size=0.3) +
  theme(panel.background = element_rect(fill="grey100", color="white"))
P3g

P3A <- data%>%
  select(Sample, Biopsy, Variable, Study, Target, Value)%>%
  filter(Sample == "Controls" |
           Sample == "P3_1A" |
           Sample == "P3_1B" |
           Sample == "P3_4A" |
           Sample == "P3_4B" ) %>%
  filter(Variable == "Above") %>%
  mutate(Target = factor(Target, levels=c("ND4","NDUFB8","SDHA","CYB","COX4","MTCO1","ATPB","TFAM","DNA")))
P3g <- ggplot(P3A, aes(x=Target, y=Value, col=Study)) +
  geom_point(aes(x=Target, y=Value, col=Study, shape=Biopsy), position=position_jitterdodge(dodge.width=0.6)) +
  scale_color_manual(values=c("deeppink2", "grey30", "black")) +
  ggtitle("P3 ABOVE") +
  ylab("Proportion above controls (%)") +
  ylim(0,100) +
  xlab("")+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5),linetype='dashed',color='black',size=0.3) +
  theme(panel.background = element_rect(fill="grey100", color="white"))
P3g

#P4
P4B <- data%>%
  select(Sample, Biopsy, Variable, Study, Target, Value)%>%
  filter(Sample == "Controls" |
           Sample == "P4_1A" |
           Sample == "P4_1B" |
           Sample == "P4_4A" |
           Sample == "P4_4B" ) %>%
  filter(Variable == "Below")%>%
  mutate(Target = factor(Target, levels=c("ND4","NDUFB8","SDHA","CYB","COX4","MTCO1","ATPB","TFAM","DNA")))
P4g <- ggplot(P4B, aes(x=Target, y=Value, col=Study)) +
  geom_point(aes(x=Target, y=Value, col=Study, shape=Biopsy), position=position_jitterdodge(dodge.width=0.6)) +
  scale_color_manual(values=c("#e57d57", "grey30", "black")) +
  ggtitle("P4 BELOW") +
  ylab("Proportion below controls (%)") +
  ylim(0,100) +
  xlab("")+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5),linetype='dashed',color='black',size=0.3) +
  theme(panel.background = element_rect(fill="grey100", color="white"))
P4g

P4A <- data%>%
  select(Sample, Biopsy, Variable, Study, Target, Value)%>%
  filter(Sample == "Controls" |
           Sample == "P4_1A" |
           Sample == "P4_1B" |
           Sample == "P4_4A" |
           Sample == "P4_4B" ) %>%
  filter(Variable == "Above") %>%
  mutate(Target = factor(Target, levels=c("ND4","NDUFB8","SDHA","CYB","COX4","MTCO1","ATPB","TFAM","DNA")))
P4g <- ggplot(P4A, aes(x=Target, y=Value, col=Study)) +
  geom_point(aes(x=Target, y=Value, col=Study, shape=Biopsy), position=position_jitterdodge(dodge.width=0.6)) +
  scale_color_manual(values=c("#e57d57", "grey30", "black")) +
  ggtitle("P4 ABOVE") +
  ylab("Proportion above controls (%)") +
  ylim(0,100) +
  xlab("")+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5),linetype='dashed',color='black',size=0.3) +
  theme(panel.background = element_rect(fill="grey100", color="white"))
P4g

#P5
P5B <- data%>%
  select(Sample, Biopsy, Variable, Study, Target, Value)%>%
  filter(Sample == "Controls" |
           Sample == "P5_1A" |
           Sample == "P5_1B" |
           Sample == "P5_4A" |
           Sample == "P5_4B" ) %>%
  filter(Variable == "Below")%>%
  mutate(Target = factor(Target, levels=c("ND4","NDUFB8","SDHA","CYB","COX4","MTCO1","ATPB","TFAM","DNA")))
P5g <- ggplot(P5B, aes(x=Target, y=Value, col=Study)) +
  geom_point(aes(x=Target, y=Value, col=Study, shape=Biopsy), position=position_jitterdodge(dodge.width=0.6)) +
  scale_color_manual(values=c("goldenrod", "grey30", "black")) +
  ggtitle("P5 BELOW") +
  ylab("Proportion below controls (%)") +
  ylim(0,100) +
  xlab("")+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5),linetype='dashed',color='black',size=0.3) +
  theme(panel.background = element_rect(fill="grey100", color="white"))
P5g

P5A <- data%>%
  select(Sample, Biopsy, Variable, Study, Target, Value)%>%
  filter(Sample == "Controls" |
           Sample == "P5_1A" |
           Sample == "P5_1B" |
           Sample == "P5_4A" |
           Sample == "P5_4B" ) %>%
  filter(Variable == "Above") %>%
  mutate(Target = factor(Target, levels=c("ND4","NDUFB8","SDHA","CYB","COX4","MTCO1","ATPB","TFAM","DNA")))
P5g <- ggplot(P5A, aes(x=Target, y=Value, col=Study)) +
  geom_point(aes(x=Target, y=Value, col=Study, shape=Biopsy), position=position_jitterdodge(dodge.width=0.6)) +
  scale_color_manual(values=c("goldenrod", "grey30", "black")) +
  ggtitle("P5 ABOVE") +
  ylab("Proportion above controls (%)") +
  ylim(0,100) +
  xlab("")+
  geom_vline(xintercept=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5),linetype='dashed',color='black',size=0.3) +
  theme(panel.background = element_rect(fill="grey100", color="white"))
P5g
