library(dplyr)
library(cowplot)

# First let's read raw data into memory
fname = "Merged file.csv"
dat = read.delim(fname, sep=",", stringsAsFactors=FALSE)

# Let's have a look at filenames so we can see who needs npc
head(dat)
print(unique(dat$Filename))
print(unique(dat$Channel))

# We need to know what the channels represent
print(unique(dat$Channel))
chans = c("MTCO1","Laminin","NDUFB8","VDAC1","Area")
names(chans) = c("Ch1","Ch2","Ch3","Ch4","Area")
dat$Chan = chans[dat$Channel]
mitochan = "VDAC1"

#Let's drop fibres with values 0
dat <- dat %>%
  select(Value, ID, Channel, Filename, Chan)%>%
  filter(Value != 0)

#Rename samples 
samples= c("1A_CTRL NPC", "1A_CTRL OXPHOS", "1B_CTRL NPC", "1B_CTRL OXPHOS", "1C_CTRL NPC", "1C_CTRL OXPHOS", 
"P1_PRE NPC", "P1_PRE OXPHOS", "P1_POST NPC", "P1_POST OXPHOS",
"P2_PRE NPC", "P2_PRE OXPHOS", "P2_POST NPC", "P2_POST OXPHOS", 
"P3_PRE NPC", "P3_PRE OXPHOS", "P3_POST NPC", "P3_POST OXPHOS",
"P4_PRE NPC", "P4_PRE OXPHOS", "P4_POST NPC", "P4_POST OXPHOS", 
"P5_PRE NPC", "P5_PRE OXPHOS", "P5_POST NPC", "P5_POST OXPHOS", 
"P6_PRE NPC", "P6_PRE OXPHOS", "P6_POST NPC", "P6_POST OXPHOS", 
"P7_PRE NPC","P7_PRE OXPHOS", "P7_POST NPC", "P7_POST OXPHOS", 
"P8_PRE NPC", "P8_PRE OXPHOS", "P8_POST NPC", "P8_POST OXPHOS", 
"P9_POST NPC", "P9_POST OXPHOS", 
"P10_PRE NPC", "P10_PRE OXPHOS", "P10_POST NPC", "P10_POST OXPHOS",
"P11_PRE NPC", "P11_PRE OXPHOS", "P11_POST NPC", "P11_POST OXPHOS")
names(samples) = c("1A NPC", "1A OXPHOS", "1B NPC", "1B OXPHOS", "1C NPC", "1C OXPHOS",
                   "P1 PRE NPC", "P1 PRE OXPHOS", "P1 POST NPC", "P1 POST OXPHOS",
                   "P2 PRE NPC", "P2 PRE OXPHOS", "P2 POST NPC", "P2 POST OXPHOS", 
                   "P3 PRE NPC", "P3 PRE OXPHOS", "P3 POST NPC", "P3 POST OXPHOS",
                   "P4 PRE NPC", "P4 PRE OXPHOS", "P4 POST NPC", "P4 POST OXPHOS", 
                   "P5 PRE NPC", "P5 PRE OXPHOS", "P5 POST NPC", "P5 POST OXPHOS", 
                   "P6 PRE NPC", "P6 PRE OXPHOS", "P6 POST NPC", "P6 POST OXPHOS", 
                   "P7 PRE NPC", "P7 PRE OXPHOS", "P7 POST NPC", "P7 POST OXPHOS", 
                   "P8 PRE NPC", "P8 PRE OXPHOS", "P8 POST NPC", "P8 POST OXPHOS", 
                   "P9 POST NPC", "P9 POST OXPHOS", 
                   "P10 PRE NPC", "P10 PRE OXPHOS", "P10 POST NPC", "P10 POST OXPHOS",
                   "P11 PRE NPC", "P11 PRE OXPHOS", "P11 POST NPC", "P11 POST OXPHOS")
dat$Filename = samples[dat$Filename]
print(unique(dat$Filename))

# Extract information from the filenames
# Difficult in this case as different system used for different sections
dat$Type = "OXPHOS"
dat$Type[grepl("NPC",dat$Filename)] = "NPC"

# This function partially extracts info.  Needs improvement.
parsefname = function(str){
  # str = "PD3_2 OXPHOS"
  outstr = strsplit(str," ")[[1]][1]
  return(outstr)
}

dat$SectionID = sapply(dat$Filename,parsefname)

ox = list()
wides = list()


for(id in unique(dat$SectionID)){
  oxphos = dat[(dat$SectionID==id)&(dat$Type=="OXPHOS"),]
  npc = dat[(dat$SectionID==id)&(dat$Type=="NPC"),]
  mchan_cutoff = quantile(npc$Value[npc$Chan==mitochan],0.95)[[1]]
  todrop = oxphos$ID[((oxphos$Chan==mitochan)&(oxphos$Value<=mchan_cutoff))]
  oxphos = oxphos[!oxphos$ID%in%todrop,]
  if(length(npc$Value)>0){
    agg = aggregate(npc$Value,list(npc$Chan),mean)
    adj = agg[,2]
    names(adj) = agg[,1]
    oxphos$Corrected = oxphos$Value - adj[oxphos$Chan]
    oxphos$Corrected[oxphos$Chan=="Laminin"] = oxphos$Value[oxphos$Chan=="Laminin"]
    todrop2 = oxphos$ID[oxphos$Corrected<=0]
    oxphos = oxphos[!oxphos$ID%in%todrop2,]
  }else{
    print(paste("WARNING!  No NPC for",id))
    oxphos$Corrected = oxphos$Value
  }
  ox[[id]] = oxphos
  
  # Convert corrected data from long to wide format
  dt = reshape(oxphos[,c("Corrected","Chan","ID")], idvar = "ID", timevar = "Chan", direction="wide")
  colnames(dt) = gsub("Corrected.","",colnames(dt))
  wides[[id]] = dt  
}

# Stick everything back together after correction
datnew = do.call("rbind",ox)
write.csv(datnew, "Data_reshaped.csv")

# Axis limits and colours
# Axis limits and colours
getRange = function(x){
  vals = log(x)
  vals = vals[!is.na(vals)&is.finite(vals)]
  return(range(vals))
}

#getRange = function(x, transform = log, quants = c(0.0,1.0)){
#vals = transform(x)
#vals = vals[!is.na(vals)&is.finite(vals)] 
#return(as.numeric(quantile(vals,probs=quants)))
#}

# There is something wrong here as I get the message that some Nans are produced in log(x) 
mitorange = getRange(datnew$Corrected[datnew$Chan==mitochan])
#vcols = c("#9c3a5d", "#355d5f")
vcols = c(rgb(0.25,0,0.3,0.4),rgb(0,0.5,0.3,0.4))
names(vcols) = c("NDUFB8","MTCO1")

# Save some plots to a .pdf report
pdf("Report.pdf",width=7,height=5)
for(id in unique(dat$SectionID)){
  dt = wides[[id]]
  #dt = dt[dt[[mitochan]]>0,] # Drop all fibres with weird mitochan values
  if(dim(dt)[1] > 0){
    op = par(mfrow=c(1,2))
    # Do everything in a loop: always avoid copy-pasting code where you can: minimises mistakes and inconsistencies
    for(ch in c("NDUFB8","MTCO1")){
      chanrange = getRange(datnew$Corrected[datnew$Chan==ch])
      # Linear regression for controls and predictive interval
      controls = do.call("rbind",wides[c("1A_CTRL","1B_CTRL","1C_CTRL")])
      xctrl = log(controls[,mitochan])
      yctrl = log(controls[,ch])
      xsyn = seq(mitorange[1],mitorange[2],length.out=50)
      mod = lm(yctrl~xctrl)
      
      pred_syn = predict(mod,newdata = data.frame(xctrl=xsyn), se.fit=TRUE,  interval = "prediction",na.action=na.omit)$fit
      mid_syn = pred_syn[,1]
      up_syn = pred_syn[,3]
      low_syn = pred_syn[,2]
      
      pred = predict(mod, newdata=data.frame(xctrl=log(dt[[mitochan]])), se.fit=TRUE,  interval = "prediction",na.action=na.omit)$fit
      up = pred[,3]
      low = pred[,2]	 
      dt[[paste(ch,"up_pred",sep="_")]]=up
      dt[[paste(ch,"low_pred",sep="_")]]=low
      dt[[paste(ch,"under_exp",sep="_")]] = log(dt[[ch]]) < dt[[paste(ch,"low_pred",sep="_")]]
      dt[[paste(ch,"over_exp",sep="_")]] = log(dt[[ch]]) > dt[[paste(ch,"up_pred",sep="_")]]
      
      #psd = (up - low)/(2*1.96)
      #upz = mid+3*psd
      #lowz = mid-3*psd 
      
      N = length(dt$ID)
      fracUP = sum(dt[[paste(ch,"over_exp",sep="_")]],na.rm=TRUE)/N
      fracDOWN = sum(dt[[paste(ch,"under_exp",sep="_")]],na.rm=TRUE)/N
      
      mlab = paste(id,"below:",signif(100*fracDOWN,3),"% above:",signif(100*fracUP,3),"%")
      
      plot(log(dt[[mitochan]]),log(dt[[ch]]),xlab=paste("log(",mitochan,")",sep=""),ylab=paste("log(",ch,")",sep=""),xlim=mitorange,ylim=chanrange,col=(vcols[ch]),pch=16,main=mlab)
      points(xsyn,mid_syn,type="l",lty=1,lwd=2,col="black")
      points(xsyn,up_syn,type="l",lty=2,lwd=2,col="black")
      points(xsyn,low_syn,type="l",lty=2,lwd=2,col="black")
    }
    mtext(paste("N =",N), outer=TRUE,  cex=1.3, line=-1.3, adj=0)
    par(op)
    
  }
}
dev.off()