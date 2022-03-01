# First let's read raw data into memory
fname = "Merged_file.csv"
dat = read.delim(fname, sep=",", stringsAsFactors=FALSE)

# Let's have a look at filenames so we can see who needs npc
unique(dat$Filename)

# Make some NPCs for each PA Filename
npcFilename = "P6_A NPC" 
for(root in c("P6_A1","P6_A2","P6_A3")){
  npcNew = dat[dat$Filename==npcFilename,] # Get correct npc data
  npcNew$Filename = paste(root,"NPC") # Change the Filename entry for these data to correct label
  dat = rbind(dat,npcNew) # Add new npc data to dataframe
}
dat = dat[dat$Filename!= npcFilename,] # Finally delete old copy of npc

# Make some NPCs for each PB Filename
npcFilename = "P6_B NPC" 
for(root in c("P6_B1","P6_B2","P6_B3")){
  npcNew = dat[dat$Filename==npcFilename,] # Get correct npc data
  npcNew$Filename = paste(root,"NPC") # Change the Filename entry for these data to correct label
  dat = rbind(dat,npcNew) # Add new npc data to dataframe
}
dat = dat[dat$Filename!= npcFilename,] # Finally delete old copy of npc

# Make some NPCs for each PC_1 Filename
npcFilename = "P6_C_1 NPC"
for(root in c("P6_C1_1","P6_C2_1","P6_C3_1","P6_C4_1")){
  npcNew = dat[dat$Filename==npcFilename,] # Get correct npc data
  npcNew$Filename = paste(root,"NPC") # Change the Filename entry for these data to correct label
  dat = rbind(dat,npcNew) # Add new npc data to dataframe
}
dat = dat[dat$Filename!= npcFilename,] # Finally delete old copy of npc

# Make some NPCs for each PC_2 Filename
npcFilename = "P6_C_2 NPC"
for(root in c("P6_C1_2","P6_C2_2","P6_C3_2")){
  npcNew = dat[dat$Filename==npcFilename,] # Get correct npc data
  npcNew$Filename = paste(root,"NPC") # Change the Filename entry for these data to correct label
  dat = rbind(dat,npcNew) # Add new npc data to dataframe
}
dat = dat[dat$Filename!= npcFilename,] # Finally delete old copy of npc

# Make some NPCs for each PC_2 Filename
npcFilename = "P6_D_1 NPC"
for(root in c("P6_D1_1","P6_D2_1","P6_D3_1")){
  npcNew = dat[dat$Filename==npcFilename,] # Get correct npc data
  npcNew$Filename = paste(root,"NPC") # Change the Filename entry for these data to correct label
  dat = rbind(dat,npcNew) # Add new npc data to dataframe
}
dat = dat[dat$Filename!= npcFilename,] # Finally delete old copy of npc

# Make some NPCs for each PC_2 Filename
npcFilename = "P6_D_2 NPC"
for(root in c("P6_D1_2","P6_D2_2","P6_D3_2")){
  npcNew = dat[dat$Filename==npcFilename,] # Get correct npc data
  npcNew$Filename = paste(root,"NPC") # Change the Filename entry for these data to correct label
  dat = rbind(dat,npcNew) # Add new npc data to dataframe
}
dat = dat[dat$Filename!= npcFilename,] # Finally delete old copy of npc

head(dat)
print(unique(dat$Filename))

# We need to know what the channels represent
print(unique(dat$Channel))
chans = c("Laminin","MTCO1","VDAC1","NDUFB8")
names(chans) = c("Ch1","Ch2","Ch3","Ch4")
dat$Chan = chans[dat$Channel]
mitochan = "VDAC1"

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

npc_correct = function(oxphos,npc,correct,mitochan,qcutoff=0.5){
  print(paste("Correcting",paste(correct,collapse=" "),"signal only."))
  mchan_cutoff = quantile(npc$Value[npc$Chan==mitochan],qcutoff)[[1]]
  Nfibres = length(unique(oxphos$ID))
  todrop = unique(oxphos$ID[((oxphos$Chan==mitochan)&(oxphos$Value<=mchan_cutoff))])
  dropfrac = length(todrop)/Nfibres
  print(paste("Before NPC:",Nfibres,"fibres..."))
  print(paste("Dropping",round(dropfrac,4)))
  oxphos = oxphos[!oxphos$ID%in%todrop,]
  if(length(npc$Value)>0){
    agg = aggregate(npc$Value,list(npc$Chan),mean)
    adj = agg[,2]
    names(adj) = agg[,1]
    oxphos$Corrected = oxphos$Value - adj[oxphos$Chan]
    for(ch in names(adj)){
      if(!ch%in%correct){
        oxphos$Corrected[oxphos$Chan==ch] = oxphos$Value[oxphos$Chan==ch]
      }
    }
    todrop2 = oxphos$ID[oxphos$Corrected<=0]
    oxphos = oxphos[!oxphos$ID%in%todrop2,]
    
  }else{
    print(paste("WARNING!  No NPC for",id))
    oxphos$Corrected = oxphos$Value
  }
}

ox = list()
wides = list()

for(id in unique(dat$SectionID)){
  oxphos = dat[(dat$SectionID==id)&(dat$Type=="OXPHOS"),]
  npc = dat[(dat$SectionID==id)&(dat$Type=="NPC"),]
  oxphos = npc_correct(oxphos,npc,c("MTCO1","NDUFB8","VDAC1"),mitochan,0.95)
  ox[[id]] = oxphos
  
  # Convert corrected data from long to wide format
  dt = reshape(oxphos[,c("Corrected","Chan","ID")], idvar = "ID", timevar = "Chan", direction="wide")
  colnames(dt) = gsub("Corrected.","",colnames(dt))
  wides[[id]] = dt  
}

# Stick everything back together after correction
datnew = do.call("rbind",ox)
write.csv(datnew, "Data_041021.csv")

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


ID=c()
CHAN=c()
UP=c()
DOWN=c()
# Save some plots to a .pdf report
pdf("Report_100121.pdf",width=8,height=5)
for(id in sort(unique(dat$SectionID))){
  dt = wides[[id]]
  #dt = dt[dt[[mitochan]]>0,] # Drop all fibres with weird mitochan values
  if(dim(dt)[1] > 0){
    op = par(mfrow=c(1,2))
    # Do everything in a loop: always avoid copy-pasting code where you can: minimises mistakes and inconsistencies
    for(ch in c("NDUFB8","MTCO1")){
      chanrange = getRange(datnew$Corrected[datnew$Chan==ch])
      # Linear regression for controls and predictive interval
      controls = do.call("rbind",wides[c("CA","CB","CC","CD")])
      xctrl = log(controls[,mitochan])
      yctrl = log(controls[,ch])
      xsyn = seq(mitorange[1],mitorange[2],length.out=50)
      mod = lm(yctrl~xctrl)
      #print(paste(id,ch))
      
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
      
      mlab = paste(id,paste("below:",signif(100*fracDOWN,3),"% above:",signif(100*fracUP,3),"%"),sep="\n")
      
      plot(log(dt[[mitochan]]),log(dt[[ch]]),xlab=paste("log(",mitochan,")",sep=""),ylab=paste("log(",ch,")",sep=""),xlim=mitorange,ylim=chanrange,col=(vcols[ch]),pch=16,main=mlab)
      points(xsyn,mid_syn,type="l",lty=1,lwd=2,col="black")
      points(xsyn,up_syn,type="l",lty=2,lwd=2,col="black")
      points(xsyn,low_syn,type="l",lty=2,lwd=2,col="black")
      ID=c(ID,id)
      CHAN=c(CHAN,ch)
      UP=c(UP,fracUP)
      DOWN=c(DOWN,fracDOWN)
    }
    mtext(paste("N =",N), outer=TRUE,  cex=1.3, line=-1.3, adj=0)
    par(op)
    
  }
}
dev.off()



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

