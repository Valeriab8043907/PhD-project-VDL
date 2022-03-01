#New script for permutation test

# First let's read raw data into memory
fname = "Merged_exercise.csv"
dat = read.delim(fname, sep=",", stringsAsFactors=FALSE)

# Let's have a look at filenames so we can see who needs npc
head(dat)
print(unique(dat$Filename))
print(unique(dat$Channel))

# We need to know what the channels represent
print(unique(dat$Channel))
chans = c("Laminin","MTCO1","VDAC1","NDUFB8")
names(chans) = c("Ch1","Ch2","Ch3","Ch4")
dat$Chan = chans[dat$Channel]
mitochan = "VDAC1"


#Drop all data for fibres with any value = 0
dat$fib_id = paste(dat$Filename,dat$ID,sep="_")
todrop = sort(unique(dat$fib_id[dat$Value==0]))
dat = dat[!dat$fib_id%in%todrop,]


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

dat$Treatment = "PRE"
dat$Treatment[grep("POST",dat$SectionID)]="POST"
dat$PatientID = substr(dat$SectionID,1,3)

print(unique(dat$Treatment))
print(unique(dat$PatientID))

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
write.csv(datnew, "Data_reshaped101021.csv")

# Axis limits and colours
getRange = function(x){
  vals = log(x)
  vals = vals[!is.na(vals)&is.finite(vals)]
  return(range(vals))
}

# There is something wrong here as I get the message that some Nans are produced in log(x) 
mitorange = getRange(datnew$Corrected[datnew$Chan==mitochan])
#vcols = c("#9c3a5d", "#355d5f")
vcols = c(rgb(0.25,0,0.3,0.4),rgb(0,0.5,0.3,0.4))
names(vcols) = c("NDUFB8","MTCO1")

# Save some plots to a .pdf report and store summaries
ID=c()
CHAN=c()
UP=c()
DOWN=c()
pdf("Report101021B.pdf",width=8,height=5)
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

#SUMMARY SCRIPT
summ = data.frame(ID=ID,CHAN=CHAN,UP=UP,DOWN=DOWN)
levls = c("CA","CB","CC","CD")
for(i in 1:5){
  levls = c(levls,sprintf("P%2d_PRE",i))
  levls = c(levls,sprintf("P%2d_POST",i))
}

png("Summary_Script.png",width=1400,height=1400,pointsize=20)
op = par(mfrow=c(2,2),mai=c(1.8,1.3,1.2,0.05))
for(ch in c("NDUFB8","MTCO1")){
  ch_summ = summ[summ$CHAN==ch,]
  ch_summ$ID = factor(ch_summ$ID,level=levls)
  stripchart(ch_summ$UP~ch_summ$ID,vertical=TRUE,las=2,ylab="Proportion above controls",main=ch,cex.axis=1.2,cex.lab=1.5,pch=16,cex=2,col="blue",ylim=c(0,max(summ$UP)))
  abline(v=c(seq(4.5,14,2)),lty=2)
  stripchart(ch_summ$DOWN~ch_summ$ID,vertical=TRUE,las=2,ylab="Proportion below controls",main=ch,cex.axis=1.2,cex.lab=1.5,pch=16,cex=2,col="blue",ylim=c(0,max(summ$DOWN)))
  abline(v=c(seq(4.5,14,2)),lty=2)
}
par(op)
dev.off()

#PERMUTATION TEST 
getRange = function(x){
  vals = log(x)
  vals = vals[!is.na(vals)&is.finite(vals)]
  return(range(vals))
}

makeWide=function(dt){
  dt = reshape(dt[,c("Corrected","Chan","ID")], idvar = "ID", timevar = "Chan", direction="wide")
  colnames(dt) = gsub("Corrected.","",colnames(dt))
  return(dt)
}

calcDiff = function(comb,ctdf,atype="normal",makeplot=FALSE,mitochan="VDAC1",id=""){
  if(atype=="normal"){
    # Do nothing
  }else if(atype=="permute"){
    comb$Treatment = sample(comb$Treatment,replace=FALSE)
  }else if(atype=="bootstrap"){
    ctdf = ctdf[sample(seq(1:length(ctdf$ID))),]
    pre = comb[comb$Treatment=="PRE",]
    post = comb[comb$Treatment=="POST",]
    pre = pre[sample(seq(1:length(pre$Treatment)),replace=TRUE),]
    post = post[sample(seq(1:length(post$Treatment)),replace=TRUE),]
    comb=rbind(pre,post)
  }else{
    print("Invalid atype specified")
    return(NULL)
  }
  
  LABELS=c()
  CHAN=c()
  UP=c()
  DOWN=c() 
  
  pchans=c("NDUFB8","MTCO1")
  for(trt in c("PRE","POST")){
    dt = comb[comb$Treatment==trt,]
    for(ch in pchans){
      # Linear regression for controls and predictive interval
      controls = ctdf
      xctrl = log(controls[,mitochan])
      yctrl = log(controls[,ch])
      mod = lm(yctrl~xctrl)
      
      pred = predict(mod, newdata=data.frame(xctrl=log(dt[[mitochan]])), se.fit=TRUE,  interval = "prediction",na.action=na.omit)$fit
      up = pred[,3]
      low = pred[,2]	 
      dt[[paste(ch,"up_pred",sep="_")]]=up
      dt[[paste(ch,"low_pred",sep="_")]]=low
      dt[[paste(ch,"under_exp",sep="_")]] = log(dt[[ch]]) < dt[[paste(ch,"low_pred",sep="_")]]
      dt[[paste(ch,"over_exp",sep="_")]] = log(dt[[ch]]) > dt[[paste(ch,"up_pred",sep="_")]]
      
      N = length(dt$ID)
      fracUP = sum(dt[[paste(ch,"over_exp",sep="_")]],na.rm=TRUE)/N
      fracDOWN = sum(dt[[paste(ch,"under_exp",sep="_")]],na.rm=TRUE)/N
      
      LABELS=c(LABELS,trt)
      CHAN=c(CHAN,ch)
      UP=c(UP,fracUP)
      DOWN=c(DOWN,fracDOWN)
      
      if(makeplot){
        #print(paste(id,trt,ch))
        mitorange = getRange(c(dt[,mitochan],ctdf[,mitochan]))
        chanrange = getRange(c(dt[,ch],ctdf[,ch]))
        
        xsyn = seq(mitorange[1],mitorange[2],length.out=50)
        mod = lm(yctrl~xctrl)
        
        pred_syn = predict(mod,newdata = data.frame(xctrl=xsyn), se.fit=TRUE,  interval = "prediction",na.action=na.omit)$fit
        mid_syn = pred_syn[,1]
        up_syn = pred_syn[,3]
        low_syn = pred_syn[,2]
        mlab = paste(paste(id,trt),paste("below:",signif(100*fracDOWN,3),"% above:",signif(100*fracUP,3),"%"),sep="\n")
        
        plot(log(dt[[mitochan]]),log(dt[[ch]]),xlab=paste("log(",mitochan,")",sep=""),ylab=paste("log(",ch,")",sep=""),xlim=mitorange,ylim=chanrange,col=rgb(1,0,0,0.1),pch=16,main=mlab)
        points(xsyn,mid_syn,type="l",lty=1,lwd=2,col="black")
        points(xsyn,up_syn,type="l",lty=2,lwd=2,col="black")
        points(xsyn,low_syn,type="l",lty=2,lwd=2,col="black")
        
        
      }
      
    }
  }
  res = data.frame(LABELS=LABELS,CHAN=CHAN,UP=UP,DOWN=DOWN,stringsAsFactors=FALSE)
  for(ch in pchans){
    deltaUP = res$UP[(res$CHAN==ch)&(res$LABELS=="POST")]-res$UP[(res$CHAN==ch)&(res$LABELS=="PRE")]
    deltaDOWN = res$DOWN[(res$CHAN==ch)&(res$LABELS=="POST")]-res$DOWN[(res$CHAN==ch)&(res$LABELS=="PRE")]
    res = rbind(res,list("DELTA",ch,deltaUP,deltaDOWN))
  }
  baselabs = paste(res$LABELS,res$CHAN,sep="_")
  labs=c(paste(baselabs,"UP",sep="_"),paste(baselabs,"DOWN",sep="_"))
  fracs = c(res$UP,res$DOWN)
  names(fracs)=labs
  return(fracs)
}


# Let's go!

pats=c()
for(pat in sort(unique(dat$PatientID))){
  if(grepl("P",pat)){
    if((paste0(pat,"_PRE")%in%dat$SectionID)&(paste0(pat,"_POST")%in%dat$SectionID)){
      pats=c(pats,pat)
    }
  }
}

ctrls = sort(unique(dat$PatientID[grepl("C",dat$PatientID)]))

ctwides = list()

for(ct in ctrls){
  dt = dat[dat$PatientID==ct,]
  dt = npc_correct(dt[dt$Type=="OXPHOS",],dt[dt$Type=="NPC",],c("MTCO1","NDUFB8","VDAC1"),mitochan,0.95)
  dt = makeWide(dt)
  dt$ID = paste(ct,dt$ID,sep="_")
  ctwides[[ct]] = dt 
}

ctdf = do.call("rbind",ctwides)

outputs = list()
Nsamps = 5000
pdf("SampReport.pdf")
for(pat in pats){
  dt = dat[dat$PatientID==pat,]
  pre_all = dt[dt$Treatment=="PRE",]
  post_all = dt[dt$Treatment=="POST",]
  
  # NPC correction for pre-treatment
  pre = makeWide(npc_correct(pre_all[pre_all$Type=="OXPHOS",],pre_all[pre_all$Type=="NPC",],c("MTCO1","NDUFB8","VDAC1"),mitochan,0.95))
  pre$Treatment = "PRE"
  # NPC correction for post-treatment
  post = makeWide(npc_correct(post_all[post_all$Type=="OXPHOS",],post_all[post_all$Type=="NPC",],c("MTCO1","NDUFB8","VDAC1"),mitochan,0.95))
  post$Treatment = "POST"
  comb = rbind(pre,post)
  res = calcDiff(comb,ctdf,atype="normal",id=pat,makeplot=TRUE)
  resb = replicate(Nsamps,calcDiff(comb,ctdf,atype="bootstrap"))
  resp = replicate(Nsamps,calcDiff(comb,ctdf,atype="permute"))
  output = list()
  for(test in c("DELTA_NDUFB8_UP","DELTA_NDUFB8_DOWN","DELTA_MTCO1_UP","DELTA_MTCO1_DOWN")){
    # p = sum(abs(res[[test]])<=abs(resp[test,]))/Nsamps # 2-tailed test
    if(grepl("_UP",test)){
      p = sum(res[[test]]<=resp[test,])/Nsamps # Probability delta from H0 at least as big as actual result
    }else{
      p = sum(res[[test]]>=resp[test,])/Nsamps # Probability delta from H0 at least as small as actual result
    }
    output[[paste("p",test,sep="_")]] = p
  }
  output[["res"]]=res
  output[["resb"]]=resb
  output[["resp"]]=resp
  
  outputs[[pat]]=output
}
dev.off()

# Post-hoc correction for multiple tests
pvals = c()
pnames = c()
pcheck = c("DELTA_NDUFB8_UP","DELTA_NDUFB8_DOWN","DELTA_MTCO1_UP","DELTA_MTCO1_DOWN")
for(pat in pats){
  for(plab in paste("p",pcheck,sep="_")){
    newlab = paste(pat,plab,sep="_")
    pvals = c(pvals,outputs[[pat]][[plab]])
    pnames = c(pnames,newlab)
  }
}
qvals = p.adjust(pvals,method="fdr")
names(qvals) = pnames
names(pvals) = pnames

png("SummaryBoot.png",width=1200,height=1200,pointsize=16)
op = par(mfrow=c(2,2),mai=c(1.5,1.2,0.5,0.05),mgp=c(4,1,0))
for(chan in c("NDUFB8","MTCO1")){
  for(direction in c("UP","DOWN")){
    pres = list()
    best=list()
    for(pat in pats){
      filt = grepl(paste(chan,direction,sep="_"),rownames(outputs[[pat]]$resb))&!grepl("DELTA",rownames(outputs[[pat]]$resb))
      rsm = outputs[[pat]]$resb[filt,]
      rownames(rsm) = paste(pat,gsub(paste0("_",direction),"",rownames(rsm)),sep="_")
      pres[[pat]]=rsm
      best[[pat]]=outputs[[pat]]$res[filt]
    }
    pres=as.data.frame(t(do.call("rbind",pres)))
    best=as.data.frame(do.call("rbind",best))
    colnames(pres) = gsub(paste0("_",chan),"",colnames(pres))
    colnames(best) = gsub(paste0("_",chan),"",colnames(best))
    if(direction=="UP"){ylab="Proportion above controls"}else{ylab="Proportion below controls"}
    stripchart(pres,vertical=TRUE,pch=16,col=rgb(1,0,0,0.075),method="jitter",jitter=0.3,las=2,ylab=ylab,main=chan,cex.axis=1.35,cex.lab=1.5,cex=0.5,ylim=c(0,0.6))
    points(c(rbind(as.numeric(best[,1]),as.numeric(best[,2]))),pch="-",cex=4)
    abline(v=c(seq(2.5,18.5,2)),lty=2,lwd=2,col="black")
  }
}
par(op)
dev.off()

# Stripchart of deltas
png("SummaryPermute.png",width=1200,height=1200,pointsize=16)
op = par(mfrow=c(2,2),mai=c(1.5,1.2,0.5,0.05),mgp=c(4,1,0))
for(chan in c("NDUFB8","MTCO1")){
  for(direction in c("UP","DOWN")){
    pres = list()
    best=list()
    pup = c()
    pdown = c()
    qup = c()
    qdown = c()
    for(pat in pats){
      filt = grepl(paste(chan,direction,sep="_"),rownames(outputs[[pat]]$resb))&grepl("DELTA",rownames(outputs[[pat]]$resb))
      rsm = outputs[[pat]]$resb[filt,]
      pres[[pat]]=rsm
      best[[pat]]=outputs[[pat]]$res[filt]
      pup = c(pup,outputs[[pat]][[paste("p_DELTA",chan,"UP",sep="_")]])
      pdown = c(pdown,outputs[[pat]][[paste("p_DELTA",chan,"DOWN",sep="_")]])
      qup = c(qup,qvals[paste(pat,"p_DELTA",chan,"UP",sep="_")])
      qdown = c(qdown,qvals[paste(pat,"p_DELTA",chan,"DOWN",sep="_")])
    }
    pres=as.data.frame(t(do.call("rbind",pres)))
    best=as.data.frame(do.call("rbind",best))
    colnames(pres) = gsub(paste0("_",chan),"",colnames(pres))
    colnames(best) = gsub(paste0("_",chan),"",colnames(best))
    if(direction=="UP"){
      ylab="Change in proportion of fibres above controls"
      txt = ifelse(pup<0.05,"o","")
      qtxt = ifelse(qup<0.05,"*","")
    }else{
      ylab="Change in proportion of fibres below controls"
      txt = ifelse(pdown<0.05,"o","")
      qtxt = ifelse(qdown<0.05,"*","")
    }
    stripchart(pres,vertical=TRUE,pch=16,col="#00008B",method="jitter",jitter=0.2,las=2,ylab=ylab,main=chan,cex.axis=1.5,cex.lab=1.5,cex=0.5,ylim=c(-0.6,0.6))
    abline(h=0.0,lwd=2,lty=2)
    #text(seq(1,length(pats)),rep(0.05*(par("usr")[4]-par("usr")[3])+par("usr")[3],length(pats)),txt,cex=2,col="blue" )
    text(seq(1,length(pats)),rep(0.05*(par("usr")[4]-par("usr")[3])+par("usr")[3],length(pats)),qtxt,cex=2,col="black")
    capture.output(summary(pres), file = paste0(paste("Delta",chan,direction,sep="_"),".txt"))
    
  }
}
par(op)
dev.off()

head(DOWN)
write.csv(DOWN, "Down.csv")
write.csv(UP, "Up.csv")
write.csv(pvals, "pValues.csv")
pvals
