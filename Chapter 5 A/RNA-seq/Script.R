library(limma)
library(edgeR)
library(ggplot2)
library(gplots)
library(ggrepel)
library(RColorBrewer)
library(plyr)
library(pheatmap)
library(statmod)
library(data.table)
library(tidyr)
library(extrafont)
library(ggplotlyExtra)
library(tibble)


####################################################

#Adjust the file you want to work with
pheno=read.csv("pheno.csv", row.names = 1)
data=read.csv("Genes_table.csv", row.names = 1)
dge=DGEList(data)
keep=rowSums(cpm(dge)>=1)>=5
dge=dge[keep,,keep.lib.sizes=FALSE]

dge1=as.matrix(dge)

dge=calcNormFactors(dge, method="TMM")

#Run the model for liner regression
designx=model.matrix(~0+pheno$exercise+pheno$delclass+pheno$ubiquinone)
colnames(designx)=c('Pre','Post','DelClass','Ubiquinone')
#this tells limma that your data is in pairs
corfit=duplicateCorrelation(dge1,block=pheno$pair)
#normilization step that plots varience plot 
v.norm3=voom(dge, designx,plot = TRUE, block=pheno$pair, cor=corfit$consensus)
# outputs normalised counts as an excel/dataframe
v.norm_out=as.matrix(v.norm3)
write.csv(v.norm_out,"Genes_norms_count.csv")


# MDS plot like PCA but adds the distance component to "flatten" the plot
plotMDS(v.norm3, col=as.numeric(pheno$exercise,pheno$delclass,pheno$ubiquinone))
plotMDS(v.norm3, col=as.numeric(pheno$exercise))
plotMDS(v.norm3, col=as.numeric(pheno$ubiquinone))
plotMDS(v.norm3, col=as.numeric(pheno$delclass))


#liner regrssion/ baysian - this is the DE step
fit3=lmFit(v.norm3, designx)
fit3=eBayes(fit3)

con=makeContrasts(prevpost=Pre-Post, del=DelClass, treatment=Ubiquinone, levels=designx)
fit4=contrasts.fit(fit3,con)
fit4=eBayes(fit4)

#logFC and p value
prevpost=topTable(fit4, coef=1, n=Inf)
Exercise=topTable(fit4, coef=1, n=Inf)
Exercise_sig=topTable(fit4, coef=1, p.value=0.05, lfc=1.5, n=Inf)
write.csv(Exercise, "Genes_Ex_040221.csv")
write.csv(Exercise_sig, "Genes_Ex_sig_120121.csv")

DelClass=topTable(fit4, coef=2, n=Inf)
DelClass_sig=topTable(fit4, coef=2, p.value=0.05, lfc=1.5, n=Inf)
write.csv(DelClass, "Genes_Del_040221.csv")

Ubiquinone=topTable(fit4, coef=3, n=Inf)
Ubiquinone_sig=topTable(fit4, coef=3, p.value=0.05, lfc=1.5, n=Inf)
write.csv(Ubiquinone, "Genes_Ubiq_040221.csv")

#QQ plot
exp.pvalues<-(rank(Exercise$P.Val, ties.method="first")+.5)/(length(Ubiquinone$P.Val)+1)
obs.pvalues<-(Exercise$P.Val)
plot(-log10(exp.pvalues), -log10(obs.pvalues), asp=0)
abline(0,1)

#Set up PCA analysis - no Norm
dge=as.matrix(dge)
dge_genes=row.names(dge)
pca_data=prcomp(t(dge))
pca_data_perc=round(100*pca_data$sdev^2/sum(pca_data$sdev^2),1)
df_pca_data=data.frame(PC1=pca_data$x[,1], PC2=pca_data$x[,2], sample=colnames(data), condition = as.factor(pheno$pair))
find_hull=function(df_pca_data) df_pca_data[chull(df_pca_data$PC1,df_pca_data$PC2),]
hulls=ddply(df_pca_data, "condition", find_hull)

ggplot(df_pca_data, aes(PC1,PC2, color=condition, fill=condition))+ 
  geom_point(size=4)+ 
  labs(x=paste0("PC1(",pca_data_perc[1],")"),y=paste0("PC2 (",pca_data_perc[2],")"))+
  scale_color_manual(values = c("#008080","#31367e","#b4127c","#ff8863","#fec83c"))+
  geom_text_repel(aes(label=sample), point.padding = 0.5)


#Volcano plot
with(Exercise, plot(logFC, -log10(P.Value), pch=20, main="Volcano plot - Exercise",
                    cex=1.0,
                    xlab=bquote(~Log[2]~fold~change), ylab=bquote(~-log[10]~P~value), 
                    col="#999999"))
with(subset(Exercise, P.Value<0.05 & abs(logFC)>=1.5),
     points(logFC, -log10(P.Value), pch=20, col="#00b4c4"))

abline(v=0, col="black", lty=3, lwd=1.0)
abline(v=-1.5, col="black", lty=3, lwd=1.0)
abline(v=1.5, col="black", lty=3, lwd=1.0)
abline(h =1.3, col = "black", lty = 3, lwd = 1.0)

#produce the graph
DATA=read.csv("Ex_Volcano_Annotation.csv")

G = ggplot(DATA, aes(x=logFC, y=-log10(P.Value), name=ensgene)) +
  geom_point(aes(colour=test), size=2, alpha=0.5) +
  scale_colour_manual(values=c("darkgrey", "#008080")) +
  geom_vline(xintercept=1.5, colour="black", linetype=3) +
  geom_vline(xintercept=-1.5, colour="black", linetype=3) +
  geom_hline(yintercept=-log10(0.05), colour="black", linetype=3) +
  ylim(0,4) +
  xlim(-3,4) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x=expression(paste("Log"[2], " FC"))) +
  labs(y=expression(paste("Log"[10], " P value"))) +
  geom_text_repel(data = DATA, aes(logFC, -log10(P.Value), label=DEgenes), size=3, box.padding = unit(0.3, "lines"), point.padding = unit(0.3, "lines"))
G


#Re-read pheno table to correct row names
filter_EX=subset(subset(Exercise, P.Value<0.05 & abs(logFC)>=1.5))

sig_prot=v.norm_out[rownames(filter_EX),]

annotation=data.frame(EX=pheno[,"exercise"], row.names = row.names(pheno))
annotation$EX=as.character(annotation$EX)
annotation$EX[annotation$EX=="N"]="Pre"
annotation$EX[annotation$EX=="Y"]="Post"
annotation$EX=as.factor(annotation$EX)
ann_col=list(EX=c(Pre="#800000", Post="#140035"))
breaklist=seq(-4,4,by=1)
heat_col=brewer.pal(n=8,name = "RdBu")
pheatmap(sig_prot, color = heat_col, na_col = "white", breaks = breaklist,
         cluster_rows = T, annotation = annotation, scale = "row",
         annotation_colors = ann_col, fontsize = 6)


#Volcano plot with S. Cockwell

#prepare the file to read
Exercise$test = Exercise$P.Value < 0.05 & abs(Exercise$logFC) > 1.5

Exercise2 = rownames_to_column(Exercise, var="ensgene")
write.csv(Exercise2, "Ex_Volcano_Annotation.csv")
# add DE genes columns for those genes that are going to be annotated

#Exercise2 = rownames_to_column(Exercise, var="ensgene")
#Exercise2 
#mutate(newP.Value = (-log10(P.Value)))
#Exercise2$test = Exercise$newP.Value < 0.05 & abs(Exercise$logFC) > 1.5
#Exercise2