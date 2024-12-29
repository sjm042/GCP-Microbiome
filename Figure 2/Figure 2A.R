library(vegan)
library(reshape2)
library(tidyverse)
library(ggforce)
library(ggrepel)
otu.table<- read.table("otu.table.filter.txt",sep="\t",header = T,row.names = 1)
### PCoA #########
pcoa  <- vegan::vegdist(t(otu.table), method = "bray") %>%  ape::pcoa(.)
eig <- pcoa$values[,1]
vectors <- data.frame(pcoa$vectors[,1:2])
pc1 <- eig[1]/sum(eig)*100
pc2 <- eig[2]/sum(eig)*100
xlab <- paste("PCo1 ","(",round(pc1,1),"%)",sep="")
ylab <- paste("PCo2 ","(",round(pc2,1),"%)",sep="")
data<- data.frame(sample=rownames(vectors), vectors) 
data$year <- str_sub(data$sample,-7,-3)
data$year <- factor(data$year,levels = c("CK0","GCP01","GCP02","GCP03","GCP04","GCP05","GCP06","GCP08","GCP11","GCP16","GCP18","GCP19"))
data$group <- c(rep("Group1",3),rep("Group3",12),rep("Group1",6),rep("Group2",9),rep("Group3",6))
pdf("PCoA.16s.pdf",width = 4.5,height = 3.5)
ggplot(data, aes(x=Axis.1,y=Axis.2))+#,colour=year
  geom_point(aes(fill=year),size = 3,shape=21,alpha=0.8)+theme_bw()+
  xlab(xlab)+ylab(ylab)+
  theme(panel.grid=element_blank(),
        legend.position="right")+
  geom_vline(xintercept = 0, linetype="dashed", color="grey50")+
  geom_hline(yintercept = 0,linetype="dashed", color="grey50")+
  scale_fill_manual(values = c("#279773","#D05D15","#6F6BAA","#D52B81","#54C1DF","#393636","#F4D92A","#EA7E34","#AD529B","#793590","#B8D552","#43938B","#B8D552","#4FBAEA","#A6774C"))+
  #scale_colour_manual(values = mycolor12)+
  geom_text_repel(aes(label=sample),size=0.9)+
  ggforce::geom_mark_ellipse(aes(fill = group,color=group))
dev.off()
