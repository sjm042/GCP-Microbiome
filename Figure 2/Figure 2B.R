library(stringr)
library(pheatmap)
library(dplyr)
library(ggplot2)
library(vegan)
library(Cairo)
alpha <- read.table("alpha.for.otu.table.filter.txt",header = T,sep="\t")
alpha$year <- str_sub(alpha$Sample,-7,-3)
alpha$year <- factor(alpha$year,levels = c("CK0","GCP01","GCP02","GCP03","GCP04","GCP05","GCP06","GCP08","GCP11","GCP16","GCP18","GCP19"))
#alpha$region2 <- ifelse(alpha$region=="D"|alpha$region=="X"|alpha$region=="L"|alpha$region=="S","Other","Xinhui")
pdf("shannon_e.pdf",width = 5,height = 3.5)
ggplot(alpha,aes(year,shannon_e,fill=year))+
  geom_boxplot()+theme_classic()+
  theme(axis.text.x=element_text(angle=30,hjust = 1,vjust = 1))+
  scale_fill_manual(values = c("#279773","#D05D15","#6F6BAA","#D52B81","#54C1DF","#393636","#F4D92A","#EA7E34","#AD529B","#793590","#B8D552","#43938B"))

dev.off()
