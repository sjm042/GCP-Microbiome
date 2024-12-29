library(stringr)
library(pheatmap)
library(dplyr)
library(ggplot2)
library(vegan)
library(Cairo)
alpha.t <- read.table("alpha.txt",header = T,sep="\t")
alpha.t$year <- str_sub(alpha.t$Sample,-7,-3)
alpha.t$year <- factor(alpha.t$year,levels = c("T_CK0","GCP01","GCP02","GCP03","GCP04","GCP05","GCP06","GCP08","GCP11","GCP16","GCP18","GCP19"))
#alpha$region2 <- ifelse(alpha$region=="D"|alpha$region=="X"|alpha$region=="L"|alpha$region=="S","Other","Xinhui")
pdf("T-GCP-shannon_e.pdf",width = 5,height = 3.5)
ggplot(alpha,aes(year,shannon_e,fill=year))+
  geom_boxplot()+theme_bw()+
  scale_fill_manual(values = c("#279773","#D05D15","#6F6BAA","#D52B81","#54C1DF","#393636","#F4D92A","#EA7E34","#AD529B","#793590","#B8D552","#43938B"))+
  theme(legend.position="right",
        legend.text = element_text(size = 15),
        axis.text.x= element_text(size=10, colour = "black", vjust = 0.5,  angle = 45),#hjust = 1,
        axis.text.y= element_text(size=15, colour = "black"),
       axis.title = element_text(size=15),
       plot.title = element_text(hjust = 0.5),
       legend.title=element_text(size=20),
        legend.background = element_blank(),
        panel.background = element_rect(fill='transparent'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()

#### LSD test ####
model <- aov(shannon_e~year,data=alpha.t)
out <- LSD.test(model,"year",p.adj = "fdr")
out$group ## print the LSD result and add the result in the figure.
