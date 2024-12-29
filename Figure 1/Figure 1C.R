


up_down <- read.table("up_and_down.number.in.GCP.txt",sep="\t",header = T)

up_down$group.name <- factor(up_down $group.name,levels = c("CK_vs_GCP1","CK_vs_GCP2","CK_vs_GCP3","CK_vs_GCP4","CK_vs_GCP5","CK_vs_GCP6","CK_vs_GCP8","CK_vs_GCP11","CK_vs_GCP16","CK_vs_GCP18","CK_vs_GCP19"))
library(ggplot2)
pdf("up-down.regulated.in.GCP.pdf",width = 5,height = 3.5)
ggplot(up_down,aes(group.name,count,fill=regulated))+
  geom_bar(stat = "identity",position = "dodge")+
  geom_text( aes(label=count),vjust=-0.5,hjust=0.5,position = position_dodge(0.8),size=3)+theme_classic() +
  theme(legend.position="right",legend.text = element_text(size = 10),
        axis.ticks.length=unit(0.1,'cm'),
        axis.text.x= element_text(size=10, colour = "black", vjust = 0.5,  angle = 45),#hjust = 1,
        axis.text.y= element_text(size=10,colour = "black"),
        axis.title = element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        legend.title=element_text(size=10))+
  theme(axis.text = element_text(colour = 'black'))
dev.off()

c("CK_vs_GCP1","CK_vs_GCP2","CK_vs_GCP3","CK_vs_GCP4","CK_vs_GCP5","CK_vs_GCP6","CK_vs_GCP8","CK_vs_GCP11","CK_vs_GCP16","CK_vs_GCP18","CK_vs_GCP19")
