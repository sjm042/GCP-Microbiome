library(ggplot2)
alpha <- read.table("alpha.bac.fungi.txt",header = T,sep="\t",row.names = 1)
alpha <- read.table("alpha.bac.fungi.mean1-6.txt",header = T,sep="\t",row.names = 1)
pre.r <- cor.test(alpha$shannon_e_bac,alpha$shannon_e_fungi)
pre.r$p.value
pdf("alpha.cor.bac.fungi.pdf",width = 3.5,height = 3.5)
ggplot(alpha,aes(shannon_e_bac,shannon_e_fungi))+
  geom_point()+stat_smooth(method = lm,level = 0.5)+
  annotate("text",label=c(paste0("Correlation R=",round(pre.r$estimate,digit=4),"\n"),paste0("Pvalue=",round(pre.r$p.value,digit=4))),x=Inf,y=-Inf,hjust=1,vjust=-0.5)
dev.off()
