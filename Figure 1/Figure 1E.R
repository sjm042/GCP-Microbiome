library(ComplexHeatmap)
library(RColorBrewer)
library(circlize)
omt.count <- read.table("OMT-count.txt2.txt",header = T,sep="\t",row.names = 1,quote = "\"",check.names = F)
omt.count <- omt.count %>% group_by(count) %>% summarise(n=n())
omt.count$count <- as.factor(omt.count$count)
omt <- read.table("ALL_sample_OMT-46omt-use-this.txt",header = T,sep="\t",row.names = 1,quote = "\"",check.names = F)
omt.reshape3 <- omt[omt$omt.counts>3,5:40]
rownames(omt.reshape3) <- omt$Compounds[omt$omt.counts>3]
omt.reshape4 <- t(apply(omt.reshape3, MARGIN = 1, FUN = scale ))
colnames(omt.reshape4) <- colnames(omt.reshape3)
omt.reshape5 <- apply(omt.reshape4, 1, mean.3) %>%  t()
colnames(omt.reshape5) <- sapply(strsplit(colnames(omt.reshape4),"_"), function(x){x[[1]]}) %>% unique()
annot.c4 <- read.table("group4.txt",row.names = 1,sep="\t",header = T)
pdf("omt.heatmap.no.split.row.OMT6-cluster.pdf",width = 7,height =5.5)
ComplexHeatmap::Heatmap(omt.reshape5,
                        border =T,cluster_columns =  F,
                        left_annotation = ht,
                        #row_split = annot.r,
                        column_split = annot.c4,
                        col <- rev(c('#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837'))
)
dev.off()
