genus.abd <- read.table("../top51.abundance.genera.bacteria.xls",sep="\t",header = T,row.names = 1)
phylum2genus <- read.table("../phyla2genera.txt",header = T)
tmp <- c()
phylum <- c()
for(i in 1:nrow(genus.abd)){
  tmp <- phylum2genus$phyla[phylum2genus$genera==rownames(genus.abd)[i]]
  phylum <- c(phylum,tmp)
}
genus.abd$phylum <- phylum

genus.abd.reshape <- genus.abd[,1:36]


#### group  ####
mean.3 <- function(x){
  x <- as.numeric(x)
  tmp.mean <- c(mean(x[1:3]),mean(x[4:6]),mean(x[7:9]),mean(x[10:12]),mean(x[13:15]),mean(x[16:18]),mean(x[19:21]),mean(x[22:24]),mean(x[25:27]),mean(x[28:30]),mean(x[31:33]),mean(x[34:36]))
  return(tmp.mean)
}

mean.3(genus.abd.reshape[1,])
genus.abd.reshape3 <- apply(genus.abd.reshape, 1, mean.3) %>%  t()
colnames(genus.abd.reshape3) <- annot.c$group %>% unique()

genus.abd.reshape4 <- t(apply(genus.abd.reshape3, MARGIN = 1, FUN = scale ))
colnames(genus.abd.reshape4) <- annot.c$group %>% unique()

pdf("bac.genus.heatmap.split.col.add.adb.mean.groups.pdf",width = 7.5,height = 8)
ComplexHeatmap::Heatmap(genus.abd.reshape4,#    ,
                        border =T,cluster_columns =  F,
                        left_annotation = ht,
                        row_split = annot.r$phylum,
                        #top_annotation = ht.col,
                        col <- rev(c('#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837'))
)
dev.off()


##### fungi #####

genus.abd <- read.table("../top24.abundance.genera.fungi.xls",sep="\t",header = T,row.names = 1)
phylum2genus <- read.table("../../ITS/phylum2genus.fungi.txt",header = T)
tmp <- c()
phylum <- c()
for(i in 1:nrow(genus.abd)){
  tmp <- phylum2genus$phyla[phylum2genus$genera==rownames(genus.abd)[i]]
  phylum <- c(phylum,tmp)
}
genus.abd$phylum <- phylum

genus.abd.reshape <- genus.abd[,1:36]


annot.r <- genus.abd[,"phylum",drop=FALSE]
annot.r$abd.mean <- rowMeans(genus.abd.reshape)

#### groups #### 
genus.abd.reshape3 <- apply(genus.abd.reshape, 1, mean.3) %>%  t()
colnames(genus.abd.reshape3) <- annot.c$group %>% unique()

genus.abd.reshape4 <- t(apply(genus.abd.reshape3, MARGIN = 1, FUN = scale ))
colnames(genus.abd.reshape4) <- annot.c$group %>% unique()

pdf("fungi.genus.heatmap.split.col.add.adb.mean.groups.pdf",width = 6,height = 4.5)
ComplexHeatmap::Heatmap(genus.abd.reshape4,#    ,
                        border =T,cluster_columns =  F,
                        left_annotation = ht,
                        row_split = annot.r$phylum,
                        #top_annotation = ht.col,
                        col <- rev(c('#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837'))
)
dev.off()

