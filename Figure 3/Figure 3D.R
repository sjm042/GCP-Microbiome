genus.abd <- read.table("top24.abundance.genera.fungi.xls",sep="\t",header = T,row.names = 1)
phylum2genus <- read.table("phylum2genus.fungi.txt",header = T)
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
#col_fun = colorRamp2(c(2:7), c('#a6bddb','#67a9cf','#3690c0','#02818a','#016c59','#014636'))
ht = HeatmapAnnotation(df = annot.r$phylum,abd.mean=anno_barplot(annot.r$abd.mean),
                       col=list(phylum=c("Ascomycota"="#984ea3","Basidiomycota"="#4daf4a")),
                       border = T,
                       #annotation_name_side='right',
                       which = "row")

annot.c <- data.frame(group=  sapply(strsplit(colnames(genus.abd.reshape),"_"), function(x){x[[1]]}))
rownames(annot.c) <- colnames(genus.abd.reshape)

ht.col = HeatmapAnnotation(df = annot.c,
                           border = T,
                           col=list(group=c("CK0"="#279773","GCP01"="#D05D15","GCP02"="#6F6BAA","GCP03"="#D52B81","GCP04"="#54C1DF","GCP05"="grey","GCP06"="#F4D92A","GCP08"="#EA7E34","GCP11"="#AD529B","GCP16"="#793590","GCP18"="#B8D552","GCP19"="#43938B")),
                           which = "col")

genus.abd.reshape2 <- t(apply(genus.abd.reshape, MARGIN = 1, FUN = scale ))
colnames(genus.abd.reshape2) <- colnames(genus.abd.reshape)

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
