library(tidyverse)


####  Bacteria ####
sum_g <- read.table("otu.table.filter.txt",sep="\t",header = T,row.names = 1)
#sum_g <- sum_g[-1,colnames(sum_g) %>% sort()]
sum_g_filter <-sum_g
metabolism.all <- read.table("ALL_sample_data.txt",sep="\t",header = T,row.names = 1)
metabolism <- metabolism.all[,colnames(sum_g_filter)]
colnames(metabolism)
a <- data.frame(n=1:(nrow(sum_g_filter)*nrow(metabolism)))
k=1
for(i in 1:nrow(sum_g_filter)){
  for(j in 1:nrow(metabolism)){
    p <- cor.test(as.numeric(sum_g_filter[i,]),as.numeric(metabolism[j,]),method = "spearman",exact = FALSE)$p.value
    
    a[k,1] <- rownames(sum_g_filter)[i]
    a[k,2] <- rownames(metabolism)[j]
    a[k,3] <- cor.test(as.numeric(sum_g_filter[i,]),as.numeric(metabolism[j,]),method = "spearman",exact = FALSE)$estimate %>% as.numeric()
    a[k,4] <- cor.test(as.numeric(sum_g_filter[i,]),as.numeric(metabolism[j,]),method = "spearman",exact = FALSE)$p.value
    k=k+1
  }
}

colnames(a) <- c("OTU","index","cor","pvalue")
a$p.adjust <- p.adjust(a$pvalue,method = "BH")
z <- a[a$p.adjust<=0.05&abs(a$cor)>=0.5,] 
tmp <- z[z$index=="mws0043",]
write.table(z,"OTU.cor.metabolism.filter.cor0.5.p.0.05.txt",sep="\t",row.names = FALSE, quote=FALSE)


#### fungus ####
fungus_sum_g <- read.table("otu.filter.4ITS.txt",sep="\t",header = T,row.names = 1)
fungus_sum_g_filter <- fungus_sum_g
colnames(fungus_sum_g_filter)==colnames(metabolism)
a <- data.frame(n=1:(nrow(fungus_sum_g_filter)*nrow(metabolism)))
k=1
for(i in 1:nrow(fungus_sum_g_filter)){
  for(j in 1:nrow(metabolism)){
    p <- cor.test(as.numeric(fungus_sum_g_filter[i,]),as.numeric(metabolism[j,]),method = "spearman",exact = FALSE)$p.value
    
    a[k,1] <- rownames(fungus_sum_g_filter)[i]
    a[k,2] <- rownames(metabolism)[j]
    a[k,3] <- cor.test(as.numeric(fungus_sum_g_filter[i,]),as.numeric(metabolism[j,]),method = "spearman",exact = FALSE)$estimate %>% as.numeric()
    a[k,4] <- cor.test(as.numeric(fungus_sum_g_filter[i,]),as.numeric(metabolism[j,]),method = "spearman",exact = FALSE)$p.value
    k=k+1
  }
}

colnames(a) <- c("OTU","index","cor","pvalue")
a$p.adjust <- p.adjust(a$pvalue,method = "BH")
z2 <- a[a$p.adjust<=0.05&abs(a$cor)>=0.5,] 
tmp <- zz2[zz2$index=="mws0043",]
z2$abs_cor <- abs(z2$cor)
z2$cor2 <- ifelse(z2$cor<0,-1,1)
z2$kingdom <- "Fungus"
write.table(z2,"OTU_fungus.cor.metabolism.filter.cor0.5.p.0.05.txt",sep="\t",row.names = FALSE, quote=FALSE)
zz2 <- rbind(z,z2)
write.table(rbind(z,z2),"OTU-bacteria_fungus.cor.metabolism.filter.cor0.5.p.0.05.txt",sep="\t",row.names = FALSE, quote=FALSE)
## OTU-bacteria_fungus.cor.metabolism.filter.cor0.5.p.0.05.txt was furthur visual in cytoscape 3.7.2 ####
