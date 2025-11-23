# Graphique saumon

library(factoextra)
library(FactoMineR)
library(ggplot2)
library(ggrepel)
library(plotly)
library(dplyr)


### Import et traitement des données
setwd("C:\\documents\\Agrocampus\\M2\\Classification_Non_Supervisee\\RmD\\Clustering\\bilan\\seances\\data")
dta_saumon<-read.table("saumon.csv",
                       stringsAsFactors = TRUE,
                       dec=",",
                       sep=";",
                       header = TRUE)
dta_saumon_N<-dta_saumon[c(1064:1088),c(1,3:32)]
dta_saumon_RB<-dta_saumon[c(1:1063),]
dta_saumon_R<-dta_saumon[,c(3:32)]

dta_saumon_R<-as.matrix(dta_saumon_R)

res.pca.3<-PCA(dta_saumon[c(-1,-2)],
               quali.sup = c(31:98),
               ind.sup = 1064:1088,
               graph = TRUE)
res.HCPC.3<-HCPC(res.pca.3,nb.clust=-1)

res.HCPC.3$desc.var



###############################

coord_ind <- res.pca.3$ind$coord
clust     <- res.HCPC.3$data.clust$clust
ind_sup   <- as.data.frame(res.pca.3$ind.sup$coord)
rownames(ind_sup) <- dta_saumon_N$IKIDEN

combined <- cbind(coord_ind,clust)

rownames(ind_sup)

# ajouter le bleu : les habitudes de consommation 
quali_sup <- as.data.frame(res.pca.3$quali.sup$coord)

# conserver que ceux qui sont intéressants 
quali_sup <- quali_sup[c(236,226,188,167,224,63, 
                         29,203,28,
                         233,128,25),]

vec_col <- c("#E41A1C", "#377EB8", "#E41A1C", "#E41A1C", 
             "#4DAF4A", "#4DAF4A", "#377EB8", "#E41A1C", 
             "#E41A1C", "#E41A1C", "#E41A1C", "#E41A1C", 
             "#E41A1C", "#377EB8", "#377EB8","#377EB8", 
             "#377EB8", "#E41A1C", "#4DAF4A", "#E41A1C",
             "#4DAF4A", "#E41A1C", "#4DAF4A", "#E41A1C", 
             "#4DAF4A")
col_quali <- c(rep("red4",6), rep("blue4",3),rep("green4",3))


p1 <- combined %>%
  ggplot() +
  aes(x = Dim.1, y = Dim.2, color = factor(clust)) +   # <-- col = clust déplacé ici
  geom_point(size = 0.8, alpha = 0.4) +
  scale_color_manual(
    values = c(
      "1" = "#E41A1C",
      "2" = "#377EB8",
      "3" = "#4DAF4A"
    ),
    name = "Cluster"
  ) + 
  
  
  geom_point(aes(x = Dim.1, y = Dim.2),   # ajouter le noir : les caractéristiques physico chimiques + pays 

                size = 2, 
                color = vec_col, 
                shape = 16,
                data = ind_sup) +
  
  geom_text_repel(data = ind_sup, 
                  label = row.names(ind_sup), 
                  col = vec_col, 
                  size = 4) + 
  
  
  geom_point(aes(x = Dim.1, y = Dim.2),     # ajouter le bleu 
                size = 2, 
                color = col_quali, 
                shape = 16, 
                data = quali_sup) +
    
    geom_text_repel(data = quali_sup, 
                    label = row.names(quali_sup), 
                    col = col_quali, 
                    size = 2.8) +
  
  # légendes et titres 
  labs(
    title = "Clustering Saumon",
    x = "Dimension 1",
    y = "Dimension 2"  ) +
  
  theme(
    legend.position = "right",
    plot.title = element_text(face="bold", size=16, hjust = 0.5),
    axis.title = element_text(face="bold"),
    panel.grid.minor = element_blank()
  )

p1





