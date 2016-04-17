rm(list=ls())

library(ggplot2)

dpi8 <- read.table("8dpi.csv", header = TRUE,  sep = ";",  stringsAsFactors = FALSE)


dpi8$KNO3 <- as.factor(dpi8$KNO3)
dpi8$NaCl <- as.factor(dpi8$NaCl)
dpi8$souches <- as.factor(dpi8$souches)

data <- data.frame()

for (S in levels(dpi8$souches)) {
  for (K in levels(dpi8$KNO3)) {
    for (N in levels(dpi8$NaCl)) {
      print(!is.nan(mean(dpi8[dpi8[,"NaCl"]==N & dpi8[,"KNO3"]==K & dpi8[,"souches"]==S,"app.col"])))
      if(!is.nan(mean(dpi8[dpi8[,"NaCl"]==N & dpi8[,"KNO3"]==K & dpi8[,"souches"]==S,"app.col"])))
        print(c(S,K,N))
        moy <- mean(dpi8[dpi8[,"NaCl"]==N & dpi8[,"KNO3"]==K & dpi8[,"souches"]==S,"app.col"], na.rm=T)
        sd <- sd(dpi8[dpi8[,"NaCl"]==N & dpi8[,"KNO3"]==K & dpi8[,"souches"]==S,"app.col"], na.rm=T)
        if(length(dpi8[dpi8[,"NaCl"]==N & dpi8[,"KNO3"]==K & dpi8[,"souches"]==S,"SR"])==1){sd <- 0}
        nod <- mean(dpi8[dpi8[,"NaCl"]==N & dpi8[,"KNO3"]==K & dpi8[,"souches"]==S,"nodules"], na.rm=T)
        nodsd <- sd(dpi8[dpi8[,"NaCl"]==N & dpi8[,"KNO3"]==K & dpi8[,"souches"]==S,"nodules"], na.rm=T)
        fixp <- mean(dpi8[dpi8[,"NaCl"]==N & dpi8[,"KNO3"]==K & dpi8[,"souches"]==S,"fixp"], na.rm=T)
        fixm <- mean(dpi8[dpi8[,"NaCl"]==N & dpi8[,"KNO3"]==K & dpi8[,"souches"]==S,"fixm"], na.rm=T)
        data <- rbind(data,cbind(S,K,N,moy,sd,nod, nodsd, fixm, fixp))
    }
  }
}

data$moy <- as.numeric(as.character(data$moy))
data$sd <- as.numeric(as.character(data$sd))
data$nod <- as.numeric(as.character(data$nod))
data$nodsd <- as.numeric(as.character(data$nodsd))
data$fixm <- as.numeric(as.character(data$fixm))
data$fixp <- as.numeric(as.character(data$fixp))

data <- data[!is.nan(data[,"moy"])&!is.na(data[,"sd"])&!is.nan(data[,"nod"]),]


p <- ggplot(data, aes(interaction(K,N,S), moy, colour=factor(N), fill=factor(K))) +
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymax=moy + sd, ymin=moy - sd), width = 0.2, size = 1)+
  annotate("text", x = 1:16, y = -0.1, label = c("A",rep("B",9),"C", rep("NI",5)))+
  xlab("Concentrations de KNO3 et de NaCl") +
  ylab("Moyenne de la taille des hypocotyles (cm)")+
  labs(title="Représentation de la taille des hypocotyles en fonction\nde la souche inoculée, du KNO3 et du NaCl à 8 DPI") +
  guides(fill = guide_legend(title="KNO3 (mM)"), color = guide_legend(title="NaCl (mM)"))+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  scale_fill_hue(l=40, c=30)

pdf(file="taille8.pdf", width=10)
print(p)
dev.off()



p2 <- ggplot(data, aes(interaction(K,N,S), nod, colour=factor(N), fill=factor(K))) +
  geom_bar(stat = "identity",position = position_dodge())+
  geom_errorbar(aes(ymax=nod + nodsd, ymin=nod - nodsd), width = 0.2, size = 1)+
  annotate("text", x = 1:16, y = -0.4, label = c("A",rep("B",9),"C", rep("NI",5)))+
  xlab("Concentrations de KNO3 et de NaCl\n(KNO3.NaCl.Souche)") +
  ylab("Nombre moyen de nodules")+
  labs(title="Représentation du nombre moyen de nodules en fonction\nde la souche inoculée, du KNO3 et du NaCl à 8 DPI") +
  guides(fill = guide_legend(title="KNO3 (mM)"), color = guide_legend(title="NaCl (mM)"))+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  scale_fill_hue(l=40, c=30)

pdf(file="nodmean8.pdf", width=10)
print(p2)
dev.off()



p3 <- ggplot(data, aes(interaction(K,N,S), fixp, colour=factor(N), fill=factor(K))) +
  geom_bar(stat = "identity",position = position_dodge())+
  # geom_errorbar(aes(ymax=nod + nodsd, ymin=nod - nodsd), width = 0.2, size = 1)+
  # annotate("text", x = 1:16, y = -0.1, label = c("A",rep("B",9),"C", rep("NI",5)))+
  xlab("Concentrations de KNO3 et de NaCl\n(KNO3.NaCl.Souche)") +
  ylab("Moyenne du nombre de nodules fix+")+
  labs(title="Représentation du nombre de nodules fix+ en fonction\nde la souche inoculée, du KNO3 et du NaCl à 8 DPI") +
  guides(fill = guide_legend(title="KNO3 (mM)"), color = guide_legend(title="NaCl (mM)"))+
  # theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  scale_fill_hue(l=40, c=30)

  pdf(file="nod+mean8.pdf", width=10)
  print(p3)
  dev.off()


p4 <- ggplot(data, aes(interaction(K,N,S), fixm, colour=factor(N), fill=factor(K))) +
  geom_bar(stat = "identity",position = position_dodge())+
  # geom_errorbar(aes(ymax=nod + nodsd, ymin=nod - nodsd), width = 0.2, size = 1)+
  # annotate("text", x = 1:16, y = -0.1, label = c("A",rep("B",9),"C", rep("NI",5)))+
  xlab("Concentrations de KNO3 et de NaCl\n(KNO3.NaCl.Souche)") +
  ylab("Moyenne du nombre de nodules fix-")+
  labs(title="Représentation du nombre de nodules fix- en fonction\nde la souche inoculée, du KNO3 et du NaCl à 8 DPI") +
  guides(fill = guide_legend(title="KNO3 (mM)"), color = guide_legend(title="NaCl (mM)"))+
  # theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  scale_fill_hue(l=40, c=30)

  pdf(file="nod-mean8.pdf", width=10)
  print(p4)
  dev.off()


p5 <- ggplot(data, aes(interaction(K,N,S), fixp/nod, colour=factor(N), fill=factor(K))) +
  geom_bar(stat = "identity",position = position_dodge())+
  # geom_errorbar(aes(ymax=nod + nodsd, ymin=nod - nodsd), width = 0.2, size = 1)+
  # annotate("text", x = 1:16, y = -0.1, label = c("A",rep("B",9),"C", rep("NI",5)))+
  xlab("Concentrations de KNO3 et de NaCl\n(KNO3.NaCl.Souche)") +
  ylab("Rapport du nombre moyen de nodosités fixatrices sur le nombre de nodosités")+
  labs(title="Représentation du rapport du nombre moyen de nodules fix+ sur le nombre total de nodosités\nen fonction de la souche inoculée, du KNO3 et du NaCl à 8 DPI") +
  guides(fill = guide_legend(title="KNO3 (mM)"), color = guide_legend(title="NaCl (mM)"))+
  # theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  scale_fill_hue(l=40, c=30)

  pdf(file="rapfix+nod8.pdf", width=10)
  print(p5)
  dev.off()


rm(list=ls())

dpi14 <- read.table("14dpi.csv", header = TRUE,  sep = ";",  stringsAsFactors = FALSE)


dpi14$KNO3 <- as.factor(dpi14$KNO3)
dpi14$NaCl <- as.factor(dpi14$NaCl)
dpi14$souches <- as.factor(dpi14$souches)
dpi14$nodo <- as.numeric(dpi14$nodo)


dpi14$SR <- dpi14$app.col / dpi14$app.rac

data <- data.frame()

for (S in levels(dpi14$souches)) {
  for (K in levels(dpi14$KNO3)) {
    for (N in levels(dpi14$NaCl)) {
      # print(!is.nan(mean(dpi14[dpi14[,"NaCl"]==N & dpi14[,"KNO3"]==K & dpi14[,"souches"]==S,"app.col"])))
      if(!is.nan(mean(dpi14[dpi14[,"NaCl"]==N & dpi14[,"KNO3"]==K & dpi14[,"souches"]==S,"SR"], na.rm=T)))
        print(c(S,K,N))
        moy <- mean(dpi14[dpi14[,"NaCl"]==N & dpi14[,"KNO3"]==K & dpi14[,"souches"]==S,"SR"], na.rm=T)
        sd <- sd(dpi14[dpi14[,"NaCl"]==N & dpi14[,"KNO3"]==K & dpi14[,"souches"]==S,"SR"], na.rm=T)
        if(length(dpi14[dpi14[,"NaCl"]==N & dpi14[,"KNO3"]==K & dpi14[,"souches"]==S,"SR"])==1){sd <- 0}
        nod <- mean(dpi14[dpi14[,"NaCl"]==N & dpi14[,"KNO3"]==K & dpi14[,"souches"]==S,"nodo"], na.rm=T)
        nodsd <- sd(dpi14[dpi14[,"NaCl"]==N & dpi14[,"KNO3"]==K & dpi14[,"souches"]==S,"nodo"], na.rm=T)
        somme <- sum(!is.na(dpi14[dpi14[,"NaCl"]==N & dpi14[,"KNO3"]==K & dpi14[,"souches"]==S,"nodo"]))
        if(somme == 1){nodsd <- 0}
        fixp <- mean(dpi14[dpi14[,"NaCl"]==N & dpi14[,"KNO3"]==K & dpi14[,"souches"]==S,"fixp"], na.rm=T)
        fixm <- mean(dpi14[dpi14[,"NaCl"]==N & dpi14[,"KNO3"]==K & dpi14[,"souches"]==S,"fixm"], na.rm=T)
        data <- rbind(data,cbind(S,K,N,moy,sd,nod, nodsd, fixm, fixp))
    }
  }
}

data$moy <- as.numeric(as.character(data$moy))
data$sd <- as.numeric(as.character(data$sd))
data$nod <- as.numeric(as.character(data$nod))
data$nodsd <- as.numeric(as.character(data$nodsd))
data$fixm <- as.numeric(as.character(data$fixm))
data$fixp <- as.numeric(as.character(data$fixp))


data <- data[!is.nan(data[,"moy"])&!is.na(data[,"sd"])&!is.nan(data[,"nod"]),]



p <- ggplot(data, aes(interaction(K,N,S), moy, colour=factor(N), fill=factor(K))) +
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymax=moy + sd, ymin=moy - sd), width = 0.2, size = 1)+
  annotate("text", x = 1:19, y = -0.1, label = c(rep("A",3),rep("B",9),"C", rep("NI",6)))+
  xlab("Concentrations de KNO3 et de NaCl") +
  ylab("Moyenne du ratio S/R")+
  labs(title="Représentation du ratio masse de l'appareil caulinaire sur masse racinaire\nen fonction de la souche inoculée, du KNO3 et du NaCl à 14 DPI") +
  guides(fill = guide_legend(title="KNO3 (mM)"), color = guide_legend(title="NaCl (mM)")) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  scale_fill_hue(l=40, c=30)

pdf(file="SR14.pdf", width=10)
print(p)
dev.off()


p2 <- ggplot(data, aes(interaction(K,N,S), nod, colour=factor(N), fill=factor(K))) +
  geom_bar(stat = "identity",position = position_dodge())+
  geom_errorbar(aes(ymax=nod + nodsd, ymin=nod - nodsd), width = 0.2, size = 1)+
  # annotate("text", x = 1:16, y = -0.1, label = c("A",rep("B",9),"C", rep("NI",5)))+
  xlab("Concentrations de KNO3 et de NaCl\n(KNO3.NaCl.Souche)") +
  ylab("Nombre moyen de nodules")+
  labs(title="Représentation du nombre moyen de nodules en fonction\nde la souche inoculée, du KNO3 et du NaCl à 14 DPI") +
  guides(fill = guide_legend(title="KNO3 (mM)"), color = guide_legend(title="NaCl (mM)"))+
  # theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  scale_fill_hue(l=40, c=30)

pdf(file="nodmean14.pdf", width=10)
print(p2)
dev.off()



p3 <- ggplot(data, aes(interaction(K,N,S), fixp, colour=factor(N), fill=factor(K))) +
  geom_bar(stat = "identity",position = position_dodge())+
  # geom_errorbar(aes(ymax=nod + nodsd, ymin=nod - nodsd), width = 0.2, size = 1)+
  # annotate("text", x = 1:16, y = -0.1, label = c("A",rep("B",9),"C", rep("NI",5)))+
  xlab("Concentrations de KNO3 et de NaCl\n(KNO3.NaCl.Souche)") +
  ylab("Moyenne du nombre de nodules fix+")+
  labs(title="Représentation du nombre de nodules fix+ en fonction\nde la souche innoculée, du KNO3 et du NaCl à 14 DPI") +
  guides(fill = guide_legend(title="KNO3 (mM)"), color = guide_legend(title="NaCl (mM)"))+
  # theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  scale_fill_hue(l=40, c=30)

  pdf(file="nod+mean14.pdf", width=10)
  print(p3)
  dev.off()


p4 <- ggplot(data, aes(interaction(K,N,S), fixm, colour=factor(N), fill=factor(K))) +
  geom_bar(stat = "identity",position = position_dodge())+
  # geom_errorbar(aes(ymax=nod + nodsd, ymin=nod - nodsd), width = 0.2, size = 1)+
  # annotate("text", x = 1:16, y = -0.1, label = c("A",rep("B",9),"C", rep("NI",5)))+
  xlab("Concentrations de KNO3 et de NaCl\n(KNO3.NaCl.Souche)") +
  ylab("Moyenne du nombre de nodules fix-")+
  labs(title="Représentation du nombre de nodules fix- en fonction\nde la souche inoculée, du KNO3 et du NaCl à 14 DPI") +
  guides(fill = guide_legend(title="KNO3 (mM)"), color = guide_legend(title="NaCl (mM)"))+
  # theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  scale_fill_hue(l=40, c=30)

  pdf(file="nod-mean14.pdf", width=10)
  print(p4)
  dev.off()


p5 <- ggplot(data, aes(interaction(K,N,S), fixp/nod, colour=factor(N), fill=factor(K))) +
  geom_bar(stat = "identity",position = position_dodge())+
  # geom_errorbar(aes(ymax=nod + nodsd, ymin=nod - nodsd), width = 0.2, size = 1)+
  # annotate("text", x = 1:16, y = -0.1, label = c("A",rep("B",9),"C", rep("NI",5)))+
  xlab("Concentrations de KNO3 et de NaCl\n(KNO3.NaCl.Souche)") +
  ylab("Rapport du nombre moyen de nodosités fixatrices sur le nombre de nodosités")+
  labs(title="Représentation du rapport du nombre moyen de nodules fix+ sur le nombre total de nodosités\nen fonction de la souche inoculée, du KNO3 et du NaCl à 14 DPI") +
  guides(fill = guide_legend(title="KNO3 (mM)"), color = guide_legend(title="NaCl (mM)"))+
  # theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  scale_fill_hue(l=40, c=30)

  pdf(file="rapfix+nod14.pdf", width=10)
  print(p5)
  dev.off()
