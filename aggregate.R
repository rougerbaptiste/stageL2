rm(list=ls()) # on supprime tous les éléments de R

# on charge tous les fichiers de données de hauteur par année
donnees1 <- read.table("hauteur-98.csv", header = T, sep=";")
donnees2 <- read.table("hauteur-99.csv", header = T, sep=";")
donnees3 <- read.table("hauteur-00.csv", header = T, sep=";")
donnees4 <- read.table("hauteur-01.csv", header = T, sep=";")
donnees5 <- read.table("hauteur-02.csv", header = T, sep=";")
donnees6 <- read.table("hauteur-03.csv", header = T, sep=";")
donnees7 <- read.table("hauteur-04.csv", header = T, sep=";")
donnees8 <- read.table("hauteur-05.csv", header = T, sep=";")
donnees9 <- read.table("hauteur-06.csv", header = T, sep=";")
donnees10 <- read.table("hauteur-07.csv", header = T, sep=";")
donnees11 <- read.table("hauteur-08.csv", header = T, sep=";")
donnees12 <- read.table("hauteur-09.csv", header = T, sep=";")
donnees13 <- read.table("hauteur-10.csv", header = T, sep=";")
# pas de données pour 2011
donnees14 <- read.table("hauteur-12.csv", header = T, sep=";")
donnees15 <- read.table("hauteur-13.csv", header = T, sep=";")
donnees16 <- read.table("hauteur-14.csv", header = T, sep=";")

datas <- rbind(donnees1, donnees2, donnees3, donnees4, donnees5, donnees6, donnees7, donnees8, donnees9, donnees10, donnees11, donnees12, donnees13, donnees14, donnees15, donnees16) # on rassemble toutes les données dans le même tableau

write.table(datas, file = "HauteursFinal.csv", na="",row.names=F,col.names=T, sep=";") # on écrit les données dans un fichier
