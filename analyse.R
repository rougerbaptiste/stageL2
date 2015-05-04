rm(list=ls()) # supprime tous les éléments stockés dans R

donnees1 <- read.table("HauteursFinal.csv", header = T, sep=";") # on charge les données de hauteurs
donnees2 <- data.frame(donnees1)


for(annee in 1998:2014){
	
	if(annee == 2011){ next}
	donnees <- donnees2[donnees2[,1] == annee,]
	
	test <- aov(donnees$hauteur ~ as.factor(donnees$famille) + as.factor(donnees$bloc) + as.factor(donnees$famille) * as.factor(donnees$plante_num),donnees)
	result <- TukeyHSD(test)
	print(annee)
	print(result)
}
