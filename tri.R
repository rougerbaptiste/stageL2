rm(list=ls())

annees <- 1998:2014

tailleDebut <- c(7 , 7 , 5, 5, 5, 0 , 5 ,5 , 2 , 2 , 5 , 5, 5 , 0 , 2 , 5 , 5 )
tailleHauteur <- c(7 , 10 , 12, 15 , 15, 0, 12,15 , 15 , 15, 15, 16,16,0,17,10,5)

pedigree <- read.csv2("pedigree.csv")

tab2 <- vector()

for(annee in annees){
	
	if(annee == 2011){
		next
	}
	
	if(annee < 2000){
		anChar <- as.character(annee)
		an <- unlist(strsplit(anChar , "19"))
	}else{	
		anChar <- as.character(annee)
		an <- unlist(strsplit(anChar , "20"))
	}
	
	fichier <- paste("./hauteurs/" , annee , "/hauteur-", an[2] , ".csv" , sep="")
	
	tab <- read.table(fichier, sep=";" , header=T)

	generation <- paste("G", 1+which(annees == annee) , sep="")
	
	if(annee == 2003){
		longueur <- "prop"
	}else{
		longueur <- "lig"
	}

	for (ligne in 1:length(tab[,longueur])){
		for(hauteur in 1:tailleHauteur[which(annees == annee)]){
			spec <- vector()
			
			if(annee == 2003){
				
				cellule <- as.character(tab[ligne, "prop"])
				vecteur <- unlist(strsplit(cellule, " "))
				lignee <- vecteur[1]
				progeniteur <- vecteur[4]
				
				popbloc <- ".pl."
				
				vecteur2 <- unlist(strsplit(vecteur[2], popbloc , fixed=T))
				
				if(vecteur2[1] == "Tard"){
					population <- "TARDIF"
				}else if(vecteur2[1] == "Prec"){
					population <- "PRECOCE"
				}else if(vecteur2[1] == "Tem"){
					population <- "TEMOIN"
				}
				
				bloc <- vecteur2[2]
				
				
				vecteur3 <- unlist(strsplit(vecteur[3], "li."))
				
				ligne2 <- vecteur3[2]
				num <- hauteur
				
				if(progeniteur != "F252" && progeniteur != "MBS" ){
					parent <- as.character(pedigree[ pedigree[,"progeniteur"] == progeniteur , "parent"])
					if(length(parent) == 0){
						parent <- NA
					}
				}else{
					parent <- progeniteur
				}
				
				height <- as.numeric(as.character(tab[ligne , hauteur + 5]))
				
				if(progeniteur == as.character("F252") || progeniteur == as.character("MBS") ){
					famille <- progeniteur
				}else{
					fam <- unlist(strsplit(progeniteur, "-"))
					if((fam[1] == "F" && fam[2] == "31") & ( as.numeric(annee) >= 2001) ){
						
						if(progeniteur =="F-31-2-2-8-8-4-17"){
							famille <- "F-31-8"
						}else if(progeniteur =="F-31-2-2-8-8-4-16"){
							famille <- "F-31-7"
						}else if(fam[4] == "16"){
							famille <- "F-31-7"
						}else if(fam[4] == "17"){
							famille <- "F-31-8"
						}else{
							famille <- paste(fam[1],fam[2],fam[3], sep="-")
						}
					}else{
						famille <- paste(fam[1],fam[2], sep="-")
					}
				}
				
				spec <- c("2003", generation , lignee, as.character(parent) , progeniteur, population, famille, bloc, ligne2 , num , as.numeric(height))
						
			}else{
				lignee <- as.character(tab[ligne,"lig"])
				progeniteur <- as.character(tab[ligne,"progeniteur"])
				
				if(is.numeric( tab[ligne,"ligne"] ) == T){
					ligne2 <- as.factor(tab[ligne,"ligne"])
				}else{
					ligne2 <- NA
				}

				population <- as.character(tab[ligne,"pop"])
				
				if(is.numeric(tab[ligne,"bloc"]) == T){
					bloc <- as.factor(tab[ligne,"bloc"])
				}else{
					bloc <- NA
				}
				
				num <- hauteur
				if(annee == 1998){ ligne2 <- NA }
				
				if(progeniteur != "F252" && progeniteur != "MBS" ){
					parent <- as.character(pedigree[ pedigree[,"progeniteur"] == progeniteur , "parent"])
					if(length(parent) == 0){
						parent <- NA
					}
				}else{
					parent <- progeniteur
				}
				
				height <- as.numeric(as.character(tab[ligne , hauteur + tailleDebut[which(annees == annee)]]))
				
				if(progeniteur == as.character("F252") || progeniteur == as.character("MBS") ){
					famille <- progeniteur
				}else{
					fam <- unlist(strsplit(progeniteur, "-"))
					if((fam[1] == "F" && fam[2] == "31") & ( as.numeric(annee) > 2001) ){
						
						if(progeniteur =="F-31-2-2-8-8-4-17"){
							famille <- "F-31-8"
						}else if(progeniteur =="F-31-2-2-8-8-4-16"){
							famille <- "F-31-7"
						}else if(progeniteur =="F-31-3-5-2-7-5-4"){
							famille <- "F-31-7"
						}else if(fam[4] == "16"){
							famille <- "F-31-7"
						}else if(fam[4] == "17"){
							famille <- "F-31-8"
						}else{
							famille <- paste(fam[1],fam[2],fam[3], sep="-")
						}
					}else{
						famille <- paste(fam[1],fam[2], sep="-")
					}
				}
				spec <- c(annee ,generation , lignee, as.character(parent) , progeniteur, population, famille, bloc, ligne2 , as.character(num) , height)
			}



			tab2 <- rbind(tab2 , spec)

		}
	}
	colnames(tab2) <- c("année", "génération" , "lig" , "parent", "progeniteur" , "pop" , "famille", "bloc" , "ligne" , "plante_num", "hauteur")
	tab3 <- data.frame(tab2)
	tab3$hauteur <- as.numeric(as.character(tab3$hauteur))

	keeplist <- !is.na(tab3$hauteur)
	tab4 <- tab3[keeplist,]
	colnames(tab4) <- c("année", "génération" , "lig" , "parent", "progeniteur" , "pop" , "famille", "bloc" , "ligne" , "plante_num", "hauteur")
	fichier <- paste("hauteur-",an[2],".csv",sep="")
	write.table(tab4, file = fichier, na="",row.names=F,col.names=T, sep=";")
	
	
	print(c(  ( (which(annees == annee))/(which(annees == 2014)) )*100 , "%")  )
	
}
