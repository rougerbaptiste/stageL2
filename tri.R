rm(list=ls()) # supprime tous les éléments stockés dans R

annees <- 1998:2014 # création de la liste des années

tailleDebut <- c(7 , 7 , 5, 5, 5, 0 , 5 ,5 , 2 , 2 , 5 , 5, 5 , 0 , 2 , 5 , 5 ) # vecteur qui contient le nombre de cases à ne pas prendre en compte au début des tableaux sources
tailleHauteur <- c(7 , 10 , 12, 15 , 15, 0, 12,15 , 15 , 15, 15, 16,16,0,17,10,5) # vecteur qui contient le nombre de valeurs de hauteur à traiter dans les tableaux sources

pedigree <- read.csv2("pedigree.csv")

tab2 <- vector() # on crée un vecteur vide (futur tableau) pour contenir les données de chaque plante

for(annee in annees){
	
	if(annee == 2011){ # pas de données pour 2011, il faut sauter cette année
		next
	}
	
	if(annee < 2000){ # petit traitement de texte pour aller chercher le bon fichier de hauteur
		anChar <- as.character(annee)
		an <- unlist(strsplit(anChar , "19"))
	}else{	
		anChar <- as.character(annee)
		an <- unlist(strsplit(anChar , "20"))
	}
	
	fichier <- paste("./hauteurs/" , annee , "/hauteur-", an[2] , ".csv" , sep="") # fichier contient le chemin d'accès au fichier source de hauteur
	
	tab <- read.table(fichier, sep=";" , header=T)

	generation <- paste("G", 1+which(annees == annee) , sep="") # on crée le nom de la génération de l'année
	
	if(annee == 2003){ # pour 2003, les données sont formatées différement dans le fichier source.
		longueur <- "prop"
	}else{
		longueur <- "lig"
	}

	for (ligne in 1:length(tab[,longueur])){ # on parcours chaque ligne du tableau de données entrant
		for(hauteur in 1:tailleHauteur[which(annees == annee)]){ # on parcours chaque donnée de hauteur de la ligne
			spec <- vector() # spec est un vecteur qui va contenir les données concernant la plante traitée
			
			if(annee == 2003){ # 2003 possède une structure de données particulière, il faut y appliquer un traitement particulier
				
				cellule <- as.character(tab[ligne, "prop"]) # cellule contient les informations de population, lignée, bloc et ligne de la plante
				vecteur <- unlist(strsplit(cellule, " ")) #on sépare les informations contenues dans cellule
				lignee <- vecteur[1]
				progeniteur <- vecteur[4]
				
				popbloc <- ".pl."
				
				vecteur2 <- unlist(strsplit(vecteur[2], popbloc , fixed=T))
				
				if(vecteur2[1] == "Tard"){ # on récupère la population pour la mettre au format standart
					population <- "TARDIF"
				}else if(vecteur2[1] == "Prec"){
					population <- "PRECOCE"
				}else if(vecteur2[1] == "Tem"){
					population <- "TEMOIN"
				}
				
				bloc <- vecteur2[2]
				
				
				vecteur3 <- unlist(strsplit(vecteur[3], "li."))
				
				ligne2 <- vecteur3[2] # ligne2 représente la ligne de la plante dans le champ (ligne étant utilisée dans la boucle)
				num <- hauteur
				
				if(progeniteur != "F252" && progeniteur != "MBS" ){ # structure de contrôle pour éviter les erreurs en cas de progéniteur absent ou non trouvé
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
					if((fam[1] == "F" && fam[2] == "31") & ( as.numeric(annee) >= 2001) ){ # après 2001, la famille F-31 est renommée, cette structure de contrôle a pour but de renommer correctement les familles de ces individus
						
						if(progeniteur =="F-31-2-2-8-8-4-17"){
							famille <- "F-31-8"
						}else if(progeniteur =="F-31-2-2-8-8-4-16"){
							famille <- "F-31-7"
						}else if(fam[3] == "5"){
							famille <- "F-31"
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
				
				spec <- c("2003", generation , lignee, as.character(parent) , progeniteur, population, famille, bloc, ligne2 , num , as.numeric(height)) # on stocke dans le vecteur spec toutes les informations : année , génération, lignée, parent, progéniteur, population, famille, bloc, ligne, numéro de la plante mesurée dans la ligne, hauteur
						
			}else{ # si l'année n'est pas 2003
				lignee <- as.character(tab[ligne,"lig"])
				progeniteur <- as.character(tab[ligne,"progeniteur"])
				
				if(is.numeric( tab[ligne,"ligne"] ) == T){ # certaines années n'ont pas les informations de la ligne, on traite les données en conséquence pour que la longueur de spec soit toujours la même
					ligne2 <- as.factor(tab[ligne,"ligne"])
				}else{
					ligne2 <- NA
				}
				
				

				population <- as.character(tab[ligne,"pop"])
				
				if(is.numeric(tab[ligne,"bloc"]) == T){ # même chose que pour la ligne
					bloc <- tab[ligne,"bloc"]
				}else{
					bloc <- NA
				}

				num <- hauteur
				if(annee == 1998){ ligne2 <- NA } # il me semble redondant avec le traitement de la ligne au dessus
				
				if(progeniteur != "F252" && progeniteur != "MBS" ){ # on traite le cas où le parent ne serait paas trouvé grâce aux fichiers de pedigree en mettant un NA à la place en cas d'absence
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
					if((fam[1] == "F" && fam[2] == "31") & ( as.numeric(annee) > 2001) ){ # même travail pour les progéniteurs que commenté pour 2003
						
						if(progeniteur =="F-31-2-2-8-8-4-17"){
							famille <- "F-31-8"
						}else if(progeniteur =="F-31-2-2-8-8-4-16"){
							famille <- "F-31-7"
						}else if(annee==2002){
							famille <- "F-31"
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



			tab2 <- rbind(tab2 , spec) # on rajoute à la suite du tableau les caractéristiques de la plante traitée

		}
	}
	colnames(tab2) <- c("année", "génération" , "lig" , "parent", "progeniteur" , "pop" , "famille", "bloc" , "ligne" , "plante_num", "hauteur")
	tab3 <- data.frame(tab2) # on transforme tab2 en dataframe
	tab3$hauteur <- as.numeric(as.character(tab3$hauteur)) # on transforme les données de hauteur en données numériques

	keeplist <- !is.na(tab3$hauteur) # on crée une liste des valeurs de hauteurs qui existent
	tab4 <- tab3[keeplist,] # on supprime toutes les données de hauteur qui sont des NA
	colnames(tab4) <- c("année", "génération" , "lig" , "parent", "progeniteur" , "pop" , "famille", "bloc" , "ligne" , "plante_num", "hauteur")
	fichier <- paste("hauteur-",an[2],".csv",sep="") # fichier prend maintenant le nom du fichier de sortie contenant toutes les données traitées des plantes de l'année
	write.table(tab4, file = fichier, na="",row.names=F,col.names=T, sep=";") # on écrit toutes ces données dans un fichier
	
	
	print(c(  ( (which(annees == annee))/(which(annees == 2014)) )*100 , "%")  ) # affiche dans la console le pourcentage d'exécution du code, /!\ pas nécessaire
	
}
