rm(list=ls())

donnees <- read.table("HauteursFinal.csv", header = T, sep=";") # on charge les données de hauteurs
donnees$progeniteur <- as.factor(donnees$progeniteur)
donnees$progeniteur <- droplevels(donnees$progeniteur)

#~ print(head(donnees))

#Définition du code couleur
mycolors <- c("#4d9221", "green","#c51b7d","red","red","red3",
				"#01665e","#5ab4ac","#fe9929","#d8b365","#8c510a","black","grey")
  names(mycolors) <- c("F-36","F-39","F-27","F-31","F-31-7","F-31-8","M-49","M-52","M-56","M-40","M-53","F252","MBS")

## création variable htcor
donnees$htcor <- NA

sigmaf <- NULL
sigmam <- NULL
moyennes <- NULL
sigma <- NULL
for(annee in 1998:2014){
	
	if(annee == 2011){
		next
	}
	
	selec <- donnees$année == annee
	
	
	for(lignee in as.character(levels(donnees[selec , "lig"]))){
		
		selec <- donnees$année == annee & donnees$lig==lignee
		tab <- donnees[selec,]
		tab <- droplevels(tab)
		tab$bloc <- as.factor(tab$bloc)
		
		
		test <- lm(hauteur ~ progeniteur+bloc,data=tab)
		
		prog <- levels(tab$progeniteur)
		
		
		
		### données corrigées pour l'effet bloc
		bloc_effect <- c(test$coef[1] ,test$coef[1] + test$coef[grep("bloc",names(test$coef))])
		bloc_effect <- bloc_effect - mean(bloc_effect)
        names(bloc_effect) <- levels(tab$bloc)		
        
		trans <- tab$hauteur - bloc_effect[as.character(tab$bloc)]
        ### On réduit les données
        trans <- trans/summary(test)$sigma
        
			
			if(lignee == "F252"){
				sigmaf <- c(sigmaf , summary(test)$sigma)
			}else{
				sigmam <- c(sigmam , summary(test)$sigma)
			}

		donnees$htcor[selec] <- trans	
     }
     
     print(annee)
        
}

donnees$AN <- as.factor(donnees$année)
mylm <- lm(htcor~AN*lig,data=donnees,subset=donnees$pop=="TEMOIN")
anlig <- paste(donnees$AN[donnees$pop=="TEMOIN"],donnees$lig[donnees$pop=="TEMOIN"],sep=":")
lig <- donnees$lig[donnees$pop=="TEMOIN"]
year_effect <- tapply(mylm$fit,anlig,mean)
mlig <- tapply(mylm$fit,lig,mean)
mm <- year_effect*0
for (l in names(mlig)){
	mm[grep(l,names(mm))] <- mlig[l]
}
year_effect <- year_effect - mm

#plot(sigmam, type="b", col="red")
#points(sigmaf, type="b", col="blue")

### correction pour effet année
anlig <- paste(donnees$AN,donnees$lig,sep=":")
donnees$htcor2 <- donnees$htcor - year_effect[anlig]


#### on remet en unité cm
#recherche unité ecart-type résiduel type
sigmaref <- median(c(sigmam,sigmaf))
donnees$HT <- donnees$htcor2*sigmaref




write.table(donnees, file = "CorrectedDatas.csv", na="",row.names=F,col.names=T, sep=";") # on écrit toutes ces données dans un fichier



Z <- aggregate(donnees$HT,by=list(donnees$AN,donnees$lig,donnees$pop),mean)
colnames(Z) <- c("an","lig","pop","ht")
#plot(Z[,"an"],Z[,"ht"])


# par(mar=c(18,4,4,2))
# boxplot(Z[,"ht"]~Z[,"an"]+Z[,"lig"]+Z[,"pop"],las=3)
 
# par(mfrow=c(2,3),mar=c(5,4,4,2))


 for (l in levels(donnees$lig)){
	
	for (p in levels(donnees$pop)){
		pdf(paste("plot_haut_pop_",l ,"_",p,".pdf", sep=""), onefile=T) # on sort les graphique dans un pdf
		tab <- droplevels(donnees[donnees$lig==l & donnees$pop==p,])
		boxplot(HT~AN,data=tab,las=3, xlab="Années", ylab="Hauteur en cm")
		#regression
		a <- lm(HT~année,data=tab)$coef[1]
		b <- lm(HT~année,data=tab)$coef[2]
		pval <- anova(lm(HT~année,data=tab))[1,5]
		x <- unique(tab$année)
		y <- a+b*x
		points(y,type="l",col=2)
		legend("top",paste(round(b,2),"(",round(pval,3),")"),bty="n")
		
		title(paste(l,p,sep=":"))
		dev.off()
	}
 }	
 
