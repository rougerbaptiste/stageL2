rm(list=ls()) # supprime tous les éléments stockés dans R

donnees1 <- read.table("HauteursFinal.csv", header = T, sep=";") # on charge les données de hauteurs
donnees2 <- data.frame(donnees1)

#Définition du code couleur
 mycolors <- c("#4d9221", "green","#c51b7d","red","red","red3",
				"#01665e","#5ab4ac","#fe9929","#d8b365","#8c510a","black","grey")
  names(mycolors) <- c("F-36","F-39","F-27","F-31","F-31-7","F-31-8","M-49","M-52","M-56","M-40","M-53","F252","MBS")


pdf("resids.pdf")
tabH <- NULL
tabM <- NULL
for(annee in 1998:2014){
	
	if(annee == 2011){ next}
	
	for(lignee in c("F252","MBS")){
		donnees <- donnees2[donnees2[,1] == annee & donnees2[,3]==lignee,]
		
		donnees$bloc <- as.factor(paste(as.character(donnees$lig),donnees$bloc,sep=":"))
		donnees$famille <- as.factor(as.character(donnees$famille))
		
		test <- lm(hauteur ~ famille + bloc ,donnees)


		#vérifier les résidus de l'anova
		plot(test$fit , test$resid , pch=as.numeric(donnees$bloc) , col=mycolors[as.character(donnees$famille)])
		title(paste(annee,lignee, sep=" - "))
		
		#calcul des moyennes et intervalle de confiance
		fams <- levels(donnees$famille)
		L <- matrix(0,nrow=length(fams),ncol=length(test$coef))
		rownames(L) <- fams
		L[,1:length(fams)] <- diag(length(fams))
		L[,1] <- 1
		
		fmeans <- L%*%test$coef
		
		V <- vcov(test)
		smeans <- sqrt(diag(L%*%V%*%t(L)))
		t <- qt(0.975,df=anova(test)[3,1])
		lb <- fmeans - t*smeans
		ub <- fmeans+t*smeans
		tabH <- rbind.data.frame(tabH,cbind.data.frame(annee=annee,fam=rownames(fmeans),lb=lb,ub=ub))

		
		plot(fmeans , ylim=c(min(lb),max(ub)) , col=mycolors[rownames(fmeans)])
		points(lb)
		points(ub)
		legend(
			"topleft",
			rownames(fmeans),
			col=mycolors[rownames(fmeans)],
			lwd=c(1,1,1,1,1,1,1,1,1,1,1),
			lty=c(1,1,1,1,1,1,2,2,2,2,2),
)


	
	}

	
}
dev.off()
