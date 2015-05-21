rm(list=ls())


#Définition du code couleur
mycolors <- c("#4d9221", "green","#c51b7d","red","red","red3","black",
		"#01665e","#5ab4ac","#fe9929","#d8b365","#8c510a","grey","blue" , "black" , "red","black")
names(mycolors) <- c("F-36","F-39","F-27","F-31","F-31-7","F-31-8","F252","M-49","M-52","M-56","M-40","M-53","MBS", "PRECOCE" , "TEMOIN" , "TARDIF","Global" )

#~ 
#~ mycolors2 <- c("blue" , "black" , "red")
#~ names(mycolors2) <- c( "PRECOCE" , "TEMOIN" , "TARDIF" )


donnees <- read.table("hau_flo_1314.csv", header = T, sep=";") # on charge les données de hauteurs


pdf("plot_haut-flo_ind.pdf", onefile=T) # on sort les graphique dans un pdf

for(year in c(2013,2014)){
	for(lignee in c("F252","MBS")){
		plot(hau ~ ST,
			col=mycolors[as.character(donnees$fam)],
			xlab="Date de floraison",
			ylab="Hauteur en cm",
			data=donnees,
			subset=an==year & lig == lignee


		)
		title("Représentation de la hauteur en fonction \n de la date de floraison par individus") 
		legend("topleft",
			names(mycolors[1:7]),
			col=mycolors[1:7],
		)
	}
}
dev.off()

pdf("plot_cor.pdf", onefile=T) # on sort les graphique dans un pdf
for(year in c(2013,2014)){

	for(lignee in c("F252","MBS")){
		
		pdf(paste(lignee,year,".pdf",sep=""), onefile=T) # on sort les graphique dans un pdf

		
		forme <- NULL
		
		v <- as.data.frame(matrix(0,nrow = length(levels(droplevels(donnees$fam[donnees$lig==lignee])))+4, ncol = 4))
		colnames(v) <- c("annee" , "famille", "estimate", "pval")
		v$famille <- ""
		count <- 1
		
		familles <- levels(droplevels(donnees$fam[donnees$an == year & donnees$lig == lignee]))
		for(famille in familles){
			
			if(famille == "F252" | famille == "MBS"){next}
			
			test <- cor.test(donnees$hau[donnees$an == year & donnees$fam == famille & donnees$lig == lignee], donnees$ST[donnees$an == year & donnees$fam == famille & donnees$lig == lignee])
			
			print(famille)
			
			v[count,c("annee", "estimate", "pval")] <- c(year, test$estimate, test$p.value)
			v[count,"famille"] <- famille
			
			forme <- c(forme,0)
			
			longueur <- length(forme)
			
			count <- count +1
			
		}
		
		for(population in c("PRECOCE","TEMOIN","TARDIF")){
			
			test <- cor.test(donnees$hau[donnees$an == year & donnees$pop == population & donnees$lig == lignee], donnees$ST[donnees$an == year & donnees$pop == population & donnees$lig == lignee])
			
			v[count,c("annee", "estimate", "pval")] <- c(year, test$estimate, test$p.value)
			v[count,"famille"] <- population
			
			forme <- c(forme,3)
			
			count <- count +1
			
		}
		test <- cor.test(donnees$hau[donnees$an == year & donnees$lig == lignee], donnees$ST[donnees$an == year & donnees$lig == lignee])
		v[count,c("annee", "estimate", "pval")] <- c(year, test$estimate, test$p.value)
		v[count,"famille"] <- "Global"
		
		
		
		forme <- c(forme , 2)
		
		forme[v$pval < 0.05] <- forme[v$pval < 0.05]+15
		
		
		plot(v$estimate,
			col=mycolors[as.character(v$famille)],
			pch = forme,
			ylim=c(-0.5,0.8),
			xlim=c(0,10),
			xlab = "",
			ylab="Coefficient de corrélation"
			
			
			
		)
		text(1:length(v$estimate),as.numeric(v$estimate), v$famille,pos=3)
		title(paste(year,lignee,sep=":"))
		position <- "topleft"
		#if(year==2014 & lignee == "F252"){ position <- "bottomleft"}
		legend(position,
			c(v$famille[-length(v$famille)]),
			col=mycolors[v$famille],
			pch = c(rep(15,longueur),rep(18,3),17)	
		)
		
		dev.off()
	}
}

